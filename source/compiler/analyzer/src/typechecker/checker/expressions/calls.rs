use super::*;

/// Typechecks a call expression.
pub fn typecheck_call_expression(
    callexp: &mut TypedCallExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    let caller = typecheck_expression(&mut callexp.caller, checker_ctx, symbollib);
    let caller_span = checker_ctx.span_of_expr(&callexp.caller, &symbollib);
    let caller = extract_call_of(caller, symbollib, checker_ctx, caller_span);
    // Model instantiations are handled differently.
    if let EvaluatedType::Model(model) = caller {
        return instantiate_model(symbollib, model, checker_ctx, callexp);
    }

    if let EvaluatedType::EnumInstance { .. } = caller {
        return EvaluatedType::Unknown; // todo.
    }

    if caller.is_unknown() {
        callexp.arguments.iter_mut().for_each(|arg| {
            typecheck_expression(arg, checker_ctx, symbollib);
        }); // for continuity.
        return caller;
    }
    // Extract parameters, generic arguments and return_type from caller.
    let (is_async, parameter_types, mut generic_arguments, mut return_type) = {
        // Rendered as a function expression instance so that generic function
        // parameters can be coerced to the solutions found by owner models of methods.
        let function_type = distill_as_function_type(&caller, symbollib).unwrap();
        let final_caller_type = EvaluatedType::FunctionExpressionInstance {
            is_async: function_type.is_async,
            is_invariant: true,
            params: function_type.parameter_types,
            return_type: Box::new(function_type.return_type),
            generic_args: function_type.generic_arguments.to_vec(),
        };
        let final_caller_type = coerce(
            final_caller_type,
            &(match caller {
                EvaluatedType::FunctionExpressionInstance {
                    generic_args: generic_arguments,
                    ..
                }
                | EvaluatedType::FunctionInstance {
                    generic_arguments, ..
                }
                | EvaluatedType::MethodInstance {
                    generic_arguments, ..
                } => generic_arguments,
                _ => unreachable!(),
            }),
        );
        match final_caller_type {
            EvaluatedType::FunctionExpressionInstance {
                is_async,
                params,
                return_type,
                generic_args,
                ..
            } => (is_async, params, generic_args, *return_type),
            _ => unreachable!(),
        }
    };
    // Try to preemptively guess the type of the first argument.
    if let Some(argument) = callexp.arguments.get_mut(0) {
        if let Some(parameter_type) = parameter_types.get(0) {
            infer_ahead(argument, &parameter_type.inferred_type, symbollib);
        }
    }
    // Account for async functions.
    if is_async {
        return_type = prospectify(return_type, symbollib);
    }
    zip_arguments(
        parameter_types,
        checker_ctx,
        callexp,
        symbollib,
        &mut generic_arguments,
    );
    return_type = coerce(return_type, &generic_arguments);
    // The operations should produce a list of arguments that contain solutions
    // for generic types. If at the end there is still no solution, then the specified
    // default type is considered.
    let mut remaining_unsolved = vec![];
    return_type.traverse(&mut |child| {
        if let EvaluatedType::Generic { base } = child {
            if let Some(SemanticSymbolKind::GenericParameter {
                default_value: Some(default),
                ..
            }) = symbollib.get(*base).map(|symbol| &symbol.kind)
            {
                let default_type_evaluated =
                    evaluate(default, symbollib, Some(&generic_arguments), &mut None, 0);
                remaining_unsolved.push((*base, default_type_evaluated));
            }
        };
    });
    callexp.inferred_type = coerce(return_type, &remaining_unsolved);
    update_expression_type(
        &mut callexp.caller,
        symbollib,
        checker_ctx.literals,
        &generic_arguments,
        Some(&callexp.inferred_type),
    );
    callexp.inferred_type.clone()
}

fn instantiate_model(
    symbollib: &mut SymbolLibrary,
    model: SymbolIndex,
    checker_ctx: &mut TypecheckerContext<'_>,
    callexp: &mut TypedCallExpr,
) -> EvaluatedType {
    let model_symbol = symbollib.get_forwarded(model).unwrap();
    let (mut generic_arguments, generic_params, parameter_types) =
        if let SemanticSymbolKind::Model {
            generic_params,
            is_constructable,
            constructor_parameters,
            ..
        } = &model_symbol.kind
        {
            let name = model_symbol.name.clone();
            let span = callexp.span;
            // if model does not have a new() function.
            if !*is_constructable {
                checker_ctx.add_error(errors::model_not_constructable(name, span));
                return EvaluatedType::Unknown;
            }
            let generic_arguments = evaluate_generic_params(generic_params, false);
            let parameter_types = convert_param_list_to_type(
                constructor_parameters.as_ref().unwrap_or(&vec![]),
                symbollib,
                &generic_arguments,
                checker_ctx,
            );
            (generic_arguments, generic_params.clone(), parameter_types)
        } else {
            unreachable!()
        };
    zip_arguments(
        parameter_types,
        checker_ctx,
        callexp,
        symbollib,
        &mut generic_arguments,
    );
    let result_model_instance = EvaluatedType::ModelInstance {
        model,
        // ignore irrelevant generic transforms.
        generic_arguments: generic_arguments
            .into_iter()
            .filter(|argument| generic_params.iter().any(|base| *base == argument.0))
            .collect(),
        is_invariant: false,
    };
    return result_model_instance;
}

/// This function unifies a list of function parameters with a list of call arguments
/// And updates a generic argument with inference results.
pub fn zip_arguments(
    parameter_types: Vec<ParameterType>,
    checker_ctx: &mut TypecheckerContext,
    callexp: &mut TypedCallExpr,
    symbollib: &mut SymbolLibrary,
    generic_arguments: &mut Vec<(SymbolIndex, EvaluatedType)>,
) {
    // mismatched arguments. It checks if the parameter list is longer, so it can account for optional parameters.
    if parameter_types.len() < callexp.arguments.len() {
        checker_ctx.add_error(errors::mismatched_function_args(
            callexp.span,
            parameter_types.len(),
            callexp.arguments.len(),
            None,
        ));
        return;
    }
    let mut generic_map = HashMap::new();
    let mut i = 0;
    let caller_type = symbollib
        .get_expression_type(&callexp.caller, checker_ctx.literals)
        .unwrap_or(EvaluatedType::Unknown);
    while i < parameter_types.len() {
        // HACK: Generics in call expressions are hard by default, but
        // they can be transformed into regular, coercible generics, depending
        // on whether the caller is a regular instance, a parameter type,
        // or a shadow instance (this). Parameter types inherit the invariance
        // of the function in which they are defined, and shadow instances
        // inherit the invariance of their parent model.
        // Everything else is free real estate.
        let mut parameter_type = parameter_types[i].inferred_type.clone();
        let caller_is_invariant = caller_type.is_invariant();
        let mut unification_option = choose_unification_option(
            caller_is_invariant,
            &parameter_type,
            &caller_type,
            symbollib,
        );

        // Account for optional types.
        let is_optional = parameter_types[i].is_optional;
        let argument_type = callexp
            .arguments
            .get_mut(i)
            .map(|expression| typecheck_expression(expression, checker_ctx, symbollib));
        let argument_type = match argument_type {
            Some(evaled_typ) => evaled_typ,
            None => {
                if !is_optional {
                    checker_ctx.add_error(errors::mismatched_function_args(
                        callexp.span,
                        parameter_types.len(),
                        callexp.arguments.len(),
                        parameter_types.iter().position(|param| param.is_optional),
                    ))
                };
                break;
            }
        };
        // --- hmm.
        let is_hard_generic =
            |child: &EvaluatedType| matches!(child, EvaluatedType::HardGeneric { .. });
        if is_hard_generic(&argument_type) {
            if !(parameter_type.is_generic()
                || parameter_types[i]
                    .type_label
                    .as_ref()
                    .is_some_and(|declared_type| {
                        matches!(declared_type, IntermediateType::This { .. })
                    }))
            {
                unification_option = UnifyOptions::Conform;
            }
        }
        // ---
        if is_optional {
            parameter_type = maybify(parameter_type, symbollib);
        }
        let unification = unify_types(
            &parameter_type,
            &argument_type,
            symbollib,
            unification_option,
            Some(&mut generic_map),
        );
        if let Err(errortype) = unification {
            for errortype in errortype {
                checker_ctx.add_error(TypeError {
                    _type: errortype,
                    span: checker_ctx.span_of_expr(&callexp.arguments[i], &symbollib),
                })
            }
        }
        // Solve generics with new evaluated types.
        for (generic, assigned_type) in generic_map.iter() {
            if let Some(entry) = generic_arguments
                .iter_mut()
                .find(|prior| prior.0 == *generic)
            {
                entry.1 = assigned_type.clone();
            } else {
                generic_arguments.push((*generic, assigned_type.clone()));
            }
        }
        // Based on transformer generic values,
        // the types of future arguments can be inferred.
        if let Some((typ, expression)) = parameter_types
            .get(i + 1)
            .and_then(|param_type| Some((param_type, callexp.arguments.get_mut(i + 1)?)))
        {
            let target_type = coerce(typ.inferred_type.clone(), &generic_arguments);
            infer_ahead(expression, &target_type, symbollib)
        }
        i += 1;
    }
}

/// Based on the type of the generic and the context of a function expression,
///
/// this function decides whether conformity will be able to solve hard generics
/// or only regular ones.
fn choose_unification_option(
    caller_is_invariant: bool,
    right_type: &EvaluatedType,
    caller_type: &EvaluatedType,
    symbollib: &mut SymbolLibrary,
) -> UnifyOptions {
    if caller_is_invariant {
        return UnifyOptions::Conform;
    }
    // If a parameter is a hard generic (or contains a hard generic),
    // it needs to be in list of generics owned by the caller to be coercible.
    if right_type
        .contains_child_for_which(&|child| matches!(child, EvaluatedType::HardGeneric { .. }))
    {
        let mut param_generics = vec![];
        right_type.gather_generics_into(&mut param_generics);
        // The vector is for getting the generics from function expressions.
        #[allow(unused_assignments)]
        let mut caller_generics_if_function_expression = None;
        let caller_generics = match caller_type {
                    EvaluatedType::FunctionInstance {
                        function: caller_base,
                        ..
                    }
                    // todo: generic params from function types.
                    | EvaluatedType::MethodInstance {
                        method: caller_base,
                        ..
                    }
                    => match &symbollib.get(*caller_base).unwrap().kind {
                        SemanticSymbolKind::Method { generic_params, .. }
                        | SemanticSymbolKind::Function { generic_params, .. } => {
                            Some(generic_params)
                        }
                        _ => None,
                    },
                    | EvaluatedType::FunctionExpressionInstance { generic_args,.. } => {
                        caller_generics_if_function_expression = Some(generic_args.iter().map(|(symbolidx, _)| *symbolidx).collect::<Vec<_>>());
                        caller_generics_if_function_expression.as_ref()
                    }
                    _ => None,
                };
        if param_generics
            .iter()
            .all(|generic| caller_generics.is_some_and(|generics| generics.contains(generic)))
            || right_type.is_function_expression_instance()
        {
            return UnifyOptions::HardConform;
        } else {
            return UnifyOptions::Conform;
        }
    }
    return UnifyOptions::HardConform;
}

pub fn convert_param_list_to_type(
    params: &Vec<SymbolIndex>,
    symbollib: &SymbolLibrary,
    solved_generics: &Vec<(SymbolIndex, EvaluatedType)>,
    checker_ctx: &mut TypecheckerContext,
) -> Vec<ParameterType> {
    params
        .iter()
        .map(|param| {
            let parameter_symbol = symbollib.get(*param).unwrap();
            let (is_optional, type_label, span, inferred_type) = match &parameter_symbol.kind {
                SemanticSymbolKind::Parameter {
                    is_optional,
                    param_type,
                    inferred_type,
                    ..
                } => (
                    *is_optional,
                    param_type,
                    parameter_symbol.ident_span(),
                    inferred_type,
                ),
                _ => unreachable!("Expected parameter but got {parameter_symbol:?}"),
            };
            ParameterType {
                name: parameter_symbol.name.clone(),
                is_optional,
                type_label: type_label.clone(),
                inferred_type: if inferred_type.is_unknown() {
                    type_label
                        .as_ref()
                        .map(|typ| {
                            evaluate(
                                typ,
                                symbollib,
                                Some(solved_generics),
                                &mut checker_ctx.tracker(span),
                                0,
                            )
                        })
                        .unwrap_or(EvaluatedType::Unknown)
                } else {
                    inferred_type.clone()
                },
            }
        })
        .collect::<Vec<_>>()
}

fn extract_call_of(
    caller: EvaluatedType,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
    caller_span: Span,
) -> EvaluatedType {
    // Only valid expressions allowed in caller positions:
    // - Enumerated values with tags
    // - Instantiable models
    // - methods
    // - functions
    // - function expressions
    let caller = match caller {
        EvaluatedType::EnumInstance { .. }
        | EvaluatedType::FunctionInstance { .. }
        | EvaluatedType::FunctionExpressionInstance { .. }
        | EvaluatedType::MethodInstance { .. }
        | EvaluatedType::Model(_) => caller,

        EvaluatedType::Module(_)
        | EvaluatedType::ModelInstance { .. }
        | EvaluatedType::InterfaceInstance { .. }
        | EvaluatedType::Interface(_)
        | EvaluatedType::Enum(_)
        | EvaluatedType::Generic { .. }
        | EvaluatedType::HardGeneric { .. }
        | EvaluatedType::Void
        | EvaluatedType::Never
        | EvaluatedType::OpaqueTypeInstance { .. } => {
            checker_ctx.add_error(errors::not_callable(
                symbollib.format_evaluated_type(&caller),
                caller_span,
            ));
            EvaluatedType::Unknown
        }
        _ => EvaluatedType::Unknown,
    };
    caller
}
