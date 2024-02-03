use super::*;

/// Typechecks a for loop.
pub fn typecheck_for_loop(
    forloop: &mut crate::TypedForStatement,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) {
    let iterator_type =
        expressions::typecheck_expression(&mut forloop.iterator, checker_ctx, symbollib);
    if symbollib.iterable.is_none() || symbollib.asiter.is_none() {
        let span = checker_ctx.span_of_expr(&forloop.iterator, symbollib);
        checker_ctx.add_error(errors::missing_intrinsic("Iteration".to_owned(), span));
        typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
        return;
    }
    // if the instance implements both AsIterator and Iterable, AsIterator will be preferred.
    let asiter = symbollib.asiter.unwrap();
    let iterable = symbollib.iterable.unwrap();
    let asiter_generic = symbollib
        .get(asiter)
        .map(|symbol| match &symbol.kind {
            SemanticSymbolKind::Interface { generic_params, .. } => Some(generic_params.get(0)?),
            _ => None,
        })
        .flatten()
        .copied();
    let iterable_generic = symbollib
        .get(iterable)
        .map(|symbol| match &symbol.kind {
            SemanticSymbolKind::Interface { generic_params, .. } => Some(generic_params.get(0)?),
            _ => None,
        })
        .flatten()
        .copied();
    let implementation = get_implementation_of(asiter, &iterator_type, symbollib)
        .or_else(|| get_implementation_of(iterable, &iterator_type, symbollib));
    if implementation.is_none() {
        let illegal_type = symbollib.format_evaluated_type(&iterator_type);
        let span = checker_ctx.span_of_expr(&forloop.iterator, symbollib);
        checker_ctx.add_error(errors::illegal_iterator(illegal_type, span));
        typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
        return;
    }
    // No variables created.
    // Check the body and return.
    if forloop.items.len() == 0 {
        let body = &mut forloop.body;
        typecheck_for_loop_body(body, checker_ctx, symbollib);
    }
    let implementation = implementation.unwrap();
    // Evaluate and determine the unit type for iteration.
    let final_type = match implementation {
        EvaluatedType::InterfaceInstance {
            generic_arguments, ..
        } => {
            let generic_solution = generic_arguments.into_iter().find(|generic| {
                Some(generic.0) == asiter_generic || Some(generic.0) == iterable_generic
            });
            if generic_solution.is_none() {
                let name = symbollib.format_evaluated_type(&iterator_type);
                let span = checker_ctx.span_of_expr(&forloop.iterator, symbollib);
                checker_ctx.add_error(errors::illegal_iterator(name, span));
                EvaluatedType::Unknown
            } else {
                let evaluated_type = generic_solution.unwrap().1;
                let full_generic_list = match iterator_type {
                    EvaluatedType::InterfaceInstance {
                        generic_arguments, ..
                    }
                    | EvaluatedType::ModelInstance {
                        generic_arguments, ..
                    } => generic_arguments,
                    _ => vec![],
                };
                coerce(evaluated_type, &full_generic_list)
            }
        }
        _ => EvaluatedType::Unknown,
    };
    // Pattern resolutions.
    for name in &forloop.items {
        let symbol = symbollib.get_mut(*name).unwrap();
        let pattern_type = if let SemanticSymbolKind::LoopVariable {
            pattern_type,
            inferred_type,
            ..
        } = &mut symbol.kind
        {
            if pattern_type.is_normal() {
                // There is only one name to infer.
                *inferred_type = final_type;
                typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
                return;
            }
            pattern_type.clone()
        } else {
            continue;
        };
        let mut pattern_result = EvaluatedType::Unknown;
        match pattern_type {
            VariablePatternForm::DestructuredFromObject {
                from_property: property_symbol_idx,
            } => {
                match &final_type {
                    EvaluatedType::ModelInstance {
                        model,
                        generic_arguments,
                        is_invariant,
                    } => {
                        let property_span = symbollib
                            .get(property_symbol_idx)
                            .map(|sym| sym.ident_span())
                            .unwrap_or_default();
                        let property_type = expressions::search_for_property(
                            checker_ctx,
                            symbollib,
                            *model,
                            property_symbol_idx,
                            generic_arguments.clone(),
                            true,
                            *is_invariant,
                            property_span,
                        );
                        let get_model_name = || symbollib.get(*model).unwrap().name.clone();
                        let get_property_name =
                            || symbollib.get(property_symbol_idx).unwrap().name.clone();
                        if property_type.is_none() {
                            let property_name = get_property_name();
                            let model_name = get_model_name();
                            checker_ctx.add_error(errors::unknown_property(
                                model_name,
                                property_name,
                                property_span,
                            ));
                        } else {
                            pattern_result = property_type.unwrap();
                            if pattern_result.is_method_instance() {
                                let property_name = get_property_name();
                                let model_name = get_model_name();
                                checker_ctx.add_error(errors::destructuring_method(
                                    model_name,
                                    property_name,
                                    property_span,
                                ))
                            }
                        }
                    }
                    _ => {
                        checker_ctx.add_error(errors::illegal_model_destructure(
                            symbollib.format_evaluated_type(&final_type),
                            forloop.span,
                        ));
                        // No point in checking other patterns.
                        typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
                        return;
                    }
                }
            }
            VariablePatternForm::DestructuredFromArray => match &final_type {
                EvaluatedType::ModelInstance {
                    generic_arguments, ..
                } if is_array(&final_type, symbollib) => {
                    pattern_result = generic_arguments.first().unwrap().1.clone()
                }
                _ => {
                    checker_ctx.add_error(errors::illegal_array_destructure(
                        symbollib.format_evaluated_type(&final_type),
                        forloop.span,
                    ));
                    // No point in checking other patterns.
                    typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
                    return;
                }
            },
            _ => {}
        }
        if let SemanticSymbolKind::LoopVariable { inferred_type, .. } =
            &mut symbollib.get_mut(*name).unwrap().kind
        {
            *inferred_type = pattern_result;
        }
    }
    typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
}

fn typecheck_for_loop_body(
    body: &mut crate::TypedBlock,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) {
    push_scopetype(checker_ctx, ScopeType::Other);
    let block_type = expressions::typecheck_block(body, false, checker_ctx, symbollib);
    pop_scopetype(checker_ctx);

    // For loop blocks should not have implicit returns.
    // If partials are allowed because having to add the semicolon can get annoying after sometime.
    if !block_type.is_void()
        && !block_type.is_unknown()
        && !block_type.is_never()
        && !(matches!(
            &block_type,
            EvaluatedType::Partial { types } if types.iter().all(|unittype| unittype.is_void() || unittype.is_never())
        ))
    {
        let err_span = body
            .statements
            .last()
            .map(|stmnt| checker_ctx.span_of_stmnt(stmnt, symbollib))
            .unwrap_or(body.span);
        let type_as_string = symbollib.format_evaluated_type(&block_type);
        checker_ctx.add_error(errors::implicit_loop_return(type_as_string, err_span));
    }
}
