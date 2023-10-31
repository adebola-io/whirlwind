#![allow(unused)]

use crate::{
    EvaluatedType::*, IntermediateType, PathIndex, ProgramError, SemanticSymbolKind, SymbolIndex,
    SymbolTable, UNKNOWN, *,
};
use ast::Span;
use errors::{expected, value_as_type, TypeError, TypeErrorType};

/// Returns true if an evaluated type is a prospect.
pub fn is_prospective_type(evaluated_type: &EvaluatedType, symboltable: &SymbolTable) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if symboltable.prospect_symbol.is_some_and(|prospect| prospect == *model)
    )
}

/// Take two evaluated types and finds the most suitable type
/// that upholds the properties of both.
pub fn unify(
    left: &EvaluatedType,
    right: &EvaluatedType,
    symboltable: &SymbolTable,
) -> Result<EvaluatedType, Vec<TypeErrorType>> {
    // Types are directly equal.
    if left == right {
        return Ok(left.clone());
    }
    let default_error = || TypeErrorType::MismatchedAssignment {
        left: symboltable.format_evaluated_type(left),
        right: symboltable.format_evaluated_type(right),
    };
    match (left, right) {
        // Either type is generic.
        (Generic { base }, free_type) | (free_type, Generic { base }) => {
            let base_parameter = symboltable.get_forwarded(*base).unwrap();
            match &base_parameter.kind {
                SemanticSymbolKind::GenericParameter {
                    traits,
                    default_value,
                } => {
                    // Default generic type if other is unknown.
                    if let Some(default) = default_value {
                        if free_type.is_unknown() {
                            return Ok(evaluate(default, symboltable, None, &mut None));
                        }
                    }
                    for _trait in traits {
                        let trait_evaluated = evaluate(_trait, symboltable, None, &mut None);
                        // The trait guard does not refer to a trait.
                        // Unification cannot continue, but it is not the problem of this process.
                        if !trait_evaluated.is_trait_instance() {
                            return Ok(Unknown);
                        }
                        let implementations = match free_type {
                            ModelInstance {
                                model,
                                generic_arguments,
                            } => match &symboltable.get_forwarded(*model).unwrap().kind {
                                SemanticSymbolKind::Model {
                                    implementations, ..
                                } => Some(implementations),
                                _ => None,
                            },
                            TraitInstance {
                                trait_,
                                generic_arguments,
                            } => match &symboltable.get_forwarded(*trait_).unwrap().kind {
                                SemanticSymbolKind::Trait {
                                    implementations, ..
                                } => Some(implementations),
                                _ => None,
                            },
                            _ => None,
                        };
                        let trait_is_implemented = implementations.is_some_and(|implementations| {
                            implementations
                                .iter()
                                .find(|implementation| {
                                    // todo: block infinite types.
                                    evaluate(implementation, symboltable, None, &mut None)
                                        == trait_evaluated
                                })
                                .is_some()
                        });
                        if !trait_is_implemented {
                            return Err(vec![
                                default_error(),
                                TypeErrorType::UnimplementedTrait {
                                    offender: symboltable.format_evaluated_type(free_type),
                                    _trait: symboltable.format_evaluated_type(&trait_evaluated),
                                },
                            ]);
                        }
                    }
                    Ok(free_type.clone())
                }
                // Something has gone wrong if this ever happens. Look into it.
                _ => Ok(Unknown),
            }
        }
        // Comparing generics
        (
            ModelInstance {
                model: first,
                generic_arguments: first_gen_args,
            },
            ModelInstance {
                model: second,
                generic_arguments: second_gen_args,
            },
        ) => {
            let first_model_symbol = symboltable.get(*first).unwrap();
            let second_model_symbol = symboltable.get(*second).unwrap();
            if !std::ptr::eq(first_model_symbol, second_model_symbol) {
                return Err(vec![default_error()]);
            }
            if first_gen_args.len() != second_gen_args.len() {
                return Err(vec![
                    default_error(),
                    TypeErrorType::MismatchedGenericArgs {
                        name: first_model_symbol.name.clone(),
                        expected: first_gen_args.len(),
                        assigned: second_gen_args.len(),
                    },
                ]);
            }
            let mut final_generic_arguments =
                match unify_generic_arguments(first_gen_args, second_gen_args, symboltable) {
                    Ok(args) => args,
                    Err(mut errors) => {
                        errors.insert(0, default_error());
                        return Err(errors);
                    }
                };
            let mut i = 0;

            return Ok(ModelInstance {
                model: *first,
                generic_arguments: final_generic_arguments,
            });
        }
        // Either type is unknown.
        (Unknown, other) | (other, Unknown) => Ok(other.clone()),
        // Either type is a function.
        (
            FunctionExpressionInstance { .. } | FunctionInstance { .. } | MethodInstance { .. },
            FunctionExpressionInstance { .. } | FunctionInstance { .. } | MethodInstance { .. },
        ) => {
            let (left_is_async, left_params, left_generic_arguments, mut left_return_type) =
                distill_function_type(left, symboltable);
            let (right_is_async, right_params, right_generic_arguments, mut right_return_type) =
                distill_function_type(right, symboltable);

            /// Confirm that there are no generic mismatches between the two functions.
            let generic_args = match unify_generic_arguments(
                left_generic_arguments,
                right_generic_arguments,
                symboltable,
            ) {
                Ok(args) => args,
                Err(mut errors) => {
                    errors.insert(0, default_error());
                    return Err(errors);
                }
            };

            let right_returns_prospect = is_prospective_type(&right_return_type, symboltable);
            let left_returns_prospect = is_prospective_type(&left_return_type, symboltable);

            let is_async = match (left_is_async, right_is_async) {
                (true, true) => true,
                (false, false) => false,
                (true, false) if !right_returns_prospect => {
                    return Err(vec![
                        default_error(),
                        TypeErrorType::AsyncMismatch {
                            async_func: symboltable.format_evaluated_type(left),
                            non_async_func: symboltable.format_evaluated_type(right),
                        },
                    ])
                }
                (false, true) if !left_returns_prospect => {
                    return Err(vec![
                        default_error(),
                        TypeErrorType::AsyncMismatch {
                            async_func: symboltable.format_evaluated_type(left),
                            non_async_func: symboltable.format_evaluated_type(right),
                        },
                    ])
                }
                _ => false,
            };

            /// PARAMETERS.
            if left_params.len() < right_params.len() {
                return Err(vec![
                    default_error(),
                    TypeErrorType::MismatchedFunctionParams {
                        expected: left_params.len(),
                        found: right_params.len(),
                        least_required: None,
                    },
                ]);
            }
            let param_results = left_params.iter().zip(right_params.iter()).map(
                |(left_param, right_param)| -> Result<ParameterType, Vec<TypeErrorType>> {
                    Ok(ParameterType {
                        name: left_param.name.clone(),
                        is_optional: left_param.is_optional || right_param.is_optional, // todo.
                        type_label: None,
                        inferred_type: unify(
                            &left_param.inferred_type,
                            &right_param.inferred_type,
                            symboltable,
                        )?,
                    })
                },
            );
            let mut params = vec![];
            for result in param_results {
                params.push(match result {
                    Ok(param) => param,
                    Err(mut errors) => {
                        errors.insert(0, default_error());
                        return Err(errors);
                    }
                })
            }

            // RETURN TYPES.
            // If one function is async and the other returns a prospect, they are unifiable.
            if left_is_async {
                left_return_type = prospectify(left_return_type, symboltable);
            }
            if right_is_async {
                right_return_type = prospectify(right_return_type, symboltable);
            }
            let return_type = Box::new(unify(&left_return_type, &right_return_type, symboltable)?);

            return Ok(EvaluatedType::FunctionExpressionInstance {
                is_async,
                params,
                return_type,
                generic_args,
            });
        }
        _ => Err(vec![TypeErrorType::MismatchedAssignment {
            left: symboltable.format_evaluated_type(left),
            right: symboltable.format_evaluated_type(right),
        }]),
    }
}

/// Encloses an evaluated type as a prospect.
pub fn prospectify(typ: EvaluatedType, symboltable: &SymbolTable) -> EvaluatedType {
    if let Some(model) = symboltable.prospect_symbol {
        let prospect_symbol = symboltable.get(model).unwrap();
        let prospect_generic_parameter = match &prospect_symbol.kind {
            SemanticSymbolKind::Model { generic_params, .. } => generic_params[0],
            _ => unreachable!(),
        };
        return EvaluatedType::ModelInstance {
            model,
            generic_arguments: vec![(prospect_generic_parameter, typ)],
        };
    } else {
        return typ;
    }
}

/// Compares two lists of generic arguments to determine
/// how similar or dissimilar they are.
pub fn unify_generic_arguments(
    left_generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    right_generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    symboltable: &SymbolTable,
) -> Result<Vec<(SymbolIndex, EvaluatedType)>, Vec<TypeErrorType>> {
    let mut generic_args = vec![];
    let mut errors = vec![];
    let arguments_in_both_lists =
        left_generic_arguments
            .iter()
            .enumerate()
            .filter_map(|(left_arr_index, (ls_idx, _))| {
                right_generic_arguments
                    .iter()
                    .enumerate()
                    .find(|(_, (rs_idx, _))| rs_idx == ls_idx)
                    .map(|(right_arr_index, _)| (right_arr_index, left_arr_index))
            });
    let arguments_in_only_left = left_generic_arguments
        .iter()
        .enumerate()
        .filter(|(_, (ls_idx, _))| {
            right_generic_arguments
                .iter()
                .find(|(rs_idx, _)| rs_idx == ls_idx)
                .is_none()
        })
        .map(|(arr_idx, _)| arr_idx);
    let arguments_in_only_right = right_generic_arguments
        .iter()
        .enumerate()
        .filter(|(_, (rs_idx, _))| {
            left_generic_arguments
                .iter()
                .find(|(ls_idx, _)| ls_idx == rs_idx)
                .is_none()
        })
        .map(|(arr_idx, _)| arr_idx);
    for (left_arr_index, right_arr_index) in arguments_in_both_lists {
        let (symbol_index, left_evaluated_type) =
            left_generic_arguments.get(left_arr_index).unwrap();
        let (symbol_index, right_evaluated_type) =
            right_generic_arguments.get(right_arr_index).unwrap();

        generic_args.push((
            *symbol_index,
            match unify(left_evaluated_type, right_evaluated_type, symboltable) {
                Ok(arg) => arg,
                Err(mut suberrors) => {
                    errors.append(&mut suberrors);
                    EvaluatedType::Unknown
                }
            },
        ));
    }
    for arr_idx in arguments_in_only_left {
        generic_args.push((left_generic_arguments.get(arr_idx).unwrap()).clone());
    }
    for arr_idx in arguments_in_only_right {
        generic_args.push((right_generic_arguments.get(arr_idx).unwrap()).clone());
    }
    if errors.len() > 0 {
        return Err(errors);
    }
    Ok(generic_args)
}

/// Reduces a functional evaluated type to its components
/// # Panics
/// It panics if the evaluated type is not functional.
fn distill_function_type<'a>(
    caller: &'a EvaluatedType,
    symboltable: &SymbolTable,
) -> (
    bool,
    Vec<ParameterType>,
    &'a Vec<(SymbolIndex, EvaluatedType)>,
    EvaluatedType,
) {
    match caller {
        EvaluatedType::MethodInstance {
            method: function,
            generic_arguments,
        }
        | EvaluatedType::FunctionInstance {
            function,
            generic_arguments,
        } => {
            let function_symbol = symboltable.get(*function).unwrap();
            match &function_symbol.kind {
                SemanticSymbolKind::Method {
                    is_async,
                    params,
                    generic_params,
                    return_type,
                    ..
                }
                | SemanticSymbolKind::Function {
                    is_async,
                    params,
                    generic_params,
                    return_type,
                    ..
                } => {
                    let parameter_types = params
                        .iter()
                        .map(|param| {
                            let parameter_symbol = symboltable.get(*param).unwrap();
                            let (is_optional, type_label) = match &parameter_symbol.kind {
                                SemanticSymbolKind::Parameter {
                                    is_optional,
                                    param_type,
                                } => (*is_optional, param_type),
                                _ => {
                                    unreachable!("Expected parameter but got {parameter_symbol:?}")
                                }
                            };
                            ParameterType {
                                name: parameter_symbol.name.clone(),
                                is_optional,
                                type_label: type_label.clone(),
                                inferred_type: type_label
                                    .as_ref()
                                    .map(|typ| {
                                        evaluate(
                                            typ,
                                            symboltable,
                                            Some(generic_arguments),
                                            &mut None,
                                        )
                                    })
                                    .unwrap_or(EvaluatedType::Unknown),
                            }
                        })
                        .collect::<Vec<_>>();
                    let return_type = return_type
                        .as_ref()
                        .map(|typ| evaluate(typ, symboltable, Some(&generic_arguments), &mut None))
                        .unwrap_or(EvaluatedType::Void);
                    (*is_async, parameter_types, generic_arguments, return_type)
                }
                _ => unreachable!("Expected functional symbol but found {:?}", function_symbol),
            }
        }
        EvaluatedType::FunctionExpressionInstance {
            is_async,
            params,
            return_type,
            generic_args,
        } => (
            *is_async,
            params.clone(),
            generic_args,
            *return_type.clone(),
        ),
        _ => unreachable!("{caller:?} cannot be distilled, because it is not a functional type."),
    }
}

/// Converts an intermediate type into an evaluation.
pub fn evaluate(
    typ: &IntermediateType,
    symboltable: &SymbolTable,
    // A map of the solutions for previously encountered generic types.
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    // Error store from the standpoint, if it exists.
    mut error_tracker: &mut Option<(&mut Vec<ProgramError>, PathIndex)>,
) -> EvaluatedType {
    match typ {
        IntermediateType::FunctionType {
            params,
            return_type,
            span,
        } => {
            let return_type = Box::new(
                return_type
                    .as_ref()
                    .map(|typ| evaluate(typ, symboltable, solved_generics, error_tracker))
                    .unwrap_or(Void),
            );
            FunctionExpressionInstance {
                is_async: false, // todo: always false (for now).
                params: params
                    .iter()
                    .map(|param| {
                        let mut param = param.clone();
                        param.inferred_type = param
                            .type_label
                            .as_ref()
                            .map(|typ| {
                                evaluate(typ, symboltable, solved_generics, &mut error_tracker)
                            })
                            .unwrap_or(Unknown);
                        param
                    })
                    .collect(),
                return_type,
                generic_args: solved_generics.cloned().unwrap_or(vec![]),
            }
        }
        IntermediateType::MemberType {
            object,
            property,
            span,
        } => Unknown,
        IntermediateType::SimpleType {
            value,
            generic_args,
            span,
        } => {
            let value = symboltable.forward(*value);
            let typ = symboltable.get_forwarded(value).unwrap();
            let mut get_generics = |generic_params| {
                generate_generics_from_arguments(
                    generic_args,
                    generic_params,
                    error_tracker,
                    typ,
                    span,
                    symboltable,
                    solved_generics,
                )
            };
            match &typ.kind {
                SemanticSymbolKind::Module { .. } => Module(value),
                SemanticSymbolKind::Trait { generic_params, .. } => TraitInstance {
                    trait_: value,
                    generic_arguments: get_generics(generic_params),
                },
                SemanticSymbolKind::Model { generic_params, .. } => ModelInstance {
                    model: value,
                    // todo
                    generic_arguments: get_generics(generic_params),
                },
                SemanticSymbolKind::GenericParameter { .. } => {
                    // check if this type already has a solution.
                    if let Some(solved_generics) = solved_generics {
                        for (generic_parameter, evaluated_type) in solved_generics {
                            if *generic_parameter == value {
                                return (*evaluated_type).clone();
                            }
                        }
                    }
                    Generic { base: value }
                }
                SemanticSymbolKind::UndeclaredValue => Unknown,
                _ => {
                    add_error_if_possible(
                        &mut error_tracker,
                        value_as_type(typ.name.clone(), *span),
                    );
                    Unknown
                }
            }
        }
        IntermediateType::This { meaning, span } => {
            // Generate a type that is equal to the enclosing model or trait.
            match meaning.clone() {
                Some(value) => {
                    let symbol = symboltable.get_forwarded(value).unwrap();
                    let generic_args = match &symbol.kind {
                        SemanticSymbolKind::Model { generic_params, .. }
                        | SemanticSymbolKind::Trait { generic_params, .. } => generic_params
                            .iter()
                            .map(|idx| IntermediateType::SimpleType {
                                value: *idx,
                                generic_args: vec![],
                                span: Span::default(),
                            })
                            .collect(),
                        _ => return Unknown,
                    };
                    let intermediate_type = IntermediateType::SimpleType {
                        value,
                        generic_args,
                        span: *span,
                    };
                    evaluate(
                        &intermediate_type,
                        symboltable,
                        solved_generics,
                        error_tracker,
                    )
                }
                None => Unknown,
            }
        }
        IntermediateType::BorrowedType { value, span } => Borrowed {
            base: Box::new(evaluate(
                &value,
                symboltable,
                solved_generics,
                error_tracker,
            )),
        },
        IntermediateType::Placeholder => {
            unreachable!("Cannot evaluate placeholder intermediate type.")
        }
        _ => Unknown,
    }
}

fn generate_generics_from_arguments(
    generic_args: &Vec<IntermediateType>,
    generic_params: &Vec<SymbolIndex>,
    mut error_tracker: &mut Option<(&mut Vec<ProgramError>, PathIndex)>,
    typ: &crate::SemanticSymbol,
    span: &Span,
    symboltable: &SymbolTable,
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
) -> Vec<(SymbolIndex, EvaluatedType)> {
    if generic_args.len() > 0 && generic_params.len() == 0 {
        add_error_if_possible(
            &mut error_tracker,
            errors::unexpected_generic_args(typ.name.clone(), *span),
        );
        vec![]
    } else {
        let args_len = generic_args.len();
        let param_len = generic_params.len();
        if args_len != param_len {
            add_error_if_possible(
                &mut error_tracker,
                errors::mismatched_generics(typ.name.clone(), param_len, args_len, *span),
            );
            vec![]
        } else {
            let mut generic_solutions = vec![];
            // Unify each parameter to argument.
            let mut i = 0;
            while i < param_len {
                // Convert each parameter symbol to an immediately evaluated generic param type.
                // which can then be unified with the argument
                // to determine the result value.
                let param_idx = generic_params[i];
                let parameter_symbol = symboltable.get(param_idx).unwrap();
                let intermediate_generic_type = IntermediateType::SimpleType {
                    value: param_idx,
                    generic_args: vec![],
                    span: Span::default(),
                };
                let generic_param_evaluated = evaluate(
                    &intermediate_generic_type,
                    symboltable,
                    solved_generics,
                    &mut error_tracker,
                );
                let argument_evaluated = evaluate(
                    &generic_args[i],
                    symboltable,
                    solved_generics,
                    &mut error_tracker,
                );
                let result_evaluated_type =
                    match unify(&generic_param_evaluated, &argument_evaluated, symboltable) {
                        Ok(value) => value,
                        Err(error) => {
                            for error in error {
                                add_error_if_possible(
                                    &mut error_tracker,
                                    TypeError {
                                        _type: error,
                                        span: generic_args[i].span(),
                                    },
                                );
                            }
                            Unknown
                        }
                    };
                generic_solutions.push((param_idx, result_evaluated_type));
                i += 1;
            }
            generic_solutions
        }
    }
}

fn add_error_if_possible(
    error_tracker: &mut Option<(&mut Vec<ProgramError>, PathIndex)>,
    error: TypeError,
) {
    if let Some((errors, path_idx)) = error_tracker {
        let error = ProgramError {
            offending_file: *path_idx,
            error_type: crate::ProgramErrorType::Typing(error),
        };
        if !errors.iter().any(|prior| *prior == error) {
            errors.push(error);
        }
    }
}

/// Creates a matching between a list of generic parameters and generic arguments.
pub fn zip<'a>(
    generic_params: &[SymbolIndex],
    generic_arguments: &'a Vec<EvaluatedType>,
) -> Vec<(SymbolIndex, &'a EvaluatedType)> {
    generic_params
        .iter()
        .enumerate()
        .map(|(index, param)| (*param, generic_arguments.get(index).unwrap_or(&UNKNOWN)))
        .collect()
}
