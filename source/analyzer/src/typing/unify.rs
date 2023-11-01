use std::collections::HashMap;

use crate::{
    utils::is_prospective_type, EvaluatedType::*, SemanticSymbolKind, SymbolIndex, SymbolTable,
    UNKNOWN, *,
};
use errors::TypeErrorType;

/// Take two evaluated types and finds the most suitable type
/// that upholds the properties of both.
/// It optionally takes in a map so it can track the generic parameters
/// that have been transformed.
// TODO: Return custom error if one type is borrowed.
// TODO: Return custom error if one type is a Maybe of the other.
// TODO: Return custom error if one type is an array of the other.
pub fn unify_types(
    left: &EvaluatedType,
    right: &EvaluatedType,
    symboltable: &SymbolTable,
    mut map: Option<&mut HashMap<SymbolIndex, EvaluatedType>>,
) -> Result<EvaluatedType, Vec<TypeErrorType>> {
    let default_error = || TypeErrorType::MismatchedAssignment {
        left: symboltable.format_evaluated_type(left),
        right: symboltable.format_evaluated_type(right),
    };
    let maybify = |typ| utils::maybify(typ, symboltable);
    // Types are directly equal.
    // therefore they are unifiable.
    if left == right {
        return Ok(left.clone());
    }
    match (left, right) {
        // Left type is a generic parameter.
        (Generic { base }, right_type) => {
            // If there is already a prior solution for the left type,
            // check that it is unifiable with the right,
            // and update its mapped value.
            if let Some(map) = map.as_deref_mut() {
                if let Some(already_assigned) = map.get(base).cloned() {
                    match unify_types(&already_assigned, right_type, symboltable, Some(map)) {
                        Ok(result_type) => {
                            map.insert(*base, result_type.clone());
                            return Ok(result_type);
                        }
                        Err(mut errors) => {
                            errors.insert(0, default_error());
                            return Err(errors);
                        }
                    }
                }
            }
            let base_parameter = symboltable.get_forwarded(*base).unwrap();
            match &base_parameter.kind {
                SemanticSymbolKind::GenericParameter {
                    traits,
                    default_value,
                } => {
                    // Default generic type if other is unknown.
                    if let Some(default) = default_value {
                        if right_type.is_unknown() {
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
                        let implementations = match right_type {
                            ModelInstance { model: base, .. }
                            | TraitInstance { trait_: base, .. }
                            | Generic { base } => {
                                match &symboltable.get_forwarded(*base).unwrap().kind {
                                    SemanticSymbolKind::GenericParameter {
                                        traits: implementations,
                                        ..
                                    }
                                    | SemanticSymbolKind::Trait {
                                        implementations, ..
                                    }
                                    | SemanticSymbolKind::Model {
                                        implementations, ..
                                    } => Some(implementations),
                                    _ => None,
                                }
                            }
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
                                    offender: symboltable.format_evaluated_type(right_type),
                                    _trait: symboltable.format_evaluated_type(&trait_evaluated),
                                },
                            ]);
                        }
                    }
                    // Generic parameter solved.
                    if let Some(map) = map {
                        map.insert(*base, right_type.clone());
                    }
                    Ok(right_type.clone())
                }
                // Something has gone wrong if this ever happens. Look into it.
                _ => Ok(Unknown),
            }
        }
        // Comparing generic types.
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
            let final_generic_arguments =
                match unify_generic_arguments(first_gen_args, second_gen_args, symboltable, map) {
                    Ok(args) => args,
                    Err(mut errors) => {
                        errors.insert(0, default_error());
                        return Err(errors);
                    }
                };

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

            // Confirm that there are no generic mismatches between the two functions.
            let generic_args = match unify_generic_arguments(
                left_generic_arguments,
                right_generic_arguments,
                symboltable,
                map.as_deref_mut(),
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

            // PARAMETERS.
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
            let mut params = vec![];
            for (left_param, right_param) in left_params.iter().zip(right_params.iter()) {
                let left_inferred_type = &left_param.inferred_type;
                let right_inferred_type = &right_param.inferred_type;
                let map = map.as_deref_mut();
                let inferred_type = match (left_param.is_optional, right_param.is_optional) {
                    (true, true) => unify_types(
                        &maybify(left_inferred_type.clone()),
                        &maybify(right_inferred_type.clone()),
                        symboltable,
                        map,
                    ),
                    (true, false) => unify_types(
                        &maybify(left_inferred_type.clone()),
                        right_inferred_type,
                        symboltable,
                        map,
                    ),
                    (false, true) => unify_types(
                        left_inferred_type,
                        &maybify(right_inferred_type.clone()),
                        symboltable,
                        map,
                    ),
                    (false, false) => {
                        unify_types(left_inferred_type, right_inferred_type, symboltable, map)
                    }
                };

                let result = ParameterType {
                    name: left_param.name.clone(),
                    is_optional: false,
                    type_label: None,
                    inferred_type: match inferred_type {
                        Ok(param) => param,
                        Err(mut errors) => {
                            errors.insert(0, default_error());
                            return Err(errors);
                        }
                    },
                };
                params.push(result);
            }

            // RETURN TYPES.
            // If one function is async and the other returns a prospect, they are unifiable.
            if left_is_async {
                left_return_type = utils::prospectify(left_return_type, symboltable);
            }
            if right_is_async {
                right_return_type = utils::prospectify(right_return_type, symboltable);
            }
            let return_type = Box::new(unify_types(
                &left_return_type,
                &right_return_type,
                symboltable,
                map,
            )?);

            return Ok(EvaluatedType::FunctionExpressionInstance {
                is_async,
                params,
                return_type,
                generic_args,
            });
        }
        // Both types are borrowed.
        (Borrowed { base: left_type }, Borrowed { base: right_type }) => {
            return Ok(Borrowed {
                base: Box::new(unify_types(&left_type, &right_type, symboltable, map)?),
            })
        }
        _ => Err(vec![TypeErrorType::MismatchedAssignment {
            left: symboltable.format_evaluated_type(left),
            right: symboltable.format_evaluated_type(right),
        }]),
    }
}

/// Compares two lists of generic arguments to determine
/// how similar or dissimilar they are.
pub fn unify_generic_arguments(
    left_generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    right_generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    symboltable: &SymbolTable,
    mut map: Option<&mut HashMap<SymbolIndex, EvaluatedType>>,
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
        let (_, left_evaluated_type) = left_generic_arguments.get(left_arr_index).unwrap();
        let (symbol_index, right_evaluated_type) =
            right_generic_arguments.get(right_arr_index).unwrap();

        generic_args.push((
            *symbol_index,
            match unify_types(
                left_evaluated_type,
                right_evaluated_type,
                symboltable,
                map.as_mut().map(|m| &mut **m),
            ) {
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
                    return_type,
                    ..
                }
                | SemanticSymbolKind::Function {
                    is_async,
                    params,
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
                                    ..
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
