use crate::{EvaluatedType::*, SemanticSymbolKind, SymbolIndex, SymbolTable, UNKNOWN, *};
use errors::TypeErrorType;
use std::collections::HashMap;

#[derive(Clone, Copy)]
pub enum UnifyOptions {
    None,
    AnyNever,
    Conform,
    HardConform,
    Return,
}

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
    options: UnifyOptions,
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
        // Left type is a hard generic, and hard generic massaging is requested.
        (HardGeneric { base } | Generic { base }, free_type)
            if matches!(options, UnifyOptions::HardConform) && !free_type.is_void() =>
        {
            solve_generic_type(
                &mut map,
                base,
                free_type,
                symboltable,
                default_error,
                options,
            )
        }
        // // One type is a hard generic, and other type is a soft variant of it.
        // (
        //     HardGeneric { base: firstbase } | Generic { base: firstbase },
        //     HardGeneric { base: secondbase } | Generic { base: secondbase },
        // ) if firstbase == secondbase => Ok(Generic { base: *firstbase }),
        // Left type is a generic parameter.
        (Generic { base }, free_type) if !free_type.is_void() => solve_generic_type(
            &mut map,
            base,
            free_type,
            symboltable,
            default_error,
            options,
        ),
        // Right type is a generic parameter and conformity is requested.
        (free_type, Generic { base } | HardGeneric { base })
            if matches!(options, UnifyOptions::HardConform) && !free_type.is_void() =>
        {
            solve_generic_type(
                &mut map,
                base,
                free_type,
                symboltable,
                default_error,
                options,
            )
        }
        // Right type is a generic parameter and conformity is requested.
        (free_type, Generic { base })
            if matches!(options, UnifyOptions::Conform) && !free_type.is_void() =>
        {
            solve_generic_type(
                &mut map,
                base,
                free_type,
                symboltable,
                default_error,
                options,
            )
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
            let final_generic_args = match unify_generic_arguments(
                first_gen_args,
                second_gen_args,
                symboltable,
                options,
                map.as_deref_mut(),
            ) {
                Ok(args) => args,
                Err(mut errors) => {
                    errors.insert(0, default_error());
                    return Err(errors);
                }
            };
            // Save solutions in map.
            if let Some(map) = map.as_deref_mut() {
                for (base, eval_type) in final_generic_args.iter() {
                    map.insert(*base, eval_type.clone());
                }
            }
            // Truncate the list of arguments to the appriopriate size.
            let generic_arg_length = match &first_model_symbol.kind {
                SemanticSymbolKind::Model { generic_params, .. } => generic_params.len(),
                _ => return Ok(EvaluatedType::Unknown),
            };

            return Ok(ModelInstance {
                model: *first,
                generic_arguments: final_generic_args
                    .into_iter()
                    .filter(|(base, _)| {
                        first_gen_args
                            .iter()
                            .any(|(firstbase, _)| firstbase == base)
                    })
                    .take(generic_arg_length)
                    .collect(),
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
                options,
                map.as_deref_mut(),
            ) {
                Ok(args) => args,
                Err(mut errors) => {
                    errors.insert(0, default_error());
                    return Err(errors);
                }
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
                        options,
                        map,
                    ),
                    (true, false) => unify_types(
                        &maybify(left_inferred_type.clone()),
                        right_inferred_type,
                        symboltable,
                        options,
                        map,
                    ),
                    (false, true) => unify_types(
                        left_inferred_type,
                        &maybify(right_inferred_type.clone()),
                        symboltable,
                        options,
                        map,
                    ),
                    (false, false) => unify_types(
                        left_inferred_type,
                        right_inferred_type,
                        symboltable,
                        options,
                        map,
                    ),
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
                options,
                map,
            )?);
            return Ok(FunctionExpressionInstance {
                is_async: false,
                params,
                return_type,
                generic_args,
            });
        }
        // Both types are borrowed.
        (Borrowed { base: left_type }, Borrowed { base: right_type }) => {
            return Ok(Borrowed {
                base: Box::new(unify_types(
                    &left_type,
                    &right_type,
                    symboltable,
                    options,
                    map,
                )?),
            })
        }
        // Left type is never.
        (Never, right_type)
            if matches!(
                options,
                UnifyOptions::AnyNever | UnifyOptions::Conform | UnifyOptions::HardConform
            ) =>
        {
            Ok(right_type.clone())
        }
        // Right type is never.
        (free, Never) => Ok(free.clone()),
        // Left type is opaque, and right type is a model or enum variant.
        // Unification is possible if right type is a component of left.
        (
            OpaqueTypeInstance {
                collaborators,
                generic_arguments: opaque_generics,
                aliased_as,
                available_methods: methods,
            },
            ModelInstance {
                model: child,
                generic_arguments: subgenerics,
            }
            | EnumInstance {
                enum_: child,
                generic_arguments: subgenerics,
            },
        ) => {
            let mut errors = vec![];
            if !collaborators.iter().any(|collab| collab == child) {
                let error = TypeErrorType::InvalidOpaqueTypeAssignment {
                    left: symboltable.format_evaluated_type(left),
                    right: symboltable.format_evaluated_type(right),
                };
                errors.push(default_error());
                errors.push(error);
                return Err(errors);
            }
            // Update generic arguments.
            let generic_arguments = match unify_generic_arguments(
                opaque_generics,
                subgenerics,
                symboltable,
                options,
                map,
            ) {
                Ok(generic_list) => generic_list,
                Err(mut generic_errors) => {
                    errors.push(default_error());
                    errors.append(&mut generic_errors);
                    return Err(errors);
                }
            };
            return Ok(OpaqueTypeInstance {
                aliased_as: *aliased_as,
                collaborators: collaborators.clone(),
                generic_arguments,
                available_methods: methods.clone(),
            });
        }
        // Left type is opaque and right type is generic.
        // Unification is possible if left type contains right type as a collaborator.
        (
            OpaqueTypeInstance {
                collaborators,
                ..
            },
            HardGeneric { base } | Generic { base },
        ) => {
            let mut errors = vec![];
            if !collaborators.iter().any(|collab| collab == base) {
                let error = TypeErrorType::InvalidOpaqueTypeAssignment {
                    left: symboltable.format_evaluated_type(left),
                    right: symboltable.format_evaluated_type(right),
                };
                errors.push(default_error());
                errors.push(error);
                return Err(errors);
            }
            return Ok(left.clone());
        }
        // Both types are opaque.
        // Unification is possible if left type is a superset of right type.
        (
            OpaqueTypeInstance {
                collaborators: left_collaborators,
                generic_arguments: left_generics,
                available_methods: methods,
                ..
            },
            OpaqueTypeInstance {
                collaborators: right_collaborators,
                generic_arguments: right_generics,
                ..
            },
        ) => {
            let mut errors = vec![];
            for ri in right_collaborators {
                if !left_collaborators.iter().any(|c| c == ri) {
                    errors.push(TypeErrorType::MissingOpaqueComponent {
                        left: symboltable.format_evaluated_type(left),
                        right: symboltable.format_evaluated_type(right),
                    });
                    return Err(errors);
                }
            }
            // Update generic arguments.
            let generic_arguments = match unify_generic_arguments(
                left_generics,
                right_generics,
                symboltable,
                options,
                map,
            ) {
                Ok(generic_list) => generic_list,
                Err(mut generic_errors) => {
                    errors.push(default_error());
                    errors.append(&mut generic_errors);
                    return Err(errors);
                }
            };
            return Ok(OpaqueTypeInstance {
                collaborators: left_collaborators.to_vec(),
                generic_arguments,
                aliased_as: None,
                available_methods: methods.clone(),
            });
        }
        _ => Err(vec![TypeErrorType::MismatchedAssignment {
            left: symboltable.format_evaluated_type(left),
            right: symboltable.format_evaluated_type(right),
        }]),
    }
}

fn solve_generic_type(
    map: &mut Option<&mut HashMap<SymbolIndex, EvaluatedType>>,
    base: &SymbolIndex,
    free_type: &EvaluatedType,
    symboltable: &SymbolTable,
    default_error: impl Fn() -> TypeErrorType,
    options: UnifyOptions,
) -> Result<EvaluatedType, Vec<TypeErrorType>> {
    // If there is already a prior solution for the left type,
    // check that it is unifiable with the right,
    // and update its mapped value.
    if let Some(map) = map.as_deref_mut() {
        if let Some(already_assigned) = map.get(base).cloned() {
            match unify_types(
                &already_assigned,
                &free_type,
                symboltable,
                options,
                Some(map),
            ) {
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
                let solved_generics = map
                    .as_ref()
                    .map(|map| map.iter().map(|(a, b)| (a.clone(), b.clone())).collect());
                if free_type.is_unknown() {
                    return Ok(evaluate(
                        default,
                        symboltable,
                        solved_generics.as_ref(),
                        &mut None,
                        0,
                    ));
                }
            }
            for _trait in traits {
                let trait_evaluated = evaluate(_trait, symboltable, None, &mut None, 0);
                // The trait guard does not refer to a trait.
                // Unification cannot continue, but it is not the problem of this process.
                if !trait_evaluated.is_trait_instance() {
                    return Ok(Unknown);
                }
                let implementations = match free_type {
                    ModelInstance { model: base, .. }
                    | TraitInstance { trait_: base, .. }
                    | Generic { base }
                    | HardGeneric { base } => match &symboltable.get_forwarded(*base).unwrap().kind
                    {
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
                    },
                    _ => None,
                };
                let trait_is_implemented = implementations.is_some_and(|implementations| {
                    implementations
                        .iter()
                        .find(|implementation| {
                            // todo: block infinite types.
                            evaluate(implementation, symboltable, None, &mut None, 0)
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
            // Generic parameter solved.
            if let Some(map) = map.as_deref_mut() {
                map.insert(*base, free_type.clone());
            }
            Ok(free_type.clone())
        }
        // Something has gone wrong if this ever happens. Look into it.
        _ => Ok(Unknown),
    }
}

/// Compares two lists of generic arguments to determine
/// how similar or dissimilar they are.
pub fn unify_generic_arguments(
    left_generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    right_generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    symboltable: &SymbolTable,
    options: UnifyOptions,
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
                options,
                map.as_deref_mut(),
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
                                            0,
                                        )
                                    })
                                    .unwrap_or(EvaluatedType::Unknown),
                            }
                        })
                        .collect::<Vec<_>>();
                    let return_type = return_type
                        .as_ref()
                        .map(|typ| {
                            evaluate(typ, symboltable, Some(&generic_arguments), &mut None, 0)
                        })
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

/// Unifies a declaration.
/// It is freer than unify_types, because generics and the never type can be transformed in either direction.
pub fn unify_freely(
    left: &EvaluatedType,
    right: &EvaluatedType,
    symboltable: &SymbolTable,
    mut map: Option<&mut HashMap<SymbolIndex, EvaluatedType>>,
) -> Result<EvaluatedType, Vec<TypeErrorType>> {
    let default_error = || TypeErrorType::MismatchedAssignment {
        left: symboltable.format_evaluated_type(left),
        right: symboltable.format_evaluated_type(right),
    };
    match (left, right) {
        (free_type, Generic { base }) => solve_generic_type(
            &mut map,
            base,
            free_type,
            symboltable,
            default_error,
            UnifyOptions::Conform,
        ),
        _ => unify_types(left, right, symboltable, UnifyOptions::Conform, map),
    }
}
