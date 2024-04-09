use crate::{
    utils::{distill_as_function_type, get_interface_types_from_symbol, FunctionType}, EvaluatedType::*, 
    SemanticSymbolKind, SymbolIndex, SymbolLibrary, UNKNOWN, *,
};
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


/// Given a target type T and a candidate type U, the unification of T <- U is 
/// an operation that subsumes the type T, compares all its bounds and constraints,
/// and produces the lowest upper bound for which U is equivalent to T, if it exists,
/// or type errors if it does not.
/// 
/// It optionally takes in a map so it can track the generic parameters
/// that have been transformed.
// TODO: Return custom error if one type is a Maybe of the other.
// TODO: Return custom error if one type is an array of the other.
pub fn unify_types(
    target: &EvaluatedType,
    candidate: &EvaluatedType,
    symbollib: &SymbolLibrary,
    options: UnifyOptions,
    mut map: Option<&mut HashMap<SymbolIndex, EvaluatedType>>,
) -> Result<EvaluatedType, Vec<TypeErrorType>> {
    let default_error = || TypeErrorType::MismatchedAssignment {
        target: symbollib.format_evaluated_type(target),
        right: symbollib.format_evaluated_type(candidate),
    };
    let maybify = |typ| utils::maybify(typ, symbollib);
    
    match (target, candidate) {
        // Types are directly equal, therefore they are unifiable.
        (_, _) if target == candidate => return Ok(target.clone()),
        // Left type is a hard generic, and hard generic massaging is requested.
        (HardGeneric { base } | Generic { base }, free_type)
            if matches!(options, UnifyOptions::HardConform) =>
        {
            solve_generic_type(
                &mut map,
                base,
                free_type,
                symbollib,
                default_error,
                options,
            )
        }
        // Left type is a generic parameter.
        (Generic { base }, free_type) if !free_type.is_void() => solve_generic_type(
            &mut map,
            base,
            free_type,
            symbollib,
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
                symbollib,
                default_error,
                options,
            )
        }
        // Right type is a generic parameter and conformity is requested.
        (free_type, Generic { base })
            if matches!(options, UnifyOptions::Conform | UnifyOptions::AnyNever | UnifyOptions::Return) && !free_type.is_void() =>
        {
            solve_generic_type(
                &mut map,
                base,
                free_type,
                symbollib,
                default_error,
                options,
            )
        }
        // Numbers
        // (
        //     ModelInstance { model: first_model, .. },
        //     ModelInstance { model: second_model, .. },
        // ) if is_numeric_type(target, symbollib) && is_numeric_type(candidate, symbollib) => {
        //     unify_numbers(first_model, second_model, target, candidate, symbollib, default_error)
        // }
        // Comparing model or interface instances.
        // Two instances are unifiable if they refer to the same symbol,
        // and their generic list is unifiable.
        (
            ModelInstance {
                model: first,
                generic_arguments: first_gen_args,
                ..
            },
            ModelInstance {
                model: second,
                generic_arguments: second_gen_args,
                ..
            },
        ) | (
            InterfaceInstance {
                interface_: first,
                generic_arguments: first_gen_args,
                ..
            },
            InterfaceInstance {
                interface_: second,
                generic_arguments: second_gen_args,
                ..
            },
        ) | (
            EnumInstance {
                enum_: first,
                generic_arguments: first_gen_args,
                ..
            },
            EnumInstance {
                enum_: second,
                generic_arguments: second_gen_args,
                ..
            },
        ) => {
            let first_instance_symbol = match symbollib.get(*first) {
                Some(first) => first,
                None => return Ok(EvaluatedType::Unknown),
            };
            let second_instance_symbol = match symbollib.get(*second) {
                Some(second) => second,
                None => return Ok(EvaluatedType::Unknown),
            };
            if !std::ptr::eq(first_instance_symbol, second_instance_symbol) {
                return Err(vec![default_error()]);
            }
            if first_gen_args.len() != second_gen_args.len() {
                return Err(vec![
                    default_error(),
                    TypeErrorType::MismatchedGenericArgs {
                        name: first_instance_symbol.name.clone(),
                        expected: first_gen_args.len(),
                        assigned: second_gen_args.len(),
                    },
                ]);
            }
            let final_generic_args = match unify_generic_arguments(
                first_gen_args,
                second_gen_args,
                symbollib,
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
            // Truncate the list of arguments to the appropriate size.
            let generic_arg_length = match &first_instance_symbol.kind {
                SemanticSymbolKind::Model { generic_params, .. } => generic_params.len(),
                _ => return Ok(EvaluatedType::Unknown),
            };

            let generic_arguments = final_generic_args
                    .into_iter()
                    .filter(|(base, _)| {
                        first_gen_args
                            .iter()
                            .any(|(firstbase, _)| firstbase == base)
                    })
                    .take(generic_arg_length)
                    .collect();
            return Ok(if first_instance_symbol.kind.is_model() {
                ModelInstance {
                model: *first,
                generic_arguments,
                is_invariant: false,
            }} else if first_instance_symbol.kind.is_interface() {
                InterfaceInstance { interface_: *first, generic_arguments, is_invariant: false }   
            } else {
                EnumInstance { enum_: *first, is_invariant: false, generic_arguments }
            });
        }
        // Either type is unknown.
        // The two types are automatically unifiable.
        (Unknown, other) | (other, Unknown) => Ok(other.clone()),
        // Either type is a function.
        // The two types can be unified if they have unifiable parameters, generic arguments and return types.
        // Function types that return a Prospect<T> are unifiable with async function types that return T.
        (
            FunctionExpressionInstance { .. } | FunctionInstance { .. } | MethodInstance { .. },
            FunctionExpressionInstance { .. } | FunctionInstance { .. } | MethodInstance { .. },
        ) => {
            let FunctionType {is_async: left_is_async, parameter_types: left_params, generic_arguments: left_generic_arguments, return_type: mut left_return_type} =
                match distill_as_function_type(target, symbollib) {
                    Some(functiontype) => functiontype,
                    None => return Ok(EvaluatedType::Unknown)
                };
            let FunctionType {is_async: right_is_async, parameter_types: right_params, generic_arguments: right_generic_arguments, return_type: mut right_return_type} =
                match distill_as_function_type(candidate, symbollib) {
                    Some(functiontype) => functiontype,
                    None => return Ok(EvaluatedType::Unknown)
                };
            // Confirm that there are no generic mismatches between the two functions.
            let generic_args = match unify_generic_arguments(
                left_generic_arguments,
                right_generic_arguments,
                symbollib,
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
            for (i, param) in left_params.iter().enumerate() {
                if param.is_optional {
                    break;
                }
                if right_params.get(i).is_none() {
                    return Err(vec![
                        default_error(),
                        TypeErrorType::MismatchedFunctionParams { expected: left_params.len(), found: i, least_required: Some(i + 1) }
                    ])
                }
            }
            let mut params = vec![];
            for (left_param, right_param) in left_params.iter().zip(right_params.iter()) {
                let mut left_inferred_type = left_param.inferred_type.clone();
                let mut right_inferred_type = right_param.inferred_type.clone();
                let map = map.as_deref_mut();
                if left_param.is_optional {
                    left_inferred_type = maybify(left_inferred_type);
                }
                if right_param.is_optional {
                    right_inferred_type = maybify(right_inferred_type);
                }
                let inferred_type = unify_types(&left_inferred_type, &right_inferred_type, symbollib, options, map);
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
            if left_is_async {
                left_return_type = utils::prospectify(left_return_type, symbollib);
            }
            if right_is_async {
                right_return_type = utils::prospectify(right_return_type, symbollib);
            }
            let return_type_unification = unify_types(
                &left_return_type,
                &right_return_type,
                symbollib,
                options,
                map,
            );
            let return_type = match return_type_unification {
                Ok(return_type) => return_type,
                Err(mut errors) => {
                    errors.insert(0, default_error());
                    return Err(errors);
                }
            };
            let return_type = Box::new(return_type);
            return Ok(FunctionExpressionInstance {
                is_async: false,
                params,
                return_type,
                generic_args,
                is_invariant: false,
            });
        }
        // Left type is never, and the unification mode is special.
        (Never, right_type)
            if matches!(
                options,
                UnifyOptions::AnyNever | UnifyOptions::Conform | UnifyOptions::HardConform
            ) =>
        {
            Ok(right_type.clone())
        }
        // Right type is never.
        // Never types are unifiable with every other type from the right.
        (free, Never) => Ok(free.clone()),
        // Left type is opaque, and right type is a model or enum variant.
        // Unification is possible if right type is a component of left.
        (
            OpaqueTypeInstance {
                collaborators,
                generic_arguments: opaque_generics,
                aliased_as,
                available_methods: methods,
                available_interfaces: interfaces
            },
            ModelInstance {
                model: child,
                generic_arguments: subgenerics,
                ..
            }
            | EnumInstance {
                enum_: child,
                generic_arguments: subgenerics,
                ..
            },
        ) => {
            let mut errors = vec![];
            if !collaborators.iter().any(|collab| collab == child) {
                let error = TypeErrorType::InvalidOpaqueTypeAssignment {
                    left: symbollib.format_evaluated_type(target),
                    right: symbollib.format_evaluated_type(candidate),
                };
                errors.push(default_error());
                errors.push(error);
                return Err(errors);
            }
            // Update generic arguments.
            let generic_arguments = match unify_generic_arguments(
                opaque_generics,
                subgenerics,
                symbollib,
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
                available_interfaces: interfaces.clone()
            });
        }
        // Left type is opaque and right type is generic.
        // Unification is possible if left type contains right type as a collaborator.
        (OpaqueTypeInstance { collaborators, .. }, HardGeneric { base } | Generic { base }) => {
            let mut errors = vec![];
            if !collaborators.iter().any(|collab| collab == base) {
                let error = TypeErrorType::InvalidOpaqueTypeAssignment {
                    left: symbollib.format_evaluated_type(target),
                    right: symbollib.format_evaluated_type(candidate),
                };
                errors.push(default_error());
                errors.push(error);
                return Err(errors);
            }
            return Ok(target.clone());
        }
        // Both types are opaque.
        // Unification is possible if left type is a superset of right type.
        (
            OpaqueTypeInstance {
                aliased_as,
                collaborators: left_collaborators,
                generic_arguments: left_generics,
                available_methods: methods,
                available_interfaces: interfaces,
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
                        left: symbollib.format_evaluated_type(target),
                        right: symbollib.format_evaluated_type(candidate),
                    });
                    return Err(errors);
                }
            }
            // Update generic arguments.
            let generic_arguments = match unify_generic_arguments(
                left_generics,
                right_generics,
                symbollib,
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
                aliased_as: *aliased_as,
                available_methods: methods.clone(),
                available_interfaces: interfaces.clone()
            });
        }
        // Otherwise, both types cannot be unified.
        _ => Err(vec![TypeErrorType::MismatchedAssignment {
            target: symbollib.format_evaluated_type(target),
            right: symbollib.format_evaluated_type(candidate),
        }]),
    }
}

// /// Unify two instances of numeric models.
// /// The casting chain is rtl,
// /// meaning that the right type must be smaller or equal in size
// /// to the left.
// /// Therefore:
// /// - Int32 <: Int32
// /// - Int64 <: Int32
// /// - Float32 <: Int64
// /// - Float64 <: Float32
// /// - Float64 <: Float64
// fn unify_numbers(
//     first_number: &SymbolIndex, 
//     second_number: &SymbolIndex, 
//     target: &EvaluatedType, 
//     candidate: &EvaluatedType,
//     symbollib: &SymbolLibrary, 
//     default_error: impl Fn() -> TypeErrorType, 
// ) -> Result<EvaluatedType, Vec<TypeErrorType>> {
//     let first_model = *first_number;
//     let second_model = *second_number;
//     if first_model == second_model {
//         return Ok(target.clone());
//     }
//     let (first_is_int32, second_is_int32) = if let Some(idx) = symbollib.int32 {
//         (first_model == idx, second_model == idx)
//     } else {
//         (false, false)
//     };
//     let (_, second_is_int64) = if let Some(idx) = symbollib.int64 {
//         (first_model == idx, second_model == idx)
//     } else {
//         (false, false)
//     };
//     let (first_is_float64, _) = if let Some(idx) = symbollib.float64 {
//         (first_model == idx, second_model == idx)
//     } else {
//         (false, false)
//     };
//     // Float64 can subsume any other numeric type.
//     if first_is_float64 || 
//         // Int32 is castable to any other type.
//         second_is_int32 ||
//         // Int64 is castable to itself, f32 and f64.
//         (second_is_int64 && !first_is_int32)
//     {
//         return Ok(target.clone());
//     }
//     return Err(vec![default_error(), TypeErrorType::NumericCastingError {
//         left: symbollib.format_evaluated_type(target),
//         right: symbollib.format_evaluated_type(candidate)
//     }])
// }

/// Generates a solution for a generic based on another evaluated type.
/// It simply checks to see if the other type obeys all the constraints defined on the generic.
fn solve_generic_type(
    map: &mut Option<&mut HashMap<SymbolIndex, EvaluatedType>>,
    base: &SymbolIndex,
    free_type: &EvaluatedType,
    symbollib: &SymbolLibrary,
    default_error: impl Fn() -> TypeErrorType,
    options: UnifyOptions,
) -> Result<EvaluatedType, Vec<TypeErrorType>> {
    // If there is already a prior solution for the left type,
    // check that it is unifiable with the right,
    // and update its mapped value.
    'check_for_prior_solutions: {
        if let Some(map) = map.as_deref_mut() {
            if let Some(already_assigned) = map.get(base).cloned() {
                // for reasons that will have to be checked again later, there are scenarios where 
                // the solution for the generic T is {type T}, so trying to
                // solve it results in an infinite loop.
                if let Generic {base: prior_solution_base} = &already_assigned {
                    if prior_solution_base == base {
                        break 'check_for_prior_solutions;
                    }
                }
                match unify_types(
                    &already_assigned,
                    &free_type,
                    symbollib,
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
    }
    
    let base_parameter = match symbollib.get_forwarded(*base) {
        Some(base) => base,
        None => return Ok(Unknown),
    };
    let default_value = match &base_parameter.kind {
        SemanticSymbolKind::GenericParameter {
            default_value,
            ..
        } => {
            default_value
        }
        // Something has gone wrong if this ever happens. Look into it.
        _ => return Ok(Unknown),
    };
    // Rendition of prior solutions as a vector.
    let solved_generics = map
            .as_ref()
            .map(|map| map.iter().map(|(a, b)| (a.clone(), b.clone())).collect());
    // Default generic type if other is unknown.
    if let Some(default) = default_value {
        if free_type.is_unknown() {
            return Ok(evaluate(
                default,
                symbollib,
                solved_generics.as_ref(),
                &mut None,
                0,
            ));
        }
    }
    let interfaces_in_generic = get_interface_types_from_symbol(*base, symbollib, solved_generics.as_ref().unwrap_or(&vec![]));
    if interfaces_in_generic.is_empty() {
        // Generic parameter solved.
        let final_type = free_type.clone();
        if let Some(map) = map.as_deref_mut() {
            map.insert(*base,  final_type.clone());
        }
        return Ok(final_type)
    }

    let free_type_generics = match free_type {
        ModelInstance { generic_arguments,.. }
        | InterfaceInstance { generic_arguments,.. }
            => Some(generic_arguments),
        _ => None,
    };
    let free_type_implementations = match free_type {
        ModelInstance { model: base, .. }
        | InterfaceInstance { interface_: base,.. }
        | Generic { base }
        | HardGeneric { base } => get_interface_types_from_symbol(*base, symbollib, free_type_generics.unwrap_or(&vec![])),
        _ => vec![]
    };

    let mut errors = vec![];
    // For every interface implemented on the generic type, there must be a matching unifiable implementation
    // in the free type.
    for interface_evaluated in interfaces_in_generic {
        // The interface guard does not refer to a interface.
        // Unification cannot continue, but it is not the problem of this process.
        if !interface_evaluated.is_interface_instance() {
            return Ok(Unknown);
        }
        

        let interface_is_implemented = 
        // Interface is implemented if it has a matching implementation 
        // in the list of the free_type's implementations.
        free_type_implementations
            .iter()
            .any(|evaluated_implemented_type| {
                // todo: block infinite types.
                // if let Some(solved_generics) = solved_generics.as_ref() {
                //     evaluated_implemented_type = coerce(evaluated_implemented_type, solved_generics)
                // }

                // Unknown types will unify easily, but it does not conclude that the
                // implementation is valid.
                if !evaluated_implemented_type.is_interface_instance() {
                    return false;
                }
                return unify_types(
                    &interface_evaluated, 
                    &evaluated_implemented_type, 
                    symbollib, 
                    options,
                    map.as_deref_mut()
                ).is_ok()
            })
        || match free_type {
            // An interface is implemented by an opaque type if all its collaborator
            // types implement the interface.
            OpaqueTypeInstance { available_interfaces, .. } => {
                available_interfaces.iter().find(|interface_| interface_ == &&interface_evaluated).is_some()
            }
            _=> false
        };
        // Since interface instances are placeholders for instances of models 
        // that implement said interface, the logical conclusion is that
        // an interface is an implementation of itself.
        let is_equal_interface =  
            free_type == &interface_evaluated
        || match (free_type, &interface_evaluated) {
            (
                EvaluatedType::InterfaceInstance { 
                    interface_: first_interface, 
                    generic_arguments: first_gen_args,
                    ..
                },
                EvaluatedType::InterfaceInstance { 
                    interface_: second_interface, 
                    generic_arguments: second_gen_args,.. 
                }
            ) => {
                first_interface == second_interface && 
                match unify_generic_arguments(
                    first_gen_args, 
                    second_gen_args, symbollib, options, map.as_deref_mut()
                ) {
                    Ok(_) => true,
                    Err(mut gen_errors) => {
                        errors.append(&mut gen_errors);
                        false
                    },
                }
            }
            _ => false,
        };
        if !interface_is_implemented && !is_equal_interface {
            errors.push(
                TypeErrorType::UnimplementedInterface {
                    offender: symbollib.format_evaluated_type(free_type),
                    _interface: symbollib.format_evaluated_type(&interface_evaluated),
                    base_generic: Some(base_parameter.name.clone())
                },
            );
        }
    }

    if !errors.is_empty() {
        errors.insert(0, default_error());
        return Err(errors);
    }
    
    // Generic parameter solved.
    let final_type = free_type.clone();
    if let Some(map) = map.as_deref_mut() {
        map.insert(*base,  final_type.clone());
    }
    Ok(final_type)
}

/// Compares two lists of generic arguments to determine
/// how similar or dissimilar they are.
pub fn unify_generic_arguments(
    left_generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    right_generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    symbollib: &SymbolLibrary,
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
            match right_generic_arguments.get(right_arr_index) {
                Some(generic_tuple) => generic_tuple,
                None => continue,
            };
        generic_args.push((
            *symbol_index,
            match unify_types(
                left_evaluated_type,
                right_evaluated_type,
                symbollib,
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
    symbollib: &SymbolLibrary,
    mut map: Option<&mut HashMap<SymbolIndex, EvaluatedType>>,
) -> Result<EvaluatedType, Vec<TypeErrorType>> {
    let default_error = || TypeErrorType::MismatchedAssignment {
        target: symbollib.format_evaluated_type(left),
        right: symbollib.format_evaluated_type(right),
    };
    match (left, right) {
        (free_type, Generic { base }) => solve_generic_type(
            &mut map,
            base,
            free_type,
            symbollib,
            default_error,
            UnifyOptions::Conform,
        ),
        _ => unify_types(left, right, symbollib, UnifyOptions::Conform, map),
    }
}


/// Two types T and U are convergent if `unify_types(T, U) == unify_types(U, T) == Ok(V)`
pub fn converge_types(
    type_a: &EvaluatedType,
    type_b: &EvaluatedType,
    symbollib: &SymbolLibrary,
) -> Option<EvaluatedType> {
    unify_types(type_a, type_b, symbollib, UnifyOptions::None, None)
        .ok()
        .map(|forward| {
            unify_types(type_b, type_a, symbollib, UnifyOptions::None, None)
                .ok()
                .and_then(|backward| match forward == backward {
                    true => Some(forward),
                    false => None,
                })
        })
        .flatten()
}
