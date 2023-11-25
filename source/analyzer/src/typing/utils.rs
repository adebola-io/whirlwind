use errors::TypeErrorType;

use crate::{evaluate, EvaluatedType, ParameterType, SemanticSymbolKind, SymbolIndex, SymbolTable};

/// Encloses an evaluated type as a prospect.
/// It does nothing if the intrinsic Prospect type is unreachable.
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

pub fn is_boolean(evaluated_type: &EvaluatedType, symboltable: &SymbolTable) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if symboltable.bool_symbol.is_some_and(|prospect| prospect == *model)
    )
}

/// Returns true if an evaluated type is a prospect.
pub fn is_prospective_type(evaluated_type: &EvaluatedType, symboltable: &SymbolTable) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if symboltable.prospect_symbol.is_some_and(|prospect| prospect == *model)
    )
}

/// Encloses an evaluated type as a Maybe.
/// It does nothing if the intrinsic Maybe type is unreachable.
pub fn maybify(typ: EvaluatedType, symboltable: &SymbolTable) -> EvaluatedType {
    if let Some(model) = symboltable.maybe_symbol {
        let maybe_symbol = symboltable.get(model).unwrap();
        let maybe_generic_parameter = match &maybe_symbol.kind {
            SemanticSymbolKind::Model { generic_params, .. } => generic_params[0],
            _ => unreachable!(),
        };
        return EvaluatedType::ModelInstance {
            model,
            generic_arguments: vec![(maybe_generic_parameter, typ)],
        };
    } else {
        return typ;
    }
}

/// Encloses an evaluated type in an array.
/// It does nothing if the intrinsic Array type is unreachable.
pub fn arrify(typ: EvaluatedType, symboltable: &SymbolTable) -> EvaluatedType {
    if let Some(model) = symboltable.array_symbol {
        let maybe_symbol = symboltable.get(model).unwrap();
        let maybe_generic_parameter = match &maybe_symbol.kind {
            SemanticSymbolKind::Model { generic_params, .. } => generic_params[0],
            _ => return EvaluatedType::Unknown,
        };
        return EvaluatedType::ModelInstance {
            model,
            generic_arguments: vec![(maybe_generic_parameter, typ)],
        };
    } else {
        return typ;
    }
}

/// Returns true if a type is evaluated to an array.
pub fn is_array(typ: &EvaluatedType, symboltable: &SymbolTable) -> bool {
    match symboltable.array_symbol {
        Some(array_symbol) => {
            matches!(typ, EvaluatedType::ModelInstance { model,.. } if *model == array_symbol)
        }
        None => false,
    }
}

/// Coerce generics in a type according to a generic list.
pub fn coerce(typ: EvaluatedType, args: &Vec<(SymbolIndex, EvaluatedType)>) -> EvaluatedType {
    match typ {
        EvaluatedType::ModelInstance {
            model,
            generic_arguments: old_generic_arguments,
        } => {
            let mut generic_arguments = vec![];
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::ModelInstance {
                model,
                generic_arguments,
            }
        }
        EvaluatedType::TraitInstance {
            trait_,
            generic_arguments: old_generic_arguments,
        } => {
            let mut generic_arguments = vec![];
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::TraitInstance {
                trait_,
                generic_arguments,
            }
        }
        EvaluatedType::EnumInstance {
            enum_,
            generic_arguments: old_generic_arguments,
        } => {
            let mut generic_arguments = vec![];
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::EnumInstance {
                enum_,
                generic_arguments,
            }
        }
        EvaluatedType::FunctionInstance {
            function,
            generic_arguments: old_generic_arguments,
        } => {
            let mut generic_arguments = vec![];
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::FunctionInstance {
                function,
                generic_arguments,
            }
        }
        EvaluatedType::FunctionExpressionInstance {
            is_async,
            params: old_params,
            return_type: old_return_type,
            generic_args: old_generic_args,
        } => {
            let mut generic_args = vec![];
            for (argument, old_type) in old_generic_args {
                generic_args.push((argument, coerce(old_type, args)));
            }
            let mut params = vec![];
            for param in old_params {
                params.push(ParameterType {
                    inferred_type: coerce(param.inferred_type, args),
                    ..param
                })
            }
            let return_type = Box::new(coerce(*old_return_type, args));
            EvaluatedType::FunctionExpressionInstance {
                is_async,
                params,
                return_type,
                generic_args,
            }
        }
        EvaluatedType::MethodInstance {
            method,
            generic_arguments: old_generic_arguments,
        } => {
            let mut generic_arguments = vec![];
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::MethodInstance {
                method,
                generic_arguments,
            }
        }
        EvaluatedType::Generic { base } => {
            for (index, new_type) in args {
                if base == *index {
                    return new_type.clone();
                }
            }
            EvaluatedType::Generic { base }
        }
        EvaluatedType::HardGeneric { base } => {
            for (index, new_type) in args {
                if base == *index {
                    return new_type.clone();
                }
            }
            EvaluatedType::HardGeneric { base }
        }
        EvaluatedType::Borrowed { base } => EvaluatedType::Borrowed {
            base: Box::new(coerce(*base, args)),
        },
        // todo: opaque type coercion.
        _ => typ,
    }
}

/// Force all free generic arguments to unify to a given type.
pub fn coerce_all_generics(input: &EvaluatedType, coercion_target: EvaluatedType) -> EvaluatedType {
    let mut generic_arguments = vec![];
    input.gather_generics_into(&mut generic_arguments);
    let map = generic_arguments
        .into_iter()
        .map(|generic| (generic, coercion_target.clone()))
        .collect::<Vec<_>>();
    coerce(input.clone(), &map)
}

/// Converts a list of generic parameter indexes to a list of evaluated generic argument types.
pub fn evaluate_generic_params(
    generic_params: &Vec<SymbolIndex>,
) -> Vec<(SymbolIndex, EvaluatedType)> {
    generic_params
        .iter()
        .map(|idx| (*idx, EvaluatedType::Generic { base: *idx }))
        .collect()
}

/// Convert a symbol to its inferred type.
pub fn symbol_to_type(
    symbol: &crate::SemanticSymbol,
    name: SymbolIndex,
    symboltable: &SymbolTable,
) -> Result<EvaluatedType, Result<EvaluatedType, TypeErrorType>> {
    let eval_type = match &symbol.kind {
        SemanticSymbolKind::Module { .. } => EvaluatedType::Module(name),
        SemanticSymbolKind::Trait { .. } => EvaluatedType::Trait(name),
        SemanticSymbolKind::Model { .. } => EvaluatedType::Model(name),
        SemanticSymbolKind::Enum { .. } => EvaluatedType::Enum(name),
        SemanticSymbolKind::Variant { owner_enum, .. } => EvaluatedType::EnumInstance {
            enum_: *owner_enum,
            generic_arguments: {
                // Try to create a space for unknown enum generics.
                // todo: unify from tagged types.
                let enum_symbol = symboltable.get_forwarded(*owner_enum).unwrap();
                match &enum_symbol.kind {
                    SemanticSymbolKind::Enum { generic_params, .. } => {
                        evaluate_generic_params(generic_params)
                    }
                    _ => vec![],
                }
            },
        },
        SemanticSymbolKind::Variable { inferred_type, .. } => inferred_type.clone(),
        SemanticSymbolKind::Constant { inferred_type, .. } => inferred_type.clone(),
        SemanticSymbolKind::Parameter {
            is_optional,
            param_type,
            inferred_type,
            ..
        } => {
            let inferred_type = param_type
                .as_ref()
                .map(|param_type| evaluate(param_type, symboltable, None, &mut None, 0))
                .unwrap_or(inferred_type.clone());
            if *is_optional {
                maybify(inferred_type, symboltable)
            } else {
                inferred_type
            }
        }
        SemanticSymbolKind::GenericParameter { .. } | SemanticSymbolKind::TypeName { .. } => {
            return Err(Err(TypeErrorType::TypeAsValue {
                type_: symbol.name.clone(),
            }));
        }
        SemanticSymbolKind::Function { generic_params, .. } => EvaluatedType::FunctionInstance {
            function: name,
            generic_arguments: evaluate_generic_params(generic_params),
        }, //TODO
        _ => EvaluatedType::Unknown,
    };
    Ok(eval_type)
}

/// Extract the available methods as evaluated types from a model, trait or generic.
pub fn get_method_types_from_symbol<'a>(
    symbol_idx: SymbolIndex,
    symboltable: &'a SymbolTable,
    generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
) -> Vec<(&'a String, EvaluatedType, bool)> {
    let mut method_types = vec![];
    let symbol = symboltable.get(symbol_idx);
    if symbol.is_none() {
        return vec![];
    }
    let symbol = symbol.unwrap();
    match &symbol.kind {
        SemanticSymbolKind::Model { methods, .. } | SemanticSymbolKind::Trait { methods, .. } => {
            for method in methods {
                let method = *method;
                let method_symbol = symboltable.get(method);
                if method_symbol.is_none() {
                    continue;
                }
                let method_symbol = method_symbol.unwrap();
                let generic_params = match &method_symbol.kind {
                    SemanticSymbolKind::Method { generic_params, .. } => generic_params,
                    _ => continue,
                };
                let initial_type = EvaluatedType::MethodInstance {
                    method,
                    generic_arguments: evaluate_generic_params(generic_params),
                };
                let coerced = coerce(initial_type, generic_arguments);
                method_types.push((&method_symbol.name, coerced, method_symbol.kind.is_public()));
            }
        }
        SemanticSymbolKind::GenericParameter { traits, .. } => {
            for int_typ in traits {
                let evaled = evaluate(int_typ, symboltable, Some(generic_arguments), &mut None, 0);
                if let EvaluatedType::TraitInstance {
                    trait_,
                    generic_arguments,
                } = evaled
                {
                    let mut methods_from_trait =
                        get_method_types_from_symbol(trait_, symboltable, &generic_arguments);
                    method_types.append(&mut methods_from_trait);
                }
            }
        }
        _ => {}
    }
    return method_types;
}

/// Get an implementation of a trait from an evaluated type, if it exists.
pub fn get_implementation_of(
    target_trait: SymbolIndex,
    operand_type: &EvaluatedType,
    symboltable: &SymbolTable,
) -> Option<EvaluatedType> {
    match operand_type {
        EvaluatedType::ModelInstance { model: base, .. }
        | EvaluatedType::TraitInstance { trait_: base, .. }
        | EvaluatedType::Generic { base }
        | EvaluatedType::HardGeneric { base } => {
            let base_symbol = symboltable.get(*base)?;
            let implementation_list = match &base_symbol.kind {
                SemanticSymbolKind::Model {
                    implementations, ..
                }
                | SemanticSymbolKind::Trait {
                    implementations, ..
                }
                | SemanticSymbolKind::GenericParameter {
                    traits: implementations,
                    ..
                } => implementations,
                _ => return None,
            };
            for implementation in implementation_list {
                let evaluated = evaluate(implementation, symboltable, None, &mut None, 0);
                if let EvaluatedType::TraitInstance { trait_, .. } = &evaluated {
                    if *trait_ == target_trait {
                        return Some(evaluated);
                    }
                }
            }
            return None;
        }
        _ => None,
    }
}
