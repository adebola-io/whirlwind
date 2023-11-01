use crate::{EvaluatedType, ParameterType, SemanticSymbolKind, SymbolIndex, SymbolTable};

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
        EvaluatedType::Borrowed { base } => EvaluatedType::Borrowed {
            base: Box::new(coerce(*base, args)),
        },
        // todo: opaque type coercion.
        _ => typ,
    }
}
