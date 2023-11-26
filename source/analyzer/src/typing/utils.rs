use crate::{
    evaluate, EvaluatedType, ParameterType, SemanticSymbolKind, SymbolIndex, SymbolTable,
    TypecheckerContext,
};
use errors::{TypeError, TypeErrorType};
/// Returns an intrinsic symbol from the symbol table or returns an unknown type.
macro_rules! get_intrinsic {
    ($expr: expr) => {{
        match $expr {
            Some(index) => index,
            None => return EvaluatedType::Unknown,
        }
    }};
}

/// Encloses an evaluated type as a prospect.
/// It does nothing if the intrinsic Prospect type is unreachable.
pub fn prospectify(typ: EvaluatedType, symboltable: &SymbolTable) -> EvaluatedType {
    if let Some(model) = symboltable.prospect {
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
        if symboltable.bool.is_some_and(|prospect| prospect == *model)
    )
}

/// Returns true if an evaluated type is a prospect.
pub fn is_prospective_type(evaluated_type: &EvaluatedType, symboltable: &SymbolTable) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if symboltable.prospect.is_some_and(|prospect| prospect == *model)
    )
}

/// Returns true if an evaluated type is a number.
pub fn is_numeric_type(evaluated_type: &EvaluatedType, symboltable: &SymbolTable) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if [
            symboltable.float32,
            symboltable.float64,
            symboltable.uint8,
            symboltable.uint16,
            symboltable.uint32,
            symboltable.uint64,
        ].iter().filter_map(|sym| *sym).any(|sym| sym == *model)
    ) || matches!(
        evaluated_type, EvaluatedType::OpaqueTypeInstance {aliased_as, ..}
        if [
            symboltable.float,
            symboltable.int,
            symboltable.uint
        ].iter().any(|sym| sym.as_ref() == aliased_as.as_ref())
    )
}

/// Encloses an evaluated type as a Maybe.
/// It does nothing if the intrinsic Maybe type is unreachable.
pub fn maybify(typ: EvaluatedType, symboltable: &SymbolTable) -> EvaluatedType {
    if let Some(model) = symboltable.maybe {
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
    if let Some(model) = symboltable.array {
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
    match symboltable.array {
        Some(array_symbol) => {
            matches!(typ, EvaluatedType::ModelInstance { model,.. } if *model == array_symbol)
        }
        None => false,
    }
}

/// Coerce generics (and possibly trait types) in a type according to a generic list.
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
            // Traits are coercible too because of the "This" type.
            if let Some(newtype) = args.iter().find(|(a, _)| *a == trait_) {
                return newtype.1.clone();
            }
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

/// Extract all available traits as evaluated types from a model, trait or generic.
pub fn get_trait_types_from_symbol(
    symbol_idx: SymbolIndex,
    symboltable: &SymbolTable,
    generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
) -> Vec<EvaluatedType> {
    let mut trait_types = vec![];
    let symbol = symboltable.get(symbol_idx);
    if symbol.is_none() {
        return vec![];
    }
    let symbol = symbol.unwrap();
    match &symbol.kind {
        SemanticSymbolKind::Model {
            implementations, ..
        }
        | SemanticSymbolKind::Trait {
            implementations, ..
        } => {
            for implementation in implementations {
                let initial_type = evaluate(
                    implementation,
                    symboltable,
                    Some(generic_arguments),
                    &mut None,
                    0,
                );
                if !initial_type.is_trait_instance() {
                    continue;
                }
                let coerced = coerce(initial_type, generic_arguments); // todo: is coercion still necessary?
                trait_types.push(coerced);
            }
        }
        SemanticSymbolKind::GenericParameter { traits, .. } => {
            for int_typ in traits {
                let evaled = evaluate(int_typ, symboltable, Some(generic_arguments), &mut None, 0);
                if let EvaluatedType::TraitInstance {
                    trait_,
                    generic_arguments,
                } = &evaled
                {
                    trait_types.push(evaled.clone());
                    let mut traits_from_trait =
                        get_trait_types_from_symbol(*trait_, symboltable, &generic_arguments);
                    trait_types.append(&mut traits_from_trait);
                }
            }
        }
        _ => {}
    }
    return trait_types;
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

/// Returns the evaluated type of a WhirlNumber.
/// It return unknown if intrinsic symbols are absent,
/// or there is an error in conversion.
pub fn get_numeric_type(
    symboltable: &SymbolTable,
    value: &ast::WhirlNumber,
    checker_ctx: Option<&mut TypecheckerContext<'_>>,
) -> EvaluatedType {
    let evaluate_index = |value| {
        evaluate(
            &crate::IntermediateType::SimpleType {
                value,
                generic_args: vec![],
                span: ast::Span::default(),
            },
            symboltable,
            None,
            &mut None,
            0,
        )
    };
    match &value.value {
        ast::Number::Decimal(decimal) => {
            // TODO: Move this to an earlier place.
            let number_evaluated = decimal.parse::<f64>();
            let number = match number_evaluated {
                Ok(number) => number,
                Err(error) => {
                    if let Some(ctx) = checker_ctx {
                        ctx.add_error(TypeError {
                            _type: TypeErrorType::NumericConversionError {
                                error: error.to_string(),
                            },
                            span: value.span,
                        });
                    }
                    return EvaluatedType::Unknown;
                }
            };
            if number.fract() == 0_f64 {
                // Unsigned Integers.
                if number >= 0_f64 {
                    if number <= u8::MAX as f64 {
                        return evaluate_index(get_intrinsic!(symboltable.uint8));
                    } else if number <= u16::MAX as f64 {
                        return evaluate_index(get_intrinsic!(symboltable.uint16));
                    } else if number <= u32::MAX as f64 {
                        return evaluate_index(get_intrinsic!(symboltable.uint32));
                    } else if number <= u64::MAX as f64 {
                        return evaluate_index(get_intrinsic!(symboltable.uint64));
                    }
                }
                return evaluate_index(get_intrinsic!(symboltable.int));
            } else {
                return evaluate_index(get_intrinsic!(symboltable.float));
            }
        }
        _ => return evaluate_index(get_intrinsic!(symboltable.int)),
    }
}
