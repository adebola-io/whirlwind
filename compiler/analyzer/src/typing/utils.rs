use crate::{
    evaluate, unify_generic_arguments, unify_types, DiagnosticType, EvaluatedType, LiteralMap,
    ParameterType, PathIndex, ProgramDiagnostic, SemanticSymbolKind, SymbolIndex, SymbolLibrary,
    TypecheckerContext, TypedExpression, UnifyOptions,
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

/// Checks that a symbol is not unused.
pub fn check_usage(symbol: &crate::SemanticSymbol, checker_ctx: &mut TypecheckerContext<'_>) {
    // Unused symbols.
    if !symbol.kind.is_public()
        && symbol.references.len() == 1
        && symbol.references[0].starts.len() == 1
    {
        match symbol.kind {
            SemanticSymbolKind::Import {
                source: Some(_), ..
            } => {
                let span = symbol.ident_span();
                let name = symbol.name.to_owned();
                checker_ctx.diagnostics.push(ProgramDiagnostic {
                    offending_file: checker_ctx.path_idx,
                    _type: DiagnosticType::Warning(errors::unused_import_symbol(name, span)),
                })
            }
            SemanticSymbolKind::Model { .. } => {
                let span = symbol.ident_span();
                let name = symbol.name.to_owned();
                checker_ctx.diagnostics.push(ProgramDiagnostic {
                    offending_file: checker_ctx.path_idx,
                    _type: DiagnosticType::Warning(errors::unused_model_symbol(name, span)),
                })
            }
            _ => {}
        }
    }
}

/// Encloses an evaluated type as a prospect.
/// It does nothing if the intrinsic Prospect type is unreachable.
pub fn prospectify(typ: EvaluatedType, symbollib: &SymbolLibrary) -> EvaluatedType {
    if let Some(model) = symbollib.prospect {
        let prospect_symbol = symbollib.get(model).unwrap();
        let prospect_generic_parameter = match &prospect_symbol.kind {
            SemanticSymbolKind::Model { generic_params, .. } => generic_params[0],
            _ => unreachable!(),
        };
        return EvaluatedType::ModelInstance {
            model,
            generic_arguments: vec![(prospect_generic_parameter, typ)],
            is_invariant: false,
        };
    } else {
        return typ;
    }
}

/// Encloses an evaluated type as a range.
/// It does nothing if the intrinsic Range type is unreachable.
pub fn rangify(typ: EvaluatedType, symbollib: &SymbolLibrary) -> EvaluatedType {
    if let Some(model) = symbollib.range {
        let range_symbol = symbollib.get(model).unwrap();
        let range_generic_parameter = match &range_symbol.kind {
            SemanticSymbolKind::Model { generic_params, .. } => generic_params[0],
            _ => unreachable!(),
        };
        return EvaluatedType::ModelInstance {
            model,
            generic_arguments: vec![(range_generic_parameter, typ)],
            is_invariant: false,
        };
    } else {
        return typ;
    }
}

pub fn is_boolean(evaluated_type: &EvaluatedType, symbollib: &SymbolLibrary) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if symbollib.bool.is_some_and(|prospect| prospect == *model)
    )
}

/// Returns true if an evaluated type is a prospect.
pub fn is_prospective_type(evaluated_type: &EvaluatedType, symbollib: &SymbolLibrary) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if symbollib.prospect.is_some_and(|prospect| prospect == *model)
    )
}

/// Returns true if the evaluated type is a maybe.
pub fn is_maybe_type(evaluated_type: &EvaluatedType, symbollib: &SymbolLibrary) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if symbollib.maybe.is_some_and(|maybe| maybe == *model)
    )
}

/// Returns true if an evaluated type is a number.
pub fn is_numeric_type(evaluated_type: &EvaluatedType, symbollib: &SymbolLibrary) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if [
            symbollib.float32,
            symbollib.float64,
            symbollib.uint8,
            symbollib.uint16,
            symbollib.uint32,
            symbollib.uint64,
            symbollib.sint,
            symbollib.bigint,
        ].iter().filter_map(|sym| *sym).any(|sym| sym == *model)
    ) || matches!(
        evaluated_type, EvaluatedType::OpaqueTypeInstance {aliased_as, ..}
        if [
            symbollib.float,
            symbollib.int,
            symbollib.uint
        ].iter().any(|sym| sym.as_ref() == aliased_as.as_ref())
    )
}

/// Returns true if an evaluated type is unsigned.
pub fn is_unsigned(evaluated_type: &EvaluatedType, symbollib: &SymbolLibrary) -> bool {
    matches!(
        evaluated_type, EvaluatedType::ModelInstance { model, .. }
        if [
            symbollib.uint8,
            symbollib.uint16,
            symbollib.uint32,
            symbollib.uint64,
        ].iter().filter_map(|sym| *sym).any(|sym| sym == *model)
    ) || matches!(
        evaluated_type, EvaluatedType::OpaqueTypeInstance {aliased_as, ..}
        if symbollib.uint.as_ref() == aliased_as.as_ref()
    )
}

/// Checks if an expression type can be modified in retrospect.
pub fn is_updateable(expression: &TypedExpression, symbollib: &SymbolLibrary) -> bool {
    match expression {
        TypedExpression::Identifier(ident) => symbollib.get(ident.value).is_some_and(|symbol| {
            matches!(&symbol.kind, SemanticSymbolKind::Variable { declared_type,.. } if declared_type.is_none())
        }),
        TypedExpression::BinaryExpr(b) => {
            is_updateable(&b.left, symbollib) && is_updateable(&b.right, symbollib)},
        TypedExpression::Literal(_) => true,
        _ => false,
    }
}

/// Encloses an evaluated type as a Maybe.
/// It does nothing if the intrinsic Maybe type is unreachable.
pub fn maybify(typ: EvaluatedType, symbollib: &SymbolLibrary) -> EvaluatedType {
    if let Some(model) = symbollib.maybe {
        let maybe_symbol = symbollib.get(model).unwrap();
        let maybe_generic_parameter = match &maybe_symbol.kind {
            SemanticSymbolKind::Model { generic_params, .. } => generic_params[0],
            _ => unreachable!(),
        };
        return EvaluatedType::ModelInstance {
            model,
            generic_arguments: vec![(maybe_generic_parameter, typ)],
            is_invariant: false,
        };
    } else {
        return typ;
    }
}

/// Encloses an evaluated type in an array.
/// It does nothing if the intrinsic Array type is unreachable.
pub fn arrify(typ: EvaluatedType, symbollib: &SymbolLibrary) -> EvaluatedType {
    if let Some(model) = symbollib.array {
        let maybe_symbol = symbollib.get(model).unwrap();
        let maybe_generic_parameter = match &maybe_symbol.kind {
            SemanticSymbolKind::Model { generic_params, .. } => generic_params[0],
            _ => return EvaluatedType::Unknown,
        };
        return EvaluatedType::ModelInstance {
            model,
            generic_arguments: vec![(maybe_generic_parameter, typ)],
            is_invariant: false,
        };
    } else {
        return typ;
    }
}

/// Returns true if a type is evaluated to an array.
pub fn is_array(typ: &EvaluatedType, symbollib: &SymbolLibrary) -> bool {
    match symbollib.array {
        Some(array_symbol) => {
            matches!(typ, EvaluatedType::ModelInstance { model,.. } if *model == array_symbol)
        }
        None => false,
    }
}

/// Coerce generics (and possibly interface types) in a type according to a generic list.
pub fn coerce(typ: EvaluatedType, args: &Vec<(SymbolIndex, EvaluatedType)>) -> EvaluatedType {
    match typ {
        EvaluatedType::ModelInstance {
            model,
            generic_arguments: old_generic_arguments,
            is_invariant,
        } => {
            let mut generic_arguments = vec![];
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::ModelInstance {
                model,
                generic_arguments,
                is_invariant,
            }
        }
        EvaluatedType::InterfaceInstance {
            interface_,
            generic_arguments: old_generic_arguments,
            is_invariant,
        } => {
            let mut generic_arguments = vec![];
            // Interfaces are coercible too because of the "This" type.
            if let Some(newtype) = args.iter().find(|(a, _)| *a == interface_) {
                return newtype.1.clone();
            }
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::InterfaceInstance {
                interface_,
                generic_arguments,
                is_invariant,
            }
        }
        EvaluatedType::EnumInstance {
            enum_,
            generic_arguments: old_generic_arguments,
            is_invariant,
        } => {
            let mut generic_arguments = vec![];
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::EnumInstance {
                enum_,
                generic_arguments,
                is_invariant,
            }
        }
        EvaluatedType::FunctionInstance {
            function,
            generic_arguments: old_generic_arguments,
            is_invariant,
        } => {
            let mut generic_arguments = vec![];
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::FunctionInstance {
                function,
                generic_arguments,
                is_invariant,
            }
        }
        EvaluatedType::FunctionExpressionInstance {
            is_async,
            params: old_params,
            return_type: old_return_type,
            generic_args: old_generic_args,
            is_invariant,
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
                is_invariant,
            }
        }
        EvaluatedType::MethodInstance {
            method,
            generic_arguments: old_generic_arguments,
            is_invariant,
        } => {
            let mut generic_arguments = vec![];
            for (argument, old_type) in old_generic_arguments {
                generic_arguments.push((argument, coerce(old_type, args)));
            }
            EvaluatedType::MethodInstance {
                method,
                generic_arguments,
                is_invariant,
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
    harden: bool,
) -> Vec<(SymbolIndex, EvaluatedType)> {
    generic_params
        .iter()
        .map(|idx| {
            (
                *idx,
                if harden {
                    EvaluatedType::HardGeneric { base: *idx }
                } else {
                    EvaluatedType::Generic { base: *idx }
                },
            )
        })
        .collect()
}

/// Convert a symbol to its inferred type.
pub fn symbol_to_type(
    symbol: &crate::SemanticSymbol,
    name: SymbolIndex,
    symbollib: &SymbolLibrary,
) -> Result<EvaluatedType, Result<EvaluatedType, TypeErrorType>> {
    let eval_type = match &symbol.kind {
        SemanticSymbolKind::Module { .. } => EvaluatedType::Module(name),
        SemanticSymbolKind::Interface { .. } => EvaluatedType::Interface(name),
        SemanticSymbolKind::Model { .. } => EvaluatedType::Model(name),
        SemanticSymbolKind::Enum { .. } => EvaluatedType::Enum(name),
        SemanticSymbolKind::Variant { owner_enum, .. } => EvaluatedType::EnumInstance {
            enum_: *owner_enum,
            generic_arguments: {
                // Try to create a space for unknown enum generics.
                // todo: unify from tagged types.
                let enum_symbol = symbollib.get_forwarded(*owner_enum).unwrap();
                match &enum_symbol.kind {
                    SemanticSymbolKind::Enum { generic_params, .. } => {
                        evaluate_generic_params(generic_params, false)
                    }
                    _ => vec![],
                }
            },
            is_invariant: false,
        },
        SemanticSymbolKind::Variable { inferred_type, .. } => inferred_type.clone(),
        SemanticSymbolKind::Constant { inferred_type, .. } => inferred_type.clone(),
        SemanticSymbolKind::LoopVariable { inferred_type, .. } => inferred_type.clone(),
        SemanticSymbolKind::Parameter {
            is_optional,
            param_type,
            inferred_type,
            ..
        } => {
            let inferred_type = param_type
                .as_ref()
                .map(|param_type| evaluate(param_type, symbollib, None, &mut None, 0))
                .unwrap_or(inferred_type.clone());
            if *is_optional {
                maybify(inferred_type, symbollib)
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
            generic_arguments: evaluate_generic_params(generic_params, false),
            is_invariant: false,
        }, //TODO
        _ => EvaluatedType::Unknown,
    };
    Ok(eval_type)
}

/// Extract the available methods as evaluated types from a model, interface or generic.
pub fn get_method_types_from_symbol<'a>(
    symbol_idx: SymbolIndex,
    symbollib: &'a SymbolLibrary,
    generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
) -> Vec<(&'a String, EvaluatedType, bool)> {
    let mut method_types = vec![];
    let symbol = symbollib.get(symbol_idx);
    if symbol.is_none() {
        return vec![];
    }
    let symbol = symbol.unwrap();
    match &symbol.kind {
        SemanticSymbolKind::Model { methods, .. }
        | SemanticSymbolKind::Interface { methods, .. } => {
            for method in methods {
                let method = *method;
                let method_symbol = symbollib.get(method);
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
                    generic_arguments: evaluate_generic_params(generic_params, false),
                    is_invariant: false,
                };
                let coerced = coerce(initial_type, generic_arguments);
                method_types.push((&method_symbol.name, coerced, method_symbol.kind.is_public()));
            }
        }
        SemanticSymbolKind::GenericParameter { interfaces, .. } => {
            for int_typ in interfaces {
                let evaled = evaluate(int_typ, symbollib, Some(generic_arguments), &mut None, 0);
                if let EvaluatedType::InterfaceInstance {
                    interface_,
                    generic_arguments,
                    ..
                } = evaled
                {
                    let mut methods_from_interface =
                        get_method_types_from_symbol(interface_, symbollib, &generic_arguments);
                    method_types.append(&mut methods_from_interface);
                }
            }
        }
        _ => {}
    }
    return method_types;
}

/// Extract all available interfaces as evaluated types from a model, interface or generic.
pub fn get_interface_types_from_symbol(
    symbol_idx: SymbolIndex,
    symbollib: &SymbolLibrary,
    generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
) -> Vec<EvaluatedType> {
    let mut interface_types = vec![];
    let symbol = symbollib.get(symbol_idx);
    if symbol.is_none() {
        return vec![];
    }
    let symbol = symbol.unwrap();
    match &symbol.kind {
        SemanticSymbolKind::Model {
            implementations, ..
        }
        | SemanticSymbolKind::Interface {
            implementations, ..
        } => {
            for implementation in implementations {
                let initial_type = evaluate(
                    implementation,
                    symbollib,
                    Some(generic_arguments),
                    &mut None,
                    0,
                );
                if !initial_type.is_interface_instance() {
                    continue;
                }
                let coerced = coerce(initial_type, generic_arguments); // todo: is coercion still necessary?
                interface_types.push(coerced);
            }
        }
        SemanticSymbolKind::GenericParameter { interfaces, .. } => {
            for int_typ in interfaces {
                let evaled = evaluate(int_typ, symbollib, Some(generic_arguments), &mut None, 0);
                if let EvaluatedType::InterfaceInstance {
                    interface_,
                    generic_arguments,
                    ..
                } = &evaled
                {
                    interface_types.push(evaled.clone());
                    let mut interfaces_from_interface =
                        get_interface_types_from_symbol(*interface_, symbollib, &generic_arguments);
                    interface_types.append(&mut interfaces_from_interface);
                }
            }
        }
        _ => {}
    }
    return interface_types;
}

/// Get an implementation of a interface from an evaluated type, if it exists.
pub fn get_implementation_of(
    target_interface: SymbolIndex,
    operand_type: &EvaluatedType,
    symbollib: &SymbolLibrary,
) -> Option<EvaluatedType> {
    match operand_type {
        EvaluatedType::ModelInstance { model: base, .. }
        | EvaluatedType::InterfaceInstance {
            interface_: base, ..
        }
        | EvaluatedType::Generic { base }
        | EvaluatedType::HardGeneric { base } => {
            let base_symbol = symbollib.get(*base)?;
            let implementation_list = match &base_symbol.kind {
                SemanticSymbolKind::Model {
                    implementations, ..
                }
                | SemanticSymbolKind::Interface {
                    implementations, ..
                }
                | SemanticSymbolKind::GenericParameter {
                    interfaces: implementations,
                    ..
                } => implementations,
                _ => return None,
            };
            for implementation in implementation_list {
                let evaluated = evaluate(implementation, symbollib, None, &mut None, 0);
                if let EvaluatedType::InterfaceInstance { interface_, .. } = &evaluated {
                    if *interface_ == target_interface {
                        return Some(evaluated);
                    }
                }
            }
            return None;
        }
        EvaluatedType::OpaqueTypeInstance {
            available_interfaces,
            ..
        } => {
            for interface in available_interfaces {
                if let EvaluatedType::InterfaceInstance { interface_, .. } = interface {
                    if *interface_ == target_interface {
                        return Some(interface.clone());
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
    symbollib: &SymbolLibrary,
    value: &ast::WhirlNumber,
    checker_ctx: Option<(&mut Vec<ProgramDiagnostic>, PathIndex)>,
) -> EvaluatedType {
    let evaluate_index = |value| {
        evaluate(
            &crate::IntermediateType::SimpleType {
                value,
                generic_args: vec![],
                span: ast::Span::default(),
            },
            symbollib,
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
                        ctx.0.push(ProgramDiagnostic {
                            offending_file: ctx.1,
                            _type: DiagnosticType::Error(crate::Error::Typing(TypeError {
                                _type: TypeErrorType::NumericConversionError {
                                    error: error.to_string(),
                                },
                                span: value.span,
                            })),
                        });
                    }
                    return EvaluatedType::Unknown;
                }
            };
            if number.fract() == 0_f64 {
                // Unsigned Integers.
                if number >= 0_f64 {
                    if number <= u8::MAX as f64 {
                        return evaluate_index(get_intrinsic!(symbollib.uint8));
                    } else if number <= u16::MAX as f64 {
                        return evaluate_index(get_intrinsic!(symbollib.uint16));
                    } else if number <= u32::MAX as f64 {
                        return evaluate_index(get_intrinsic!(symbollib.uint32));
                    } else if number <= u64::MAX as f64 {
                        return evaluate_index(get_intrinsic!(symbollib.uint64));
                    }
                }
                return evaluate_index(get_intrinsic!(symbollib.int));
            } else {
                return evaluate_index(get_intrinsic!(symbollib.float));
            }
        }
        _ => return evaluate_index(get_intrinsic!(symbollib.int)),
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionType<'a> {
    pub is_async: bool,
    pub parameter_types: Vec<ParameterType>,
    pub generic_arguments: &'a Vec<(SymbolIndex, EvaluatedType)>,
    pub return_type: EvaluatedType,
}

/// Reduces a functional evaluated type to its components
/// It returns None if the type passed in is not functional.
pub fn distill_as_function_type<'a>(
    caller: &'a EvaluatedType,
    symbollib: &SymbolLibrary,
) -> Option<FunctionType<'a>> {
    match caller {
        EvaluatedType::MethodInstance {
            method: function,
            generic_arguments,
            ..
        }
        | EvaluatedType::FunctionInstance {
            function,
            generic_arguments,
            ..
        } => {
            let function_symbol = symbollib.get(*function).unwrap();
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
                    let is_async = *is_async;
                    let parameter_types = params
                        .iter()
                        .map(|param| {
                            let parameter_symbol = symbollib.get(*param).unwrap();
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
                                            symbollib,
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
                        .map(|typ| evaluate(typ, symbollib, Some(&generic_arguments), &mut None, 0))
                        .unwrap_or(EvaluatedType::Void);
                    Some(FunctionType {
                        is_async,
                        parameter_types,
                        generic_arguments,
                        return_type,
                    })
                }
                _ => unreachable!("Expected functional symbol but found {:?}", function_symbol),
            }
        }
        EvaluatedType::FunctionExpressionInstance {
            is_async,
            params,
            return_type,
            generic_args,
            ..
        } => {
            let is_async = *is_async;
            let parameter_types = params.clone();
            let return_type = *return_type.clone();
            let generic_arguments = generic_args;
            Some(FunctionType {
                is_async,
                parameter_types,
                generic_arguments,
                return_type,
            })
        }
        _ => None,
    }
}

/// When designing Whirlwind, support for type inferencing was added,
/// which turns out to be more work than previously expected.
/// Checking types in the other direction requires a whole new visitor, which is
/// unecessary if the types to infer are minimal.
///
/// The purpose of this function is to "suggest" a type for expressions and parameters
/// that have unknown types, before they are properly typechecked.
/// It is most useful in anonymous function parameters without type labels, where
/// there is a left hand side type available, such as in calls, assignments and returns.
pub fn infer_ahead(
    expression: &mut TypedExpression,
    target_type: &EvaluatedType,
    symbollib: &mut SymbolLibrary,
) {
    match expression {
        TypedExpression::FnExpr(function_expr) => {
            if let Some(functiontype) = distill_as_function_type(target_type, symbollib) {
                // Infer parameters.
                for (index, param_idx) in function_expr.params.iter().enumerate() {
                    let shadow_type_is_maybe =
                        functiontype
                            .parameter_types
                            .get(index)
                            .is_some_and(|shadow_type| {
                                shadow_type.is_optional
                                    || is_maybe_type(&shadow_type.inferred_type, symbollib)
                            });
                    let parameter_symbol = symbollib.get_mut(*param_idx).unwrap();
                    if let SemanticSymbolKind::Parameter {
                        param_type,
                        is_optional,
                        inferred_type,
                        ..
                    } = &mut parameter_symbol.kind
                    {
                        let shadow_type = match functiontype.parameter_types.get(index) {
                            Some(shadow) => shadow,
                            None => break,
                        };
                        let is_optional = *is_optional;
                        let optionality_is_equal =
                            (shadow_type.is_optional == is_optional) || (shadow_type_is_maybe);

                        if param_type.is_none() && optionality_is_equal {
                            *inferred_type = match functiontype.parameter_types.get(index) {
                                Some(param_type) => param_type.inferred_type.clone(),
                                None => break,
                            }
                        }
                    };
                }
                // Infer return type.
                infer_ahead(
                    &mut function_expr.body,
                    &functiontype.return_type,
                    symbollib,
                );
            }
        }
        _ => {} // todo: what else needs inference?
    }
}

pub fn ensure_assignment_validity(
    inference_result: &EvaluatedType,
    checker_ctx: &mut TypecheckerContext<'_>,
    span: ast::Span,
) {
    if inference_result.is_void() {
        checker_ctx.add_diagnostic(errors::void_assignment(span));
    } else if inference_result.is_partial() {
        checker_ctx.add_diagnostic(errors::partial_type_assignment(span));
    }
}

/// Mutates the type of an expression based on the solved generics.
pub fn update_expression_type(
    caller: &mut TypedExpression,
    symbollib: &mut SymbolLibrary,
    literals: &mut LiteralMap,
    generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    // In case the types match each other directly, to prevent double nesting.
    optional_type: Option<&EvaluatedType>,
) {
    match caller {
        TypedExpression::Identifier(ident) => {
            // DOUBLED SO SYMBOLLIB CAN BE BORROWED IMMUTABLY.
            let mut unified_type = None;
            let ident_symbol = symbollib.get(ident.value).unwrap();
            if let SemanticSymbolKind::Variable {
                inferred_type,
                declared_type,
                ..
            } = &ident_symbol.kind
            {
                // Identifiers with declared types cannot be updated.
                if declared_type.is_some() {
                    return;
                }
                unified_type = optional_type.and_then(|opt_type| {
                    let mut new_type = unify_types(
                        inferred_type,
                        opt_type,
                        symbollib,
                        UnifyOptions::Conform,
                        None,
                    )
                    .ok();
                    // Numeric types should be upgraded when they are updated.
                    if new_type.is_none() && is_numeric_type(inferred_type, symbollib) {
                        new_type = unify_types(
                            opt_type,
                            inferred_type,
                            symbollib,
                            UnifyOptions::Conform,
                            None,
                        )
                        .ok();
                    }
                    new_type
                });
            }

            let ident_symbol = symbollib.get_mut(ident.value).unwrap();
            if let SemanticSymbolKind::Variable { inferred_type, .. } = &mut ident_symbol.kind {
                if let Some(unified_type) = unified_type {
                    *inferred_type = unified_type;
                }
                let mut empty = vec![];
                if inferred_type.contains_child_for_which(&|child| {
                    matches!(child, EvaluatedType::Generic { .. })
                }) {
                    let inferred_type_generics = get_type_generics_mut(inferred_type, &mut empty);
                    for (generic_idx, generic_solution) in inferred_type_generics {
                        if let Some((_, newsolution)) = generic_arguments
                            .iter()
                            .rev()
                            .find(|(a, _)| a == generic_idx)
                        {
                            if !newsolution.is_unknown() {
                                *generic_solution = newsolution.clone();
                            }
                        }
                    }
                }
            }
        }
        TypedExpression::CallExpr(call) => {
            update_expression_type(
                &mut call.caller,
                symbollib,
                literals,
                generic_arguments,
                optional_type,
            );
            call.arguments.iter_mut().for_each(|arg| {
                update_expression_type(arg, symbollib, literals, generic_arguments, optional_type)
            });
        }
        TypedExpression::AccessExpr(access) => {
            let empty = LiteralMap::new();
            let inferred_type = symbollib
                .get_expression_type(&access.property, &empty)
                .unwrap();
            let empty = vec![];
            let prior_generics = get_type_generics(&inferred_type, &empty);
            let generic_args = match unify_generic_arguments(
                prior_generics,
                generic_arguments,
                symbollib,
                UnifyOptions::None,
                None,
            ) {
                Ok(generics) => generics,
                _ => return,
            };
            update_expression_type(
                &mut access.object,
                symbollib,
                literals,
                &generic_args,
                optional_type,
            );
        }
        TypedExpression::ArrayExpr(array) => {
            if optional_type.is_some_and(|optional_type| is_array(optional_type, symbollib)) {
                let optional_type = optional_type.map(|opt_type| match opt_type {
                    EvaluatedType::ModelInstance {
                        generic_arguments, ..
                    } => &generic_arguments[0].1,
                    _ => opt_type,
                });
                array.elements.iter_mut().for_each(|exp| {
                    update_expression_type(
                        exp,
                        symbollib,
                        literals,
                        generic_arguments,
                        optional_type,
                    )
                })
            }
            array.elements.iter_mut().for_each(|exp| {
                update_expression_type(exp, symbollib, literals, generic_arguments, optional_type)
            });
        }
        TypedExpression::IndexExpr(index) => {
            update_expression_type(
                &mut index.object,
                symbollib,
                literals,
                generic_arguments,
                optional_type,
            );
            // todo: allow index overloading?
        }
        TypedExpression::UnaryExpr(unary) => update_expression_type(
            &mut unary.operand,
            symbollib,
            literals,
            generic_arguments,
            optional_type,
        ),
        TypedExpression::UpdateExpr(exp) => update_expression_type(
            &mut exp.operand,
            symbollib,
            literals,
            generic_arguments,
            optional_type,
        ),
        TypedExpression::BinaryExpr(binexp) => {
            if is_updateable(&binexp.left, symbollib) {
                update_expression_type(
                    &mut binexp.left,
                    symbollib,
                    literals,
                    generic_arguments,
                    optional_type,
                );
            }
            if is_updateable(&binexp.right, symbollib) {
                update_expression_type(
                    &mut binexp.right,
                    symbollib,
                    literals,
                    generic_arguments,
                    optional_type,
                );
            }
        }
        TypedExpression::Literal(literal) => {
            if let Some(literal) = literals.get_mut(*literal) {
                if let crate::Literal::NumericLiteral {
                    ref mut inferred_type,
                    ..
                } = literal
                {
                    if let Some(optional_type) = optional_type {
                        let left = optional_type;
                        let right = &inferred_type;
                        if let Ok(result) =
                            unify_types(left, right, symbollib, UnifyOptions::Conform, None).or(
                                unify_types(right, left, symbollib, UnifyOptions::Conform, None),
                            )
                        {
                            *inferred_type = result
                        };
                    }
                }
            }
        }
        _ => {}
    }
}

/// Extracts the generic arguments passed to an evaluated type.
pub fn get_type_generics<'a>(
    result_type: &'a EvaluatedType,
    empty: &'a Vec<(SymbolIndex, EvaluatedType)>,
) -> &'a Vec<(SymbolIndex, EvaluatedType)> {
    match result_type {
        EvaluatedType::ModelInstance {
            generic_arguments, ..
        }
        | EvaluatedType::InterfaceInstance {
            generic_arguments, ..
        }
        | EvaluatedType::EnumInstance {
            generic_arguments, ..
        }
        | EvaluatedType::FunctionInstance {
            generic_arguments, ..
        }
        | EvaluatedType::FunctionExpressionInstance {
            generic_args: generic_arguments,
            ..
        }
        | EvaluatedType::OpaqueTypeInstance {
            generic_arguments, ..
        }
        | EvaluatedType::MethodInstance {
            generic_arguments, ..
        } => generic_arguments,
        _ => empty,
    }
}

/// Extracts the generic arguments passed to an evaluated type, mutably.
pub fn get_type_generics_mut<'a>(
    result_type: &'a mut EvaluatedType,
    empty: &'a mut Vec<(SymbolIndex, EvaluatedType)>,
) -> &'a mut Vec<(SymbolIndex, EvaluatedType)> {
    match result_type {
        EvaluatedType::ModelInstance {
            generic_arguments, ..
        }
        | EvaluatedType::InterfaceInstance {
            generic_arguments, ..
        }
        | EvaluatedType::EnumInstance {
            generic_arguments, ..
        }
        | EvaluatedType::FunctionInstance {
            generic_arguments, ..
        }
        | EvaluatedType::FunctionExpressionInstance {
            generic_args: generic_arguments,
            ..
        }
        | EvaluatedType::OpaqueTypeInstance {
            generic_arguments, ..
        }
        | EvaluatedType::MethodInstance {
            generic_arguments, ..
        } => generic_arguments,
        _ => empty,
    }
}

/// Creates an instance of Bool.
pub fn boolean_instance(bool_symbol: SymbolIndex) -> EvaluatedType {
    EvaluatedType::ModelInstance {
        model: bool_symbol,
        is_invariant: false,
        generic_arguments: vec![],
    }
}
