use crate::{
    evaluate, evaluate_bare, evaluate_type_clause, unify_generic_arguments, unify_types,
    DiagnosticType, EvaluatedType, IntermediateType, IntermediateTypeClause, LiteralMap,
    ParameterType, PathIndex, ProgramDiagnostic, ScopeId, SemanticSymbolKind, Supposition,
    SymbolIndex, SymbolLibrary, TypeEnvironment, TypecheckerContext, TypedExpression, UnifyOptions,
};
use ast::LogicOperator;
use errors::{TypeError, TypeErrorType, Warning, WarningType};
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

/// Returns true if an evaluated type is a boolean.
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
            symbollib.int32,
            symbollib.int64,
            symbollib.float64,
            symbollib.float32,
        ].iter().filter_map(|sym| *sym).any(|sym| sym == *model)
    ) || matches!(
        evaluated_type, EvaluatedType::OpaqueTypeInstance { aliased_as, .. }
        if [
            symbollib.number,
            symbollib.float,
            symbollib.int
        ].iter().any(|opaque| opaque.as_ref() == aliased_as.as_ref())
    )
}

// /// Returns true if an evaluated type is unsigned.
// pub fn is_unsigned(evaluated_type: &EvaluatedType, symbollib: &SymbolLibrary) -> bool {
//     matches!(
//         evaluated_type, EvaluatedType::ModelInstance { model, .. }
//         if [
//             symbollib.uint8,
//             symbollib.uint16,
//             symbollib.uint32,
//             symbollib.uint64,
//         ].iter().filter_map(|sym| *sym).any(|sym| sym == *model)
//     ) || matches!(
//         evaluated_type, EvaluatedType::OpaqueTypeInstance {aliased_as, ..}
//         if symbollib.uint.as_ref() == aliased_as.as_ref()
//     )
// }

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
) -> Result<EvaluatedType, TypeErrorType> {
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
            return Err(TypeErrorType::TypeAsValue {
                type_: symbol.name.clone(),
            });
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
            // Get methods from type environments.
            symbollib
                .type_environments
                .iter()
                .filter_map(|environment| {
                    environment
                        .suppositions
                        .iter()
                        .find(|supposition| supposition.base == symbol_idx)
                })
                .map(|supposition| supposition.implementations.iter())
                .flatten()
                .for_each(|interface| {
                    if let EvaluatedType::InterfaceInstance {
                        interface_,
                        generic_arguments,
                        ..
                    } = interface
                    {
                        let mut methods_from_interface =
                            get_method_types_from_symbol(*interface_, symbollib, generic_arguments);
                        method_types.append(&mut methods_from_interface);
                    }
                })
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
        SemanticSymbolKind::Model { interfaces, .. }
        | SemanticSymbolKind::Interface { interfaces, .. } => {
            for implementation in interfaces {
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
            // Get implementations from type environment.
            symbollib
                .type_environments
                .iter()
                .filter_map(|environment| {
                    environment
                        .suppositions
                        .iter()
                        .find(|supposition| supposition.base == symbol_idx)
                })
                .map(|supposition| supposition.implementations.iter())
                .flatten()
                .for_each(|interface| interface_types.push(interface.clone()))
        }
        _ => {}
    }
    return interface_types;
}

/// Extracts the implementation of an interface from an evaluated type,
/// if it exists.
///
/// If the implementation is conditional, it will solve the constaint
/// and return the implementation if the bounds are upheld. Otherwise
/// it will return None.
pub fn get_implementation_of(
    target_interface: SymbolIndex,
    eval_type: &EvaluatedType,
    symbollib: &SymbolLibrary,
) -> Option<EvaluatedType> {
    match eval_type {
        EvaluatedType::ModelInstance { model: base, .. }
        | EvaluatedType::InterfaceInstance {
            interface_: base, ..
        }
        | EvaluatedType::Generic { base }
        | EvaluatedType::HardGeneric { base, .. } => {
            // Every interface is an implementation of itself.
            if eval_type.is_interface_instance() && *base == target_interface {
                return Some(eval_type.clone());
            }
            extract_impl(symbollib, base, eval_type, target_interface)
        }
        EvaluatedType::OpaqueTypeInstance {
            available_interfaces,
            ..
        } => {
            return search_for_interface(available_interfaces, target_interface);
        }
        _ => None,
    }
}

fn search_for_interface(
    available_interfaces: &Vec<EvaluatedType>,
    target_interface: SymbolIndex,
) -> Option<EvaluatedType> {
    for interface in available_interfaces {
        if let EvaluatedType::InterfaceInstance { interface_, .. } = interface {
            if *interface_ == target_interface {
                return Some(interface.clone());
            }
        }
    }
    return None;
}

fn extract_impl(
    symbollib: &SymbolLibrary,
    base: &SymbolIndex,
    eval_type: &EvaluatedType,
    target_interface: SymbolIndex,
) -> Option<EvaluatedType> {
    let base = symbollib.forward(*base);
    let base_symbol = symbollib.get(base)?;
    let implementation_list = match &base_symbol.kind {
        SemanticSymbolKind::Model {
            interfaces: implementations,
            ..
        }
        | SemanticSymbolKind::Interface {
            interfaces: implementations,
            ..
        }
        | SemanticSymbolKind::GenericParameter {
            interfaces: implementations,
            ..
        } => implementations,
        _ => return None,
    };
    let empty = vec![];
    let generic_arguments = match eval_type {
        EvaluatedType::ModelInstance {
            generic_arguments, ..
        } => generic_arguments,
        EvaluatedType::InterfaceInstance {
            generic_arguments, ..
        } => generic_arguments,
        _ => &empty,
    };
    for implementation in implementation_list {
        let evaluated = match implementation {
            IntermediateType::BoundConstraintType {
                consequent, clause, ..
            } => {
                let consequent = evaluate(&consequent, symbollib, None, &mut None, 0);
                let clause_is_valid =
                    evaluate_type_clause(clause, symbollib, Some(generic_arguments), &mut None, 0)
                        .unwrap_or_default();
                if clause_is_valid {
                    consequent
                } else {
                    continue;
                }
            }
            _ => evaluate(implementation, symbollib, None, &mut None, 0),
        };
        if let EvaluatedType::InterfaceInstance { interface_, .. } = &evaluated {
            if *interface_ == target_interface {
                return Some(evaluated);
            }
        }
    }
    // Check in type environments.
    let implementation_list_from_env = symbollib
        .type_environments
        .iter()
        .filter_map(|environment| {
            environment
                .suppositions
                .iter()
                .find(|supposition| supposition.base == base)
        })
        .map(|supposition| supposition.implementations.iter())
        .flatten();
    for evaluated in implementation_list_from_env {
        if let EvaluatedType::InterfaceInstance { interface_, .. } = &evaluated {
            if *interface_ == target_interface {
                return Some(evaluated.clone());
            }
        }
    }
    return None;
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
            &IntermediateType::SimpleType {
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
                if number > i64::MAX as f64 {
                    return evaluate_index(get_intrinsic!(symbollib.bigint));
                }
                // Integers.
                if number > i32::MAX as f64 {
                    return evaluate_index(get_intrinsic!(symbollib.int64));
                }
                return evaluate_index(get_intrinsic!(symbollib.int32));
            } else {
                return evaluate_index(get_intrinsic!(symbollib.float64));
            }
        }
        _ => return evaluate_index(get_intrinsic!(symbollib.float64)),
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
            let function_symbol = symbollib.get_forwarded(*function).unwrap();
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
                            let parameter_symbol = symbollib.get_forwarded(*param).unwrap();
                            let (is_optional, type_label, inferred_type) = match &parameter_symbol
                                .kind
                            {
                                SemanticSymbolKind::Parameter {
                                    is_optional,
                                    param_type,
                                    inferred_type,
                                    ..
                                } => (
                                    *is_optional,
                                    param_type,
                                    (!inferred_type.is_unknown()).then(|| inferred_type.clone()),
                                ),
                                _ => {
                                    unreachable!("Expected param, got {parameter_symbol:?}")
                                }
                            };
                            let compute_from_label = || {
                                type_label.as_ref().map(|typ| {
                                    evaluate(typ, symbollib, Some(generic_arguments), &mut None, 0)
                                })
                            };
                            ParameterType {
                                name: parameter_symbol.name.clone(),
                                is_optional,
                                type_label: type_label.clone(),
                                inferred_type: inferred_type
                                    .or_else(compute_from_label)
                                    .unwrap_or(EvaluatedType::Unknown),
                            }
                        })
                        .collect::<Vec<_>>();
                    let return_type = return_type
                        .as_ref()
                        .map(|typ| evaluate(typ, symbollib, None, &mut None, 0))
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

/// Ensures that the type of value assigned to a variable is valid.
/// Void types and Partial types cannot be assigned.
pub fn ensure_assignment_validity(
    inference_result: &EvaluatedType,
    checker_ctx: &mut TypecheckerContext<'_>,
    span: ast::Span,
) {
    if inference_result.is_void() {
        checker_ctx.add_error(errors::void_assignment(span));
    } else if inference_result.is_partial() {
        checker_ctx.add_error(errors::partial_type_assignment(span));
    }
}

/// Takes a bounded type clause and creates an environment that supposes it is true.
///
/// It takes the checker context so it can:
/// - error for unsatisfiable constraints. e.g.`String implements Number`,
/// - produce warnings for redundant constraints e.g. `T is T`
pub fn assume_clause_verity(
    clause: &IntermediateTypeClause,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &SymbolLibrary,
    span: ast::Span,
    id: ScopeId,
) -> Option<TypeEnvironment> {
    // Before creating an environment, we check if the clause is currently true.
    // If it is, this implies that it has always been true and it will always remain true
    // (at least for the current environment), making it redundant to create an environment for.
    let clause_is_true =
        evaluate_type_clause(clause, symbollib, None, &mut checker_ctx.tracker(span), 0)
            .unwrap_or_default();
    if clause_is_true {
        checker_ctx.add_warning(Warning {
            span,
            warning_type: WarningType::RedundantConstraint,
        });
        return None;
    }
    let mut suppositions: Vec<Supposition> = vec![];
    match typecheck_clause(clause, &mut suppositions, symbollib, checker_ctx, span) {
        Ok(supposition) => {
            if let Some(last) = supposition {
                suppositions.push(last);
            }
        }
        Err(errors) => {
            for _type in errors {
                checker_ctx.add_error(TypeError { _type, span })
            }
        }
    }
    Some(TypeEnvironment { id, suppositions })
}

/// Confirms that a clause can be made true, given previous type environments and suppositions.
/// The reason why it returns Option<Option<Supposition>> is because there are situations
/// where the check fails (meaning it returns None), situations where the check passes but without
/// a new supposition value (Some(None)), and situations where new suppositions are produced (Some(Some(s))).
fn typecheck_clause(
    clause: &IntermediateTypeClause,
    suppositions: &mut Vec<Supposition>,
    symbollib: &SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
    span: ast::Span,
) -> Result<Option<Supposition>, Vec<TypeErrorType>> {
    // Unsatisfiable constraints include:
    // - Any clause with a non-generic base that is not true. e.g. String implements Try.
    // - Any clause that contradicts a previously established truth, such as:
    //      - Divergent implementations of the same interface e.g. T implements U<W> and T implements U<V>.
    //      - An implementation of an interface that is not convergent with the implementation of that same interface on the base.
    //      - Implementation of two interfaces with conflicting methods.
    match clause {
        IntermediateTypeClause::Binary {
            left,
            operator,
            right,
        } => {
            let left = typecheck_clause(&left, suppositions, symbollib, checker_ctx, span)?;
            let right = typecheck_clause(&right, suppositions, symbollib, checker_ctx, span)?;
            match operator {
                LogicOperator::And | LogicOperator::AndLiteral => {
                    match (left, right) {
                        (Some(left), Some(right)) if left.base == right.base => {
                            return left.and(right, symbollib).map(|s| Some(s));
                        }
                        (left, right) => {
                            // If the bases are not the same, there is nothing to do.
                            left.map(|s| suppositions.push(s));
                            right.map(|s| suppositions.push(s));
                            return Ok(None);
                        }
                    }
                }
                LogicOperator::Or | LogicOperator::OrLiteral => {
                    match (left, right) {
                        (Some(left), Some(right)) if left.base == right.base => {
                            return left.or(right, symbollib).map(|s| Some(s));
                        }
                        (left, right) => {
                            // If the bases are not the same, there is nothing to do.
                            left.map(|s| suppositions.push(s));
                            right.map(|s| suppositions.push(s));
                            return Ok(None);
                        }
                    }
                }
            }
        }
        IntermediateTypeClause::Implements { base, interfaces } => {
            let base = *base;
            // if the base is not generic, the clause can be evaluated directly.
            if !symbollib
                .get(base)
                .is_some_and(|symbol| symbol.kind.is_generic_parameter())
            {
                if !evaluate_type_clause(clause, symbollib, None, &mut None, 0).unwrap_or_default()
                {
                    return Err(vec![TypeErrorType::UnsatisfiableConstraint]);
                }
                return Ok(None);
            }
            // the code for Supposition.and has already been written, so I can take
            // "T implements E + F + G" to equal "T implements E and T implements F and T implements G"
            let interfaces = interfaces
                .iter()
                .map(|intermediate_interface| evaluate_bare(intermediate_interface, symbollib))
                .collect::<Vec<_>>();
            // We start by removing the previous supposition for T (if it exists).
            let mut main_supposition = suppositions
                .iter()
                .enumerate()
                .find(|(_, s)| s.base == base)
                .map(|(idx, _)| idx)
                .map(|idx| suppositions.remove(idx))
                .unwrap_or_else(|| Supposition::new(base));
            for interface in interfaces {
                main_supposition = Supposition::from_implementation(base, interface, symbollib)
                    .and_then(move |next_supposition| {
                        main_supposition.and(next_supposition, symbollib)
                    })
                    .unwrap_or_else(|errors| {
                        checker_ctx.add_error(TypeError {
                            _type: TypeErrorType::UnsatisfiableConstraint,
                            span,
                        });
                        for _type in errors {
                            checker_ctx.add_error(TypeError { _type, span })
                        }
                        Supposition::new(base)
                    });
            }
            return Ok(Some(main_supposition));
        }
    }
}

/// Mutates the type of previous expression based on the solved generics.
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
            let ident_symbol = symbollib.get_forwarded(ident.value).unwrap();
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
