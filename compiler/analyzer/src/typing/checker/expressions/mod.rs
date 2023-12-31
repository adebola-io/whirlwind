mod assignment;
mod binary;

use super::*;
use crate::{
    programdiagnostic::DiagnosticType,
    utils::{is_unsigned, is_updateable},
    Error, IntermediateType,
};
use assignment::typecheck_assignment_expression;
use binary::typecheck_binary_expression;

/// Typechecks an expression.
pub fn typecheck_expression(
    expression: &mut crate::TypedExpression,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    match expression {
        TypedExpression::Identifier(i) => {
            // Ensure that identifiers refer to values, not types.
            match typecheck_identifier(i, symbollib) {
                Ok(evaluated_type) => evaluated_type,
                Err(error_type) => {
                    checker_ctx.add_diagnostic(TypeError {
                        _type: error_type,
                        span: checker_ctx.span_of_expr(expression, &symbollib),
                    });
                    EvaluatedType::Unknown
                }
            }
        }
        TypedExpression::Literal(l) => typecheck_literal(l, checker_ctx, symbollib),
        TypedExpression::NewExpr(newexp) => {
            typecheck_new_expression(&mut *newexp, symbollib, checker_ctx)
        }
        TypedExpression::ThisExpr(this) => typecheck_this_expression(this, symbollib, checker_ctx),
        TypedExpression::CallExpr(c) => typecheck_call_expression(&mut *c, symbollib, checker_ctx),
        TypedExpression::FnExpr(f) => {
            typecheck_function_expression(&mut *f, symbollib, checker_ctx)
        }
        TypedExpression::Block(body) => typecheck_block(body, false, checker_ctx, symbollib),
        TypedExpression::IfExpr(ifexp) => typecheck_if_expression(ifexp, checker_ctx, symbollib),
        TypedExpression::AccessExpr(access) => {
            typecheck_access_expression(&mut *access, symbollib, checker_ctx)
        }
        TypedExpression::ArrayExpr(array) => {
            typecheck_array_expression(array, symbollib, checker_ctx)
        }
        TypedExpression::IndexExpr(indexexp) => {
            typecheck_index_expression(indexexp, symbollib, checker_ctx)
        }
        TypedExpression::BinaryExpr(binexp) => {
            typecheck_binary_expression(binexp, checker_ctx, symbollib)
        }
        TypedExpression::AssignmentExpr(assexp) => {
            typecheck_assignment_expression(assexp, checker_ctx, symbollib)
        }
        TypedExpression::UnaryExpr(unaryexp) => {
            typecheck_unary_expression(unaryexp, symbollib, checker_ctx)
        }
        TypedExpression::LogicExpr(logexp) => {
            typecheck_logic_expression(logexp, checker_ctx, symbollib)
        }
        TypedExpression::UpdateExpr(updateexp) => {
            typecheck_update_expression(updateexp, checker_ctx, symbollib)
        }
    }
}

/// Typechecks an update expression.
fn typecheck_update_expression(
    updateexp: &mut crate::TypedUpdateExpr,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    updateexp.inferred_type = (|| {
        let operand_type = typecheck_expression(&mut updateexp.operand, checker_ctx, symbollib);
        match updateexp.operator {
            // the ! operator.
            // Can only be used by models that implement Guaranteed.
            // It evaluates to the single generic type of the interface.
            ast::UpdateOperator::Assert => {
                if let Some(guaranteed) = symbollib.guaranteed {
                    let guaranteed_generic = match &symbollib.get(guaranteed).unwrap().kind {
                        SemanticSymbolKind::Interface { generic_params, .. } => generic_params[0],
                        _ => return EvaluatedType::Unknown,
                    };
                    // Reference types of models that implement Guarantee, also implement Guarantee.
                    let operand_type_deref = &operand_type;
                    if let Some(implementation) =
                        get_implementation_of(guaranteed, operand_type_deref, &symbollib)
                    {
                        match implementation {
                            EvaluatedType::InterfaceInstance {
                                generic_arguments, ..
                            } => {
                                let generic_solution = generic_arguments
                                    .into_iter()
                                    .find(|generic| generic.0 == guaranteed_generic);
                                if generic_solution.is_none() {
                                    let name = symbollib.format_evaluated_type(&operand_type);
                                    checker_ctx.add_diagnostic(errors::illegal_guarantee(
                                        name,
                                        updateexp.span,
                                    ));
                                    return EvaluatedType::Unknown;
                                }
                                let evaluated_type = generic_solution.unwrap().1;
                                let full_generic_list = match operand_type {
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
                            _ => return EvaluatedType::Unknown,
                        }
                    } else {
                        let name = symbollib.format_evaluated_type(&operand_type);
                        checker_ctx.add_diagnostic(errors::illegal_guarantee(name, updateexp.span));
                        EvaluatedType::Unknown
                    }
                } else {
                    checker_ctx.add_diagnostic(errors::missing_intrinsic(
                        String::from("Guaranteed"),
                        updateexp.span,
                    ));
                    EvaluatedType::Unknown
                }
            }
            // the ? operator.
            // The Try interface has two generics, one for the value to be retreived,
            // and another for the value to be immediately returned.
            ast::UpdateOperator::TryFrom => {
                if let Some(try_idx) = symbollib.try_s {
                    let (first_generic, second_generic) =
                        match &symbollib.get(try_idx).unwrap().kind {
                            SemanticSymbolKind::Interface { generic_params, .. } => {
                                (generic_params[0], generic_params[1])
                            }
                            _ => return EvaluatedType::Unknown,
                        };
                    // Reference types of models that implement Try, also implement Try.
                    let operand_type = &operand_type;
                    let implementation = get_implementation_of(try_idx, operand_type, &symbollib);
                    if implementation.is_none() {
                        let name = symbollib.format_evaluated_type(&operand_type);
                        checker_ctx.add_diagnostic(errors::illegal_try(name, updateexp.span));
                        return EvaluatedType::Unknown;
                    }
                    let implementation = implementation.unwrap();
                    if let EvaluatedType::InterfaceInstance {
                        generic_arguments, ..
                    } = implementation
                    {
                        let first_generic_solution = generic_arguments
                            .iter()
                            .find(|generic| generic.0 == first_generic)
                            .map(|(a, b)| (*a, b.clone()));
                        let second_generic_solution = generic_arguments
                            .into_iter()
                            .find(|generic| generic.0 == second_generic);
                        if first_generic_solution.is_none() || second_generic_solution.is_none() {
                            let name = symbollib.format_evaluated_type(&operand_type);
                            checker_ctx.add_diagnostic(errors::illegal_try(name, updateexp.span));
                            return EvaluatedType::Unknown;
                        }
                        let evaluated_type = first_generic_solution.unwrap().1;
                        let mut returned_type = second_generic_solution.unwrap().1;
                        let empty = vec![];
                        let full_generic_list = match &operand_type {
                            EvaluatedType::InterfaceInstance {
                                generic_arguments, ..
                            }
                            | EvaluatedType::ModelInstance {
                                generic_arguments, ..
                            } => generic_arguments,
                            _ => &empty,
                        };
                        // Confirm that the type slated for return is
                        // compatible with the already stated return type.
                        let function_ctx = checker_ctx.current_function_context.last();
                        returned_type = coerce(returned_type, &full_generic_list);
                        if !returned_type.is_generic()
                            && !returned_type.contains_child_for_which(&|child| {
                                matches!(child, EvaluatedType::HardGeneric { .. })
                            })
                            && function_ctx.is_some_and(|ctx| ctx.is_named)
                        {
                            returned_type =
                                coerce_all_generics(&returned_type, EvaluatedType::Never)
                        }
                        if function_ctx.is_none() {
                            checker_ctx.add_diagnostic(TypeError {
                                _type: TypeErrorType::MismatchedReturnType {
                                    expected: symbollib.format_evaluated_type(&EvaluatedType::Void),
                                    found: symbollib.format_evaluated_type(&returned_type),
                                },
                                span: updateexp.span,
                            });
                            return EvaluatedType::Unknown;
                        }
                        let function_ctx = function_ctx.unwrap();
                        match unify_types(
                            &function_ctx.return_type,
                            &returned_type,
                            &symbollib,
                            UnifyOptions::Return,
                            None,
                        ) {
                            // Unification successful, update function's return type.
                            Ok(result) => {
                                // Cannot assign directly to function_ctx because borrowed as mutable yada yada yada.
                                checker_ctx
                                    .current_function_context
                                    .last_mut()
                                    .unwrap()
                                    .return_type = result;
                            }
                            Err(errors) => {
                                checker_ctx.add_diagnostic(TypeError {
                                    _type: TypeErrorType::MismatchedReturnType {
                                        expected: symbollib
                                            .format_evaluated_type(&function_ctx.return_type),
                                        found: symbollib.format_evaluated_type(&returned_type),
                                    },
                                    span: updateexp.span,
                                });
                                for _type in errors {
                                    checker_ctx.add_diagnostic(TypeError {
                                        _type,
                                        span: updateexp.span,
                                    })
                                }
                            }
                        }
                        coerce(evaluated_type, &full_generic_list)
                    } else {
                        return EvaluatedType::Unknown;
                    }
                } else {
                    checker_ctx.add_diagnostic(errors::missing_intrinsic(
                        String::from("Try"),
                        updateexp.span,
                    ));
                    EvaluatedType::Unknown
                }
            }
        }
    })();
    updateexp.inferred_type.clone()
}

/// Typechecks a unary expression.
fn typecheck_unary_expression(
    unaryexp: &mut crate::TypedUnaryExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
) -> EvaluatedType {
    unaryexp.inferred_type = (|| {
        let operand_type = typecheck_expression(&mut unaryexp.operand, checker_ctx, symbollib);
        match unaryexp.operator {
            UnaryOperator::Negation | UnaryOperator::NegationLiteral => {
                if !is_boolean(&operand_type, symbollib) {
                    let name = symbollib.format_evaluated_type(&operand_type);
                    checker_ctx.add_diagnostic(TypeError {
                        _type: TypeErrorType::NonBooleanLogic { name },
                        span: unaryexp.span,
                    })
                }
                symbollib
                    .bool
                    .map(|boolean| EvaluatedType::ModelInstance {
                        model: boolean,
                        generic_arguments: vec![],
                        is_invariant: false,
                    })
                    .unwrap_or(EvaluatedType::Unknown)
            }
            UnaryOperator::Plus | UnaryOperator::Minus => {
                if is_numeric_type(&operand_type, symbollib) {
                    if is_unsigned(&operand_type, symbollib) {
                        if symbollib.int.is_none() {
                            checker_ctx.add_diagnostic(errors::missing_intrinsic(
                                format!("Int"),
                                unaryexp.span,
                            ));
                            return EvaluatedType::Unknown;
                        }
                        let int = evaluate(
                            &IntermediateType::SimpleType {
                                value: symbollib.int.unwrap(),
                                generic_args: vec![],
                                span: Span::default(),
                            },
                            symbollib,
                            None,
                            &mut None,
                            0,
                        );
                        update_expression_type(
                            &mut unaryexp.operand,
                            symbollib,
                            checker_ctx.literals,
                            &vec![],
                            Some(&int),
                        );
                        return int;
                    }
                    return operand_type;
                } else {
                    let typ = symbollib.format_evaluated_type(&operand_type);
                    checker_ctx
                        .add_diagnostic(errors::numeric_exclusive_operation(typ, unaryexp.span));
                    return EvaluatedType::Unknown;
                }
            }
        }
    })();
    unaryexp.inferred_type.clone()
}

/// Typechecks if expression.
fn typecheck_if_expression(
    ifexp: &mut TypedIfExpr,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    ifexp.inferred_type = {
        let condition_type = typecheck_expression(&mut ifexp.condition, checker_ctx, symbollib);
        if !is_boolean(&condition_type, symbollib) && !condition_type.is_unknown() {
            checker_ctx.add_diagnostic(errors::non_boolean_logic(
                symbollib.format_evaluated_type(&condition_type),
                checker_ctx.span_of_expr(&ifexp.condition, symbollib),
            ));
        }
        // If in a model's constructor, update the scope entered.
        push_scopetype(
            checker_ctx,
            ScopeType::IfBlock {
                id: ifexp.consequent.scopeid,
            },
        );
        let block_type = typecheck_block(&mut ifexp.consequent, false, checker_ctx, symbollib);
        pop_scopetype(checker_ctx);
        if let Some(else_) = &mut ifexp.alternate {
            push_scopetype(
                checker_ctx,
                ScopeType::ElseBlock {
                    id_of_parent_if: ifexp.consequent.scopeid,
                },
            );
            let else_type = typecheck_expression(&mut else_.expression, checker_ctx, symbollib);
            pop_scopetype(checker_ctx);
            match unify_types(
                &block_type,
                &else_type,
                symbollib,
                UnifyOptions::AnyNever,
                None,
            ) {
                Ok(result) => result,
                Err(errors) => {
                    checker_ctx.add_diagnostic(errors::separate_if_types(
                        ifexp.span,
                        symbollib.format_evaluated_type(&block_type),
                        symbollib.format_evaluated_type(&else_type),
                    ));
                    for error in errors {
                        checker_ctx.add_diagnostic(TypeError {
                            _type: error,
                            span: ifexp.span,
                        })
                    }
                    EvaluatedType::Unknown
                }
            }
        } else if block_type.is_void() {
            EvaluatedType::Void
        } else {
            EvaluatedType::Partial {
                types: vec![block_type, EvaluatedType::Void],
            }
        }
    };
    ifexp.inferred_type.clone()
}

/// If the expression passed in refers to an attribute, it returns the symbol index of the attribute.
/// Otherwise it returns none.
fn expression_is_attribute(
    expr: &TypedExpression,
    symbollib: &mut SymbolLibrary,
) -> Option<SymbolIndex> {
    match expr {
        TypedExpression::AccessExpr(accessexp) => match &accessexp.object {
            TypedExpression::ThisExpr(this) => {
                let property_name = match &accessexp.property {
                    TypedExpression::Identifier(ident) => symbollib.get(ident.value)?.name.as_str(),
                    _ => return None,
                };
                let model_or_interface = symbollib.get(this.model_or_interface?)?;
                let attributes = match &model_or_interface.kind {
                    SemanticSymbolKind::Model { attributes, .. } => attributes,
                    _ => return None,
                };
                for attribute_idx in attributes {
                    let attribute_idx = *attribute_idx;
                    let attribute = symbollib.get(attribute_idx)?;
                    if attribute.name == property_name {
                        return Some(attribute_idx);
                    }
                }
                return None;
            }
            _ => return None,
        },
        _ => return None,
    }
}

/// Returns true if the left hand side is a valid assignment target, syntactically.
fn is_valid_lhs(expression: &TypedExpression) -> bool {
    match expression {
        TypedExpression::Identifier(_) | TypedExpression::ThisExpr(_) => true,
        TypedExpression::AccessExpr(accessexp) => {
            is_valid_lhs(&accessexp.object)
                || matches!(accessexp.object, TypedExpression::ThisExpr(_))
        }
        TypedExpression::IndexExpr(indexp) => is_valid_lhs(&indexp.object),
        _ => false,
    }
}

fn typecheck_logic_expression(
    logexp: &mut TypedLogicExpr,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    logexp.inferred_type = (|| {
        let left = typecheck_expression(&mut logexp.left, checker_ctx, symbollib);
        let right = typecheck_expression(&mut logexp.right, checker_ctx, symbollib);

        if !is_boolean(&left, symbollib) && !left.is_unknown() {
            checker_ctx.add_diagnostic(errors::non_boolean_logic(
                symbollib.format_evaluated_type(&left),
                checker_ctx.span_of_expr(&logexp.right, symbollib),
            ));
        }
        if !is_boolean(&right, symbollib) && !right.is_unknown() {
            checker_ctx.add_diagnostic(errors::non_boolean_logic(
                symbollib.format_evaluated_type(&right),
                checker_ctx.span_of_expr(&logexp.right, symbollib),
            ));
        }
        if let Some(boolean_idx) = symbollib.bool {
            return EvaluatedType::ModelInstance {
                model: boolean_idx,
                generic_arguments: vec![],
                is_invariant: false,
            };
        } else {
            checker_ctx.add_diagnostic(errors::missing_intrinsic(format!("Bool"), logexp.span));
            return EvaluatedType::Unknown;
        }
    })();
    logexp.inferred_type.clone()
}

fn typecheck_this_expression(
    this: &mut TypedThisExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    this.inferred_type = (|| {
        if this.model_or_interface.is_none() {
            // Error is already handled.
            return EvaluatedType::Unknown;
        }
        let model_or_interface = this.model_or_interface.unwrap();
        if checker_ctx
            .current_function_is_static
            .is_some_and(|is_static| is_static)
        {
            let start = [this.start_line, this.start_character];
            let span = Span::on_line(start, 4);
            checker_ctx.add_diagnostic(errors::this_in_static_method(span));
            return EvaluatedType::Unknown;
        }
        // Block the use of `this` as a standalone value in the constructor.
        if !checker_ctx.current_expression_is_access.is_some_and(|x| x)
            && checker_ctx.current_constructor_context.last().is_some()
        {
            let start = [this.start_line, this.start_character];
            let span = Span::on_line(start, 4);
            checker_ctx.add_diagnostic(errors::using_this_before_construction(span));
        }
        let symbol = symbollib.get_forwarded(model_or_interface).unwrap();
        match &symbol.kind {
            SemanticSymbolKind::Model { generic_params, .. } => EvaluatedType::ModelInstance {
                model: model_or_interface,
                generic_arguments: evaluate_generic_params(generic_params, true),
                is_invariant: true,
            },
            SemanticSymbolKind::Interface { generic_params, .. } => {
                EvaluatedType::InterfaceInstance {
                    interface_: model_or_interface,
                    generic_arguments: evaluate_generic_params(generic_params, true),
                    is_invariant: true,
                }
            }
            _ => unreachable!("{symbol:#?} is not a model or interface."),
        }
    })();
    this.inferred_type.clone()
}

/// Typechecks a new expression.
fn typecheck_new_expression(
    newexp: &mut TypedNewExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    newexp.inferred_type = (|| {
        match &mut newexp.value {
            // Helper to fix code if new X is called without parenthesis.
            TypedExpression::Identifier(ident) => {
                let symbol = match symbollib.get_forwarded(ident.value) {
                    Some(symbol) => symbol,
                    None => return EvaluatedType::Unknown,
                };
                if matches!(symbol.kind, SemanticSymbolKind::Model { .. }) {
                    checker_ctx.add_diagnostic(errors::calling_new_on_identifier(
                        symbol.name.clone(),
                        Span::on_line(ident.start, symbol.name.len() as u32),
                    ));
                }
                return EvaluatedType::Unknown;
            }
            TypedExpression::CallExpr(callexp) => {
                let caller = &mut callexp.caller;
                let evaluated_caller = typecheck_expression(caller, checker_ctx, symbollib);
                match evaluated_caller {
                    EvaluatedType::Model(model) => {
                        let model_symbol = symbollib.get_forwarded(model).unwrap();
                        let (mut generic_arguments, generic_params, parameter_types) =
                            if let SemanticSymbolKind::Model {
                                generic_params,
                                is_constructable,
                                constructor_parameters,
                                ..
                            } = &model_symbol.kind
                            {
                                let name = model_symbol.name.clone();
                                let span = newexp.span;
                                // if model does not have a new() function.
                                if !*is_constructable {
                                    checker_ctx.add_diagnostic(errors::model_not_constructable(
                                        name, span,
                                    ));
                                    return EvaluatedType::Unknown;
                                }
                                let generic_arguments =
                                    evaluate_generic_params(generic_params, false);
                                let parameter_types = convert_param_list_to_type(
                                    constructor_parameters.as_ref().unwrap_or(&vec![]),
                                    symbollib,
                                    &generic_arguments,
                                    checker_ctx,
                                );
                                (generic_arguments, generic_params.clone(), parameter_types)
                            } else {
                                unreachable!()
                            };
                        zip_arguments(
                            parameter_types,
                            checker_ctx,
                            callexp,
                            symbollib,
                            &mut generic_arguments,
                        );
                        let result_model_instance = EvaluatedType::ModelInstance {
                            model,
                            // ignore irrelevant generic transforms.
                            generic_arguments: generic_arguments
                                .into_iter()
                                .filter(|argument| {
                                    generic_params.iter().any(|base| *base == argument.0)
                                })
                                .collect(),
                            is_invariant: false,
                        };
                        return result_model_instance;
                    }
                    _ => {
                        checker_ctx.add_diagnostic(errors::invalid_new_expression(
                            checker_ctx.span_of_expr(&callexp.caller, &symbollib),
                        ));
                        return EvaluatedType::Unknown;
                    }
                }
            }
            // Invalid new expressions.
            _ => {
                checker_ctx.add_diagnostic(errors::invalid_new_expression(
                    checker_ctx.span_of_expr(&newexp.value, &symbollib),
                ));
                typecheck_expression(&mut newexp.value, checker_ctx, symbollib);
                return EvaluatedType::Unknown;
            }
        }
    })();
    newexp.inferred_type.clone()
}

/// Typechecks an index expression.
fn typecheck_index_expression(
    indexexp: &mut TypedIndexExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    indexexp.inferred_type = (|| {
        let type_of_indexed = typecheck_expression(&mut indexexp.object, checker_ctx, symbollib);
        let type_of_indexer = typecheck_expression(&mut indexexp.index, checker_ctx, symbollib);
        // todo: handle Index interfaceface overloading.
        let ptr = type_of_indexed;
        if !is_array(&ptr, &symbollib) {
            if !is_array(&ptr, symbollib) {
                checker_ctx.add_diagnostic(errors::invalid_index_subject(
                    symbollib.format_evaluated_type(&ptr),
                    indexexp.span,
                ));
                return EvaluatedType::Unknown;
            }
        }
        // Confirms that the indexer is at least a component of UnsignedInt.
        if let Some(idx) = symbollib.uint {
            let opaque_instance = evaluate(
                &crate::IntermediateType::SimpleType {
                    value: idx,
                    generic_args: vec![],
                    span: Span::default(),
                },
                symbollib,
                None,
                &mut None,
                0,
            );
            if let Err(errors) = unify_types(
                &opaque_instance,
                &type_of_indexer,
                symbollib,
                UnifyOptions::None,
                None,
            ) {
                let span = checker_ctx.span_of_expr(&indexexp.index, symbollib);
                checker_ctx.add_diagnostic(errors::indexing_with_illegal_value(
                    symbollib.format_evaluated_type(&type_of_indexer),
                    span,
                ));
                for error in errors {
                    checker_ctx.add_diagnostic(TypeError { _type: error, span })
                }
            }
        } else {
            checker_ctx.add_diagnostic(errors::missing_intrinsic(
                format!("UnsignedInt"),
                checker_ctx.span_of_expr(&indexexp.index, symbollib),
            ));
        }
        match ptr {
            EvaluatedType::ModelInstance {
                model,
                mut generic_arguments,
                ..
            } if symbollib.array.is_some_and(|idx| idx == model) => {
                if generic_arguments.len() != 1 {
                    EvaluatedType::Unknown
                } else {
                    generic_arguments.remove(0).1
                }
            }
            _ => {
                checker_ctx.add_diagnostic(errors::invalid_index_subject(
                    symbollib.format_evaluated_type(&ptr),
                    indexexp.span,
                ));
                EvaluatedType::Unknown
            }
        }
    })();
    indexexp.inferred_type.clone()
}

/// Typechecks an array expression.
fn typecheck_array_expression(
    array: &mut crate::TypedArrayExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    array.inferred_type = (|| {
        if symbollib.array.is_none() {
            checker_ctx.add_diagnostic(missing_intrinsic(format!("Array"), array.span));
        }
        if array.elements.len() == 0 {
            if symbollib.array.is_some() {
                let empty = vec![];
                let placeholder = arrify(EvaluatedType::Unknown, symbollib);
                let generic = get_type_generics(&placeholder, &empty)
                    .first()
                    .map(|(idx, _)| idx);
                if generic.is_some() {
                    return arrify(
                        EvaluatedType::Generic {
                            base: *generic.unwrap(),
                        },
                        symbollib,
                    );
                }
            }
            return arrify(EvaluatedType::Unknown, &symbollib);
        }
        let mut element_types = vec![];

        let mut next_type = array
            .elements
            .first_mut()
            .map(|expression| typecheck_expression(expression, checker_ctx, symbollib))
            .unwrap_or(EvaluatedType::Unknown);
        // The first type is the base type and is assumed to be the true type for every other type in the array.
        for element in &mut array.elements.iter_mut().skip(1) {
            // Try to guess the type of the next element based on the first type.
            // todo: should the next type be progressively unified before inferring?
            infer_ahead(element, &next_type, symbollib);
            element_types.push(typecheck_expression(element, checker_ctx, symbollib));
        }
        // Reduce individual types to determine final array form.
        let mut i = 1;
        let mut errors_gotten = vec![];
        for evaluated_type in element_types {
            let mut unification = unify_types(
                &next_type,
                &evaluated_type,
                symbollib,
                UnifyOptions::None,
                None,
            );
            // For numeric types, casting should occur bidirectionally.
            // So that elements will always scale the array upwards in size.
            if is_numeric_type(&next_type, symbollib)
                && is_numeric_type(&evaluated_type, symbollib)
                && is_updateable(&array.elements[i], symbollib)
            {
                unification = unification.or(unify_types(
                    &evaluated_type,
                    &next_type,
                    symbollib,
                    UnifyOptions::None,
                    None,
                ));
            }
            match unification {
                Ok(new_type) => next_type = new_type,
                Err(errortypes) => {
                    errors_gotten.push((i, errortypes));
                }
            };
            i += 1;
        }
        if errors_gotten.len() > 0 {
            checker_ctx.add_diagnostic(TypeError {
                _type: TypeErrorType::HeterogeneousArray,
                span: array.span,
            });
            for (idx, errortype) in errors_gotten {
                for error in errortype {
                    checker_ctx.add_diagnostic(TypeError {
                        _type: error,
                        span: array
                            .elements
                            .get(idx)
                            .map(|el| checker_ctx.span_of_expr(el, symbollib))
                            .unwrap_or(array.span),
                    });
                }
            }
        }
        for element in &mut array.elements {
            update_expression_type(
                element,
                symbollib,
                checker_ctx.literals,
                &vec![],
                Some(&next_type),
            )
        }
        arrify(next_type, symbollib)
    })();
    array.inferred_type.clone()
}

/// Typechecks a literal value.
fn typecheck_literal(
    l: &mut crate::LiteralIndex,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    match checker_ctx.literals.get_mut(*l) {
        Some(l) => match l {
            Literal::StringLiteral { value, .. } => typecheck_string_literal(
                value,
                &mut checker_ctx.diagnostics,
                checker_ctx.path_idx,
                symbollib,
            ),
            Literal::NumericLiteral {
                value,
                inferred_type,
                ..
            } => {
                *inferred_type = typecheck_numeric_literal(
                    value,
                    &mut checker_ctx.diagnostics,
                    checker_ctx.path_idx,
                    symbollib,
                );
                inferred_type.clone()
            } // todo.
            Literal::BooleanLiteral {
                value,
                start_line,
                start_character,
                ..
            } => typecheck_boolean_literal(
                symbollib,
                &mut checker_ctx.diagnostics,
                checker_ctx.path_idx,
                start_line,
                start_character,
                value,
            ),
        },
        None => EvaluatedType::Unknown,
    }
}

/// Typechecks a numeric literal.
fn typecheck_numeric_literal(
    value: &ast::WhirlNumber,
    diagnostics: &mut Vec<ProgramDiagnostic>,
    path_idx: PathIndex,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    get_numeric_type(symbollib, value, Some((diagnostics, path_idx)))
}

/// Typechecks a bool literal by matching it with the bool intrinsic symbol.
fn typecheck_boolean_literal(
    symbollib: &mut SymbolLibrary,
    diagnostics: &mut Vec<ProgramDiagnostic>,
    path_idx: PathIndex,
    start_line: &u32,
    start_character: &u32,
    value: &bool,
) -> EvaluatedType {
    if let Some(bool_index) = symbollib.bool {
        return EvaluatedType::ModelInstance {
            model: bool_index,
            generic_arguments: vec![],
            is_invariant: false,
        };
    } else {
        diagnostics.push(ProgramDiagnostic {
            offending_file: path_idx,
            _type: DiagnosticType::Error(Error::Typing(errors::missing_intrinsic(
                format!("Bool"),
                Span::on_line([*start_line, *start_character], if *value { 4 } else { 5 }),
            ))),
        });
        EvaluatedType::Unknown
    }
}

/// Typechecks a string literal by matching it with the string intrinsic symbol.
fn typecheck_string_literal(
    value: &ast::WhirlString,
    diagnostics: &mut Vec<ProgramDiagnostic>,
    path_idx: PathIndex,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    if let Some(string_index) = symbollib.string {
        return EvaluatedType::ModelInstance {
            model: string_index,
            generic_arguments: vec![],
            is_invariant: false,
        };
    }
    diagnostics.push(ProgramDiagnostic {
        offending_file: path_idx,
        _type: DiagnosticType::Error(Error::Typing(errors::missing_intrinsic(
            format!("String"),
            value.span,
        ))),
    });
    return EvaluatedType::Unknown;
}

/// Typechecks a call expression.
fn typecheck_call_expression(
    callexp: &mut TypedCallExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    let caller = typecheck_expression(&mut callexp.caller, checker_ctx, symbollib);
    let caller_span = checker_ctx.span_of_expr(&callexp.caller, &symbollib);
    let caller = extract_call_of(caller, symbollib, checker_ctx, caller_span);
    if caller.is_unknown() {
        callexp.arguments.iter_mut().for_each(|arg| {
            typecheck_expression(arg, checker_ctx, symbollib);
        }); // for continuity.
        return caller;
    }
    // Extract parameters, generic arguments and return_type from caller.
    let (is_async, parameter_types, mut generic_arguments, mut return_type) = match caller {
        EvaluatedType::MethodInstance {
            method,
            mut generic_arguments,
            ..
        }
        | EvaluatedType::FunctionInstance {
            function: method,
            mut generic_arguments,
            ..
        } => {
            let method_symbol = symbollib.get(method).unwrap();
            match &method_symbol.kind {
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
                    let parameter_types = convert_param_list_to_type(
                        params,
                        symbollib,
                        &generic_arguments,
                        checker_ctx,
                    );
                    let return_type = return_type
                        .as_ref()
                        .map(|typ| evaluate(typ, symbollib, None, &mut checker_ctx.tracker(), 0))
                        .unwrap_or(EvaluatedType::Void);
                    (
                        *is_async,
                        parameter_types,
                        {
                            generic_arguments
                                .append(&mut evaluate_generic_params(generic_params, false));
                            generic_arguments
                        },
                        return_type,
                    )
                }
                _ => unreachable!("Expected functional symbol but found {:?}", method_symbol),
            }
        }
        EvaluatedType::FunctionExpressionInstance {
            is_async,
            params,
            return_type,
            generic_args,
            ..
        } => (is_async, params, generic_args, *return_type),
        _ => {
            return EvaluatedType::Unknown;
        }
    };
    // Try to preemptively guess the type of the first argument.
    if let Some(argument) = callexp.arguments.get_mut(0) {
        if let Some(parameter_type) = parameter_types.get(0) {
            infer_ahead(argument, &parameter_type.inferred_type, symbollib);
        }
    }
    // Account for async functions.
    if is_async {
        return_type = prospectify(return_type, symbollib);
    }
    zip_arguments(
        parameter_types,
        checker_ctx,
        callexp,
        symbollib,
        &mut generic_arguments,
    );
    callexp.inferred_type = coerce(return_type, &generic_arguments);
    update_expression_type(
        &mut callexp.caller,
        symbollib,
        checker_ctx.literals,
        &generic_arguments,
        Some(&callexp.inferred_type),
    );
    callexp.inferred_type.clone()
}

/// This function unifies a list of function parameters with a list of call arguments
/// And updates a generic argument with inference results.
fn zip_arguments(
    parameter_types: Vec<ParameterType>,
    checker_ctx: &mut TypecheckerContext,
    callexp: &mut TypedCallExpr,
    symbollib: &mut SymbolLibrary,
    generic_arguments: &mut Vec<(SymbolIndex, EvaluatedType)>,
) {
    // mismatched arguments. It checks if the parameter list is longer, so it can account for optional parameters.
    if parameter_types.len() < callexp.arguments.len() {
        checker_ctx.add_diagnostic(errors::mismatched_function_args(
            callexp.span,
            parameter_types.len(),
            callexp.arguments.len(),
            None,
        ));
        return;
    }
    let mut generic_map = HashMap::new();
    let mut i = 0;
    let caller_type = symbollib
        .get_expression_type(&callexp.caller, checker_ctx.literals)
        .unwrap_or(EvaluatedType::Unknown);
    while i < parameter_types.len() {
        // Generics in call expressions are hard by default, but
        // they can be transformed into regular, coercible generics, depending
        // on whether the caller is a regular instance, a parameter type,
        // or a shadow instance (this). Parameter types inherit the invariance
        // of the function in which they are defined, and shadow instances
        // inherit the invariance of their parent model.
        // Everything else is free real estate.
        let mut parameter_type = parameter_types[i].inferred_type.clone();
        let caller_is_invariant = caller_type.is_invariant();
        let mut unification_option = if caller_is_invariant {
            UnifyOptions::Conform
        } else {
            // If a parameter is a hard generic (or contains a hard generic),
            // it needs to be in list of generics owned by the caller to be coercible.
            if parameter_type.contains_child_for_which(&|child| {
                matches!(child, EvaluatedType::HardGeneric { .. })
            }) {
                let mut param_generics = vec![];
                parameter_type.gather_generics_into(&mut param_generics);
                let caller_generics = match &caller_type {
                        EvaluatedType::FunctionInstance {
                            function: caller_base,
                            ..
                        }
                        // todo: generic params from function types.
                        | EvaluatedType::MethodInstance {
                            method: caller_base,
                            ..
                        } => match &symbollib.get(*caller_base).unwrap().kind {
                            SemanticSymbolKind::Method { generic_params, .. }
                            | SemanticSymbolKind::Function { generic_params, .. } => {
                                Some(generic_params)
                            }
                            _ => None,
                        },
                        _ => None,
                    };
                if param_generics.iter().all(|generic| {
                    caller_generics.is_some_and(|generics| generics.contains(generic))
                }) {
                    UnifyOptions::HardConform
                } else {
                    UnifyOptions::Conform
                }
            } else {
                UnifyOptions::HardConform
            }
        };

        // Account for optional types.
        let is_optional = parameter_types[i].is_optional;
        let argument_type = callexp
            .arguments
            .get_mut(i)
            .map(|expression| typecheck_expression(expression, checker_ctx, symbollib));
        let argument_type = match argument_type {
            Some(evaled_typ) => evaled_typ,
            None => {
                if !is_optional {
                    checker_ctx.add_diagnostic(errors::mismatched_function_args(
                        callexp.span,
                        parameter_types.len(),
                        callexp.arguments.len(),
                        parameter_types.iter().position(|param| param.is_optional),
                    ))
                };
                break;
            }
        };
        // --- hmm.
        let is_hard_generic =
            |child: &EvaluatedType| matches!(child, EvaluatedType::HardGeneric { .. });
        if is_hard_generic(&argument_type) {
            if !(parameter_type.is_generic()
                || parameter_types[i]
                    .type_label
                    .as_ref()
                    .is_some_and(|declared_type| {
                        matches!(declared_type, IntermediateType::This { .. })
                    }))
            {
                unification_option = UnifyOptions::Conform;
            }
        }
        // ---
        if is_optional {
            parameter_type = maybify(parameter_type, symbollib);
        }
        let unification = unify_types(
            &parameter_type,
            &argument_type,
            symbollib,
            unification_option,
            Some(&mut generic_map),
        );
        if let Err(errortype) = unification {
            for errortype in errortype {
                checker_ctx.add_diagnostic(TypeError {
                    _type: errortype,
                    span: checker_ctx.span_of_expr(&callexp.arguments[i], &symbollib),
                })
            }
        }
        // Solve generics with new evaluated types.
        for (generic, assigned_type) in generic_map.iter() {
            if let Some(entry) = generic_arguments
                .iter_mut()
                .find(|prior| prior.0 == *generic)
            {
                entry.1 = assigned_type.clone();
            } else {
                generic_arguments.push((*generic, assigned_type.clone()));
            }
        }
        // Based on transformer generic values,
        // the types of future arguments can be inferred.
        if let Some((typ, expression)) = parameter_types
            .get(i + 1)
            .and_then(|param_type| Some((param_type, callexp.arguments.get_mut(i + 1)?)))
        {
            let target_type = coerce(typ.inferred_type.clone(), &generic_arguments);
            infer_ahead(expression, &target_type, symbollib)
        }
        i += 1;
    }
}

fn convert_param_list_to_type(
    params: &Vec<SymbolIndex>,
    symbollib: &SymbolLibrary,
    solved_generics: &Vec<(SymbolIndex, EvaluatedType)>,
    checker_ctx: &mut TypecheckerContext,
) -> Vec<ParameterType> {
    params
        .iter()
        .map(|param| {
            let parameter_symbol = symbollib.get(*param).unwrap();
            let (is_optional, type_label) = match &parameter_symbol.kind {
                SemanticSymbolKind::Parameter {
                    is_optional,
                    param_type,
                    ..
                } => (*is_optional, param_type),
                _ => unreachable!("Expected parameter but got {parameter_symbol:?}"),
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
                            Some(solved_generics),
                            &mut checker_ctx.tracker(),
                            0,
                        )
                    })
                    .unwrap_or(EvaluatedType::Unknown),
            }
        })
        .collect::<Vec<_>>()
}

fn extract_call_of(
    caller: EvaluatedType,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
    caller_span: Span,
) -> EvaluatedType {
    // Only valid expressions allowed in caller positions:
    // - Enumerated values with tags.
    // - methods
    // - functions
    // - function expressions
    let caller = match caller {
        EvaluatedType::EnumInstance { .. }
        | EvaluatedType::FunctionInstance { .. }
        | EvaluatedType::FunctionExpressionInstance { .. }
        | EvaluatedType::MethodInstance { .. } => caller,
        EvaluatedType::Model(base) => {
            let symbol = symbollib.get_forwarded(base).unwrap();
            checker_ctx
                .add_diagnostic(errors::illegal_model_call(symbol.name.clone(), caller_span));
            EvaluatedType::Unknown
        }
        EvaluatedType::Module(_)
        | EvaluatedType::ModelInstance { .. }
        | EvaluatedType::InterfaceInstance { .. }
        | EvaluatedType::Interface(_)
        | EvaluatedType::Enum(_)
        | EvaluatedType::Generic { .. }
        | EvaluatedType::HardGeneric { .. }
        | EvaluatedType::Void
        | EvaluatedType::Never
        | EvaluatedType::OpaqueTypeInstance { .. } => {
            checker_ctx.add_diagnostic(errors::not_callable(
                symbollib.format_evaluated_type(&caller),
                caller_span,
            ));
            EvaluatedType::Unknown
        }
        _ => EvaluatedType::Unknown,
    };
    caller
}

/// Typechecks a function expression.
fn typecheck_function_expression(
    f: &mut TypedFnExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    f.inferred_type = (|| {
        let mut parameter_types = vec![];
        let generic_args = evaluate_generic_params(&f.generic_params, true);
        for param in &f.params {
            let parameter_symbol = symbollib.get_forwarded(*param).unwrap();
            let (param_type, is_optional) = match &parameter_symbol.kind {
                SemanticSymbolKind::Parameter {
                    param_type,
                    is_optional,
                    ..
                } => (param_type, *is_optional),
                _ => unreachable!(),
            };
            let inferred_type = param_type
                .as_ref()
                .map(|typ| evaluate(typ, symbollib, None, &mut checker_ctx.tracker(), 0))
                .unwrap_or(EvaluatedType::Unknown);
            // Interfaces cannot be used as parameter types.
            if let EvaluatedType::InterfaceInstance { interface_, .. } = &inferred_type {
                let symbol = symbollib.get(*interface_);
                checker_ctx.add_diagnostic(errors::interface_as_type(
                    symbol
                        .map(|symbol| symbol.name.clone())
                        .unwrap_or(String::from("{Interface}")),
                    param_type.as_ref().map(|p| p.span()).unwrap_or_default(),
                ));
            }
            parameter_types.push(ParameterType {
                name: parameter_symbol.name.clone(),
                is_optional,
                type_label: param_type.clone(),
                inferred_type,
            });
        }
        let return_type = f.return_type.as_ref().map(|typ| {
            (
                evaluate(
                    typ,
                    &symbollib,
                    Some(&generic_args),
                    &mut checker_ctx.tracker(),
                    0,
                ),
                typ.span(),
            )
        });
        // Interfaces cannot be used as return types.
        if let Some((EvaluatedType::InterfaceInstance { interface_, .. }, _)) = &return_type {
            let symbol = symbollib.get(*interface_);
            checker_ctx.add_diagnostic(errors::interface_as_type(
                symbol
                    .map(|symbol| symbol.name.clone())
                    .unwrap_or(String::from("{Interface}")),
                f.return_type.as_ref().unwrap().span(),
            ));
        }
        let mut inferred_return_type = match &mut f.body {
            // Function body is scoped to block.
            TypedExpression::Block(block) => {
                checker_ctx
                    .current_function_context
                    .push(CurrentFunctionContext {
                        is_named: false,
                        return_type: return_type
                            .as_ref()
                            .map(|(a, _)| a.clone())
                            .unwrap_or(EvaluatedType::Unknown),
                    });
                push_scopetype(checker_ctx, ScopeType::Other);
                let blocktype = typecheck_block(block, false, checker_ctx, symbollib);
                pop_scopetype(checker_ctx);
                checker_ctx.current_function_context.pop();
                // Derive return type from last return.
                if let Some(statement) = block.statements.last() {
                    if let TypedStmnt::ReturnStatement(retstat) = statement {
                        retstat
                            .value
                            .as_ref()
                            .map(|expr| {
                                symbollib
                                    .get_expression_type(expr, checker_ctx.literals)
                                    .unwrap_or(EvaluatedType::Unknown)
                            })
                            .unwrap_or(EvaluatedType::Void)
                    } else {
                        blocktype
                    }
                } else {
                    blocktype
                }
            }
            expression => typecheck_expression(expression, checker_ctx, symbollib),
        };
        if return_type.is_some() {
            let (return_type, span) = return_type.unwrap();
            match unify_types(
                &return_type,
                &inferred_return_type,
                symbollib,
                UnifyOptions::Return,
                None,
            ) {
                Ok(final_return_type) => {
                    inferred_return_type = final_return_type;
                }
                Err(errors) => {
                    for _type in errors {
                        checker_ctx.add_diagnostic(TypeError { _type, span })
                    }
                    checker_ctx.add_diagnostic(TypeError {
                        _type: TypeErrorType::MismatchedReturnType {
                            expected: symbollib.format_evaluated_type(&return_type),
                            found: symbollib.format_evaluated_type(&inferred_return_type),
                        },
                        span,
                    })
                }
            }
        }
        EvaluatedType::FunctionExpressionInstance {
            is_async: f.is_async,
            params: parameter_types,
            generic_args,
            return_type: Box::new(inferred_return_type),
            is_invariant: false,
        }
    })();
    f.inferred_type.clone()
}

/// Typechecks an access expression.
fn typecheck_access_expression(
    access: &mut TypedAccessExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    checker_ctx.current_expression_is_access = Some(true);
    access.inferred_type = (|| {
        let object_type = typecheck_expression(&mut access.object, checker_ctx, symbollib);
        let property_symbol_idx = match &access.property {
            TypedExpression::Identifier(i) => i.value,
            _ => unreachable!(),
        };
        extract_property_of(
            object_type,
            symbollib,
            property_symbol_idx,
            checker_ctx,
            access,
        )
    })();
    checker_ctx.current_expression_is_access = Some(false);
    access.inferred_type.clone()
}

/// Extract a property based on the value of its object.
fn extract_property_of(
    object_type: EvaluatedType,
    symbollib: &mut SymbolLibrary,
    property_symbol_idx: SymbolIndex,
    checker_ctx: &mut TypecheckerContext,
    access: &mut TypedAccessExpr,
) -> EvaluatedType {
    let property_span = symbollib.get(property_symbol_idx).unwrap().ident_span();
    match object_type {
        // Accessing a property on a model instance.
        EvaluatedType::ModelInstance {
            model,
            ref generic_arguments,
            is_invariant,
        } => search_for_property(
            checker_ctx,
            symbollib,
            model,
            property_symbol_idx,
            generic_arguments.clone(),
            true,
            is_invariant,
            property_span,
        ),
        EvaluatedType::Model(model) => {
            let symbol = symbollib.get_forwarded(model).unwrap();
            let object_is_instance = false;
            // The generic arguments is an unknown list from the generic parameters.
            let generic_arguments = match &symbol.kind {
                SemanticSymbolKind::Model { generic_params, .. } => {
                    evaluate_generic_params(generic_params, false)
                }
                _ => vec![],
            };
            search_for_property(
                checker_ctx,
                symbollib,
                model,
                property_symbol_idx,
                generic_arguments,
                object_is_instance,
                false,
                property_span,
            )
        }
        // EvaluatedType::Interface(_) => todo!(),
        // EvaluatedType::Enum(_) => todo!(),
        EvaluatedType::Module(module) => {
            let module = symbollib.get_forwarded(module).unwrap();
            let property_symbol = symbollib.get_forwarded(property_symbol_idx).unwrap();
            match &module.kind {
                SemanticSymbolKind::Module {
                    global_declaration_symbols,
                    ..
                } => {
                    for idx in global_declaration_symbols {
                        let symbol = match symbollib.get(*idx) {
                            Some(symbol) => symbol,
                            None => continue,
                        };
                        if symbol.name == property_symbol.name {
                            if !symbol.kind.is_public() {
                                checker_ctx.add_diagnostic(TypeError {
                                    _type: TypeErrorType::PrivateSymbolLeak {
                                        modulename: module.name.clone(),
                                        property: property_symbol.name.clone(),
                                    },
                                    span: property_span,
                                });
                            }
                            let actualidx = symbollib.forward(*idx);
                            let actualsymbol = match symbollib.get_forwarded(actualidx) {
                                Some(symbol) => symbol,
                                None => return EvaluatedType::Unknown,
                            };
                            let evaluated_type = symbol_to_type(actualsymbol, actualidx, symbollib)
                                .ok()
                                .unwrap_or(EvaluatedType::Unknown);
                            // get mutably and resolve.
                            let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
                            if let SemanticSymbolKind::Property { resolved, .. } =
                                &mut property_symbol.kind
                            {
                                *resolved = Some(actualidx)
                            }
                            return evaluated_type;
                        }
                    }
                    // No such symbol found.
                    checker_ctx.add_diagnostic(TypeError {
                        _type: TypeErrorType::NoSuchSymbol {
                            modulename: module.name.clone(),
                            property: property_symbol.name.clone(),
                        },
                        span: property_span,
                    });
                    return EvaluatedType::Unknown;
                }
                _ => return EvaluatedType::Unknown,
            }
        }
        EvaluatedType::OpaqueTypeInstance {
            ref available_methods,
            ref generic_arguments,
            ref available_interfaces,
            ..
        } => {
            let mut generic_arguments = generic_arguments.clone();
            // Gather methods from all the implementations.
            let implementation_methods = available_interfaces
                .iter()
                .filter_map(|implementation| {
                    match implementation {
                        EvaluatedType::InterfaceInstance {
                            interface_,
                            generic_arguments: interface_generics,
                            ..
                        } => {
                            let interface_ = *interface_;
                            // Update the solutions of the interfaces generics.
                            generic_arguments.append(&mut (interface_generics.clone()));
                            // Here a interface is treated as a generic argument and given a solution.
                            // This allows the `This` marker to refer to the implementing model, rather than the interface.
                            generic_arguments.push((interface_, object_type.clone()));
                            let interface_symbol = symbollib.get_forwarded(interface_)?;
                            match &interface_symbol.kind {
                                SemanticSymbolKind::Interface { methods, .. } => Some(methods),
                                _ => return None,
                            }
                        }
                        _ => return None,
                    }
                })
                .map(|methods| methods.iter())
                .flatten();
            let complete_method_list: Vec<_> = available_methods
                .iter()
                .chain(implementation_methods)
                .collect();
            let property_symbol = symbollib.get(property_symbol_idx).unwrap();
            for method in complete_method_list {
                let method = *method;
                let method_symbol = match symbollib.get(method) {
                    Some(sym) => sym,
                    None => continue,
                };
                if method_symbol.name == property_symbol.name && method_symbol.kind.is_public() {
                    // get mutably and resolve.
                    let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
                    if let SemanticSymbolKind::Property {
                        resolved,
                        is_opaque,
                    } = &mut property_symbol.kind
                    {
                        *resolved = Some(method);
                        *is_opaque = true;
                    }
                    // todo: Is method public?
                    return EvaluatedType::MethodInstance {
                        method,
                        generic_arguments: generic_arguments.clone(),
                        is_invariant: false,
                    };
                }
            }
            None
        }
        EvaluatedType::Enum(enum_) => {
            let enum_symbol = match symbollib.get(enum_) {
                Some(enum_symbol) => enum_symbol,
                None => return EvaluatedType::Unknown,
            };
            let mut target = None;
            if let SemanticSymbolKind::Enum {
                generic_params,
                variants,
                ..
            } = &enum_symbol.kind
            {
                for variant in variants {
                    let variant_symbol = ast::unwrap_or_continue!(symbollib.get(*variant));
                    let property_symbol =
                        ast::unwrap_or_continue!(symbollib.get(property_symbol_idx));
                    if variant_symbol.name == property_symbol.name {
                        target = Some((*variant, evaluate_generic_params(generic_params, false)));
                        break;
                    };
                }
            }
            if let Some((variant, generic_arguments)) = target {
                let variant_symbol = symbollib.get_mut(variant).unwrap();
                variant_symbol.add_reference(checker_ctx.path_idx, property_span);
                let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
                if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind {
                    *resolved = Some(variant)
                }
                Some(EvaluatedType::EnumInstance {
                    enum_,
                    is_invariant: false,
                    generic_arguments,
                })
            } else {
                None
            }
        }
        EvaluatedType::Generic { base } | EvaluatedType::HardGeneric { base } => {
            search_for_property(
                checker_ctx,
                symbollib,
                base,
                property_symbol_idx,
                vec![],
                true,
                false,
                property_span,
            )
        }
        EvaluatedType::Unknown => return EvaluatedType::Unknown,
        _ => None,
    }
    .unwrap_or_else(|| {
        let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
        if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind {
            *resolved = None; // Just in case.
        }
        let property = property_symbol.name.clone();
        let error = TypeErrorType::NoSuchProperty {
            base_type: symbollib.format_evaluated_type(&object_type),
            property,
        };
        checker_ctx.add_diagnostic(TypeError {
            _type: error,
            span: checker_ctx.span_of_expr(&access.property, &symbollib),
        });
        EvaluatedType::Unknown
    })
}

/// Look through all the possible methods and attributes of a model or generic
/// to determine the property being referenced.
pub fn search_for_property(
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
    model: SymbolIndex,
    property_symbol_idx: SymbolIndex,
    mut generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    object_is_instance: bool,
    is_invariant: bool,
    property_span: Span,
) -> Option<EvaluatedType> {
    // The base type of the model or generic.
    let base_symbol = symbollib.get_forwarded(model).unwrap();
    let property_symbol = symbollib.get(property_symbol_idx).unwrap();
    let impls = match &base_symbol.kind {
        SemanticSymbolKind::Model {
            implementations, ..
        }
        | SemanticSymbolKind::GenericParameter {
            interfaces: implementations,
            ..
        } => implementations,
        _ => return None,
    };
    let model_props = match &base_symbol.kind {
        SemanticSymbolKind::Model {
            methods,
            attributes,
            ..
        } => Some((methods, attributes)),
        _ => None,
    };
    // Gather methods from all the implementations.
    let implementation_methods = impls
        .iter()
        .filter_map(|int_typ| {
            let implementation =
                evaluate(int_typ, symbollib, Some(&generic_arguments), &mut None, 0);
            match implementation {
                EvaluatedType::InterfaceInstance {
                    interface_,
                    generic_arguments: mut interface_generics,
                    ..
                } => {
                    // Update the solutions of the interfaces generics.
                    generic_arguments.append(&mut interface_generics);
                    // Here a interface is treated as a generic argument and given a solution.
                    // This allows the `This` marker to refer to the implementing model, rather than the interface.
                    generic_arguments.push((
                        interface_,
                        EvaluatedType::ModelInstance {
                            model,
                            generic_arguments: generic_arguments.clone(),
                            is_invariant: false,
                        },
                    ));
                    let interface_symbol = symbollib.get_forwarded(interface_)?;
                    match &interface_symbol.kind {
                        SemanticSymbolKind::Interface { methods, .. } => Some(methods),
                        _ => return None,
                    }
                }
                _ => return None,
            }
        })
        .map(|methods| methods.iter())
        .flatten();
    // Collecting into a new vector here because I have not found a feasible way
    // to use different iterator types in the same context, without duplicating a
    // lot of code.
    let complete_method_list: Vec<_> = match &model_props {
        Some((methods, _)) => methods.iter().chain(implementation_methods).collect(),
        None => implementation_methods.collect(),
    };
    // Is property a method?
    // Search through the compound list of methods for appriopriate property.
    for method in complete_method_list.iter() {
        let method = **method;
        let method_symbol = symbollib.get_forwarded(method).unwrap();
        if method_symbol.name == property_symbol.name {
            let method_is_static = match &method_symbol.kind {
                SemanticSymbolKind::Method { is_static, .. } => *is_static,
                _ => false,
            };
            if method_is_static && object_is_instance {
                checker_ctx.add_diagnostic(errors::instance_static_method_access(
                    base_symbol.name.clone(),
                    method_symbol.name.clone(),
                    property_span,
                ))
            } else if !method_is_static && !object_is_instance {
                checker_ctx.add_diagnostic(errors::contructor_non_static_method_access(
                    base_symbol.name.clone(),
                    method_symbol.name.clone(),
                    property_span,
                ))
            }
            // get mutably and resolve.
            let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
            if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind {
                *resolved = Some(method)
            }
            // Add reference on source.
            let method_symbol = symbollib.get_mut(method).unwrap();
            method_symbol.add_reference(checker_ctx.path_idx, property_span);
            // Block non-public access.
            if !method_symbol.kind.is_public()
                && checker_ctx.enclosing_model_or_interface != Some(model)
            {
                checker_ctx.add_diagnostic(errors::private_property_leak(
                    method_symbol.name.clone(),
                    property_span,
                ));
            }
            // Block access in constructor.
            if checker_ctx
                .current_constructor_context
                .last()
                .is_some_and(|constructor_ctx| constructor_ctx.model == model)
                && !method_is_static
            {
                checker_ctx.add_diagnostic(errors::method_in_constructor(property_span));
            }
            return Some(EvaluatedType::MethodInstance {
                method,
                generic_arguments,
                is_invariant,
            });
        }
    }
    // Is property an attribute?
    if let Some((_, attributes)) = model_props {
        for attribute in attributes.iter() {
            let attribute = *attribute;
            let attribute_symbol = symbollib.get_forwarded(attribute).unwrap();
            if attribute_symbol.name == property_symbol.name {
                let result_type = match &attribute_symbol.kind {
                    // todo: Is attribute public?
                    SemanticSymbolKind::Attribute { declared_type, .. } => evaluate(
                        &declared_type,
                        symbollib,
                        Some(&generic_arguments),
                        &mut checker_ctx.tracker(),
                        0,
                    ),
                    _ => return Some(EvaluatedType::Unknown),
                };
                // get mutably.
                let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
                if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind {
                    *resolved = Some(attribute)
                }
                // Add reference on source.
                let attribute_symbol = symbollib.get_mut(attribute).unwrap();
                attribute_symbol.add_reference(checker_ctx.path_idx, property_span);
                if !attribute_symbol.kind.is_public()
                    && checker_ctx.enclosing_model_or_interface != Some(model)
                {
                    checker_ctx.add_diagnostic(errors::private_property_leak(
                        attribute_symbol.name.clone(),
                        property_span,
                    ));
                }
                return Some(result_type);
            }
        }
        // Property has ultimately not been found in the model.
        // Search through the attribute list for possible suggestions.
        for attributes in attributes.iter() {
            let attribute = *attributes;
            let attribute_symbol = symbollib.get_forwarded(attribute).unwrap();
            if attribute_symbol.name.to_lowercase() == property_symbol.name.to_lowercase() {
                checker_ctx.add_diagnostic(errors::mispelled_name(
                    attribute_symbol.name.clone(),
                    property_span,
                ));
                return None;
            }
        }
    }
    // Property has not been found anywhere.
    // Search through complete method list for suggestions.
    for method in complete_method_list {
        let method = *method;
        let method_symbol = symbollib.get_forwarded(method).unwrap();
        if method_symbol.name.to_lowercase() == property_symbol.name.to_lowercase() {
            checker_ctx.add_diagnostic(errors::mispelled_name(
                method_symbol.name.clone(),
                property_span,
            ));
            return None;
        }
    }
    None
}

/// Typechecks an identifier.
fn typecheck_identifier(
    i: &mut TypedIdent,
    symbollib: &mut SymbolLibrary,
) -> Result<EvaluatedType, TypeErrorType> {
    let name = symbollib.forward(i.value);
    let symbol = match symbollib.get_forwarded(name) {
        Some(symbol) => symbol,
        None => return Ok(EvaluatedType::Unknown),
    };
    let eval_type = match symbol_to_type(symbol, name, symbollib) {
        Ok(value) => value,
        Err(value) => return value,
    };
    Ok(eval_type)
}

/// Typechecks a block expression.
pub fn typecheck_block(
    body: &mut TypedBlock,
    is_function_block: bool,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    body.inferred_type = (|| {
        let mut loopindex = 0;
        let statements = &mut body.statements;
        let no_of_statements = statements.len();
        while loopindex < no_of_statements {
            let statement = &mut statements[loopindex];
            if loopindex == no_of_statements - 1 {
                // Returns the type of the last expression in the block.
                // todo: also check for expression statements for diagnostics.
                if let TypedStmnt::FreeExpression(expression) = statement {
                    if is_function_block {
                        if let Some(return_type) = checker_ctx
                            .current_function_context
                            .last()
                            .map(|ctx| &ctx.return_type)
                        {
                            infer_ahead(expression, return_type, symbollib);
                        }
                    }
                    let expression_type = typecheck_expression(expression, checker_ctx, symbollib);
                    let empty = vec![];
                    let generics = get_type_generics(&expression_type, &empty);
                    update_expression_type(
                        expression,
                        symbollib,
                        checker_ctx.literals,
                        generics,
                        Some(&expression_type),
                    );
                    return expression_type;
                } else if let TypedStmnt::ReturnStatement(retstat) = statement {
                    statements::typecheck_return_statement(retstat, checker_ctx, symbollib);
                    if !is_function_block {
                        return EvaluatedType::Never;
                    } else {
                        loopindex += 1;
                        continue;
                    }
                }
            }
            statements::typecheck_statement(statement, checker_ctx, symbollib);
            loopindex += 1;
        }

        EvaluatedType::Void
    })();
    body.inferred_type.clone()
}
