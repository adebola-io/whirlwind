mod access;
mod assignment;
mod binary;
mod calls;

use super::{statements::typecheck_generic_params, *};
use crate::{
    programdiagnostic::DiagnosticType, utils::distill_as_function_type, Error, IntermediateType,
};
pub use access::search_for_property;
use access::typecheck_access_expression;
use assignment::typecheck_assignment_expression;
use binary::typecheck_binary_expression;
use calls::typecheck_call_expression;

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
                    checker_ctx.add_error(TypeError {
                        _type: error_type,
                        span: checker_ctx.span_of_expr(expression, &symbollib),
                    });
                    EvaluatedType::Unknown
                }
            }
        }
        TypedExpression::Literal(l) => typecheck_literal(l, checker_ctx, symbollib),
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
                                    checker_ctx
                                        .add_error(errors::illegal_guarantee(name, updateexp.span));
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
                        checker_ctx.add_error(errors::illegal_guarantee(name, updateexp.span));
                        EvaluatedType::Unknown
                    }
                } else {
                    checker_ctx.add_error(errors::missing_intrinsic(
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
                        checker_ctx.add_error(errors::illegal_try(name, updateexp.span));
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
                            checker_ctx.add_error(errors::illegal_try(name, updateexp.span));
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
                            checker_ctx.add_error(TypeError {
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
                                checker_ctx.add_error(TypeError {
                                    _type: TypeErrorType::MismatchedReturnType {
                                        expected: symbollib
                                            .format_evaluated_type(&function_ctx.return_type),
                                        found: symbollib.format_evaluated_type(&returned_type),
                                    },
                                    span: updateexp.span,
                                });
                                for _type in errors {
                                    checker_ctx.add_error(TypeError {
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
                    checker_ctx.add_error(errors::missing_intrinsic(
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
                    checker_ctx.add_error(TypeError {
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
                    return operand_type;
                } else {
                    let typ = symbollib.format_evaluated_type(&operand_type);
                    checker_ctx.add_error(errors::numeric_exclusive_operation(typ, unaryexp.span));
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
            checker_ctx.add_error(errors::non_boolean_logic(
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
                    checker_ctx.add_error(errors::separate_if_types(
                        ifexp.span,
                        symbollib.format_evaluated_type(&block_type),
                        symbollib.format_evaluated_type(&else_type),
                    ));
                    for error in errors {
                        checker_ctx.add_error(TypeError {
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
            checker_ctx.add_error(errors::non_boolean_logic(
                symbollib.format_evaluated_type(&left),
                checker_ctx.span_of_expr(&logexp.right, symbollib),
            ));
        }
        if !is_boolean(&right, symbollib) && !right.is_unknown() {
            checker_ctx.add_error(errors::non_boolean_logic(
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
            checker_ctx.add_error(errors::missing_intrinsic(format!("Bool"), logexp.span));
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
            checker_ctx.add_error(errors::this_in_static_method(span));
            return EvaluatedType::Unknown;
        }
        // Block the use of `this` as a standalone value in the constructor.
        if !checker_ctx.current_expression_is_access.is_some_and(|x| x)
            && checker_ctx.current_constructor_context.last().is_some()
        {
            let start = [this.start_line, this.start_character];
            let span = Span::on_line(start, 4);
            checker_ctx.add_error(errors::using_this_before_construction(span));
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
            _ => return EvaluatedType::Unknown,
        }
    })();
    this.inferred_type.clone()
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
                checker_ctx.add_error(errors::invalid_index_subject(
                    symbollib.format_evaluated_type(&ptr),
                    indexexp.span,
                ));
                return EvaluatedType::Unknown;
            }
        }
        // Confirms that the indexer is at least an int32.
        if let Some(idx) = symbollib.i32 {
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
                checker_ctx.add_error(errors::indexing_with_illegal_value(
                    symbollib.format_evaluated_type(&type_of_indexer),
                    span,
                ));
                for error in errors {
                    checker_ctx.add_error(TypeError { _type: error, span })
                }
            }
        } else {
            checker_ctx.add_error(errors::missing_intrinsic(
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
                checker_ctx.add_error(errors::invalid_index_subject(
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
            checker_ctx.add_error(missing_intrinsic(format!("Array"), array.span));
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
            let unification = unify_types(
                &next_type,
                &evaluated_type,
                symbollib,
                UnifyOptions::None,
                None,
            );
            match unification {
                Ok(new_type) => next_type = new_type,
                Err(errortypes) => {
                    errors_gotten.push((i, errortypes));
                }
            };
            i += 1;
        }
        if errors_gotten.len() > 0 {
            checker_ctx.add_error(TypeError {
                _type: TypeErrorType::HeterogeneousArray,
                span: array.span,
            });
            for (idx, errortype) in errors_gotten {
                for error in errortype {
                    checker_ctx.add_error(TypeError {
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

/// Typechecks a function expression.
fn typecheck_function_expression(
    f: &mut TypedFnExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    f.inferred_type = (|| {
        let mut parameter_types = vec![];
        typecheck_generic_params(&f.generic_params, symbollib, checker_ctx);
        let generic_args = evaluate_generic_params(&f.generic_params, true);
        for param in &f.params {
            let parameter_symbol = symbollib.get_forwarded(*param).unwrap();
            let (param_type, is_optional, span, inferred_type) = match &parameter_symbol.kind {
                SemanticSymbolKind::Parameter {
                    param_type,
                    is_optional,
                    inferred_type,
                    ..
                } => (
                    param_type,
                    *is_optional,
                    parameter_symbol.ident_span(),
                    inferred_type,
                ),
                _ => unreachable!(),
            };
            let inferred_type = (!inferred_type.is_unknown())
                .then(|| inferred_type.clone())
                .or_else(|| {
                    param_type.as_ref().map(|typ| {
                        evaluate(typ, symbollib, None, &mut checker_ctx.tracker(span), 0)
                    })
                })
                .unwrap_or(EvaluatedType::Unknown);
            // Interfaces cannot be used as parameter types.
            if let EvaluatedType::InterfaceInstance { interface_, .. } = &inferred_type {
                let symbol = symbollib.get(*interface_);
                checker_ctx.add_error(errors::interface_as_type(
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
                    &mut checker_ctx.tracker(typ.span()),
                    0,
                ),
                typ.span(),
            )
        });
        // Interfaces cannot be used as return types.
        if let Some((EvaluatedType::InterfaceInstance { interface_, .. }, _)) = &return_type {
            let symbol = symbollib.get(*interface_);
            checker_ctx.add_error(errors::interface_as_type(
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
                        checker_ctx.add_error(TypeError { _type, span })
                    }
                    checker_ctx.add_error(TypeError {
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
        Err(error) => return Err(error),
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
