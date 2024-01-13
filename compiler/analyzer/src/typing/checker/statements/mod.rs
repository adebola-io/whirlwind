use ast::{unwrap_or_continue, unwrap_or_return};

use crate::utils::assume_verity;

use super::{expressions::typecheck_block, *};

mod interface_declaration;
mod model_declaration;
pub use interface_declaration::typecheck_interface;
pub use model_declaration::typecheck_model_declaration;

pub fn typecheck_statement(
    statement: &mut TypedStmnt,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
) {
    match statement {
        // TypedStmnt::RecordDeclaration => todo!(),
        TypedStmnt::TestDeclaration(test) => {
            push_scopetype(checker_ctx, ScopeType::Other);
            typecheck_block(&mut test.body, false, checker_ctx, symbollib);
            pop_scopetype(checker_ctx);
        }
        // TypedStmnt::EnumDeclaration(_) => todo!(),
        TypedStmnt::VariableDeclaration(variable) => {
            typecheck_variable_declaration(variable, checker_ctx, symbollib)
        }
        TypedStmnt::ShorthandVariableDeclaration(shorthand_variable) => {
            typecheck_shorthand_variable_declaration(shorthand_variable, checker_ctx, symbollib)
        }
        // TypedStmnt::ConstantDeclaration(_) => todo!(),
        // TypedStmnt::TypeDeclaration(_) => todo!(),
        // TypedStmnt::ModelDeclaration(_) => todo!(),
        // TypedStmnt::ModuleDeclaration(_) => todo!(),
        TypedStmnt::FunctionDeclaration(function) => {
            typecheck_function(function, checker_ctx, symbollib)
        }
        TypedStmnt::InterfaceDeclaration(interface) => {
            typecheck_interface(interface, checker_ctx, symbollib)
        }
        TypedStmnt::ExpressionStatement(expression) | TypedStmnt::FreeExpression(expression) => {
            expressions::typecheck_expression(expression, checker_ctx, symbollib);
        }
        TypedStmnt::ReturnStatement(retstat) => {
            typecheck_return_statement(retstat, checker_ctx, symbollib);
        }
        // TypedStmnt::BreakStatement(_) => todo!(),
        TypedStmnt::ForStatement(forloop) => typecheck_for_loop(forloop, checker_ctx, symbollib),
        TypedStmnt::WhileStatement(whil) => typecheck_while_statement(whil, checker_ctx, symbollib),
        TypedStmnt::ModelDeclaration(model) => {
            typecheck_model_declaration(model, checker_ctx, symbollib)
        }
        TypedStmnt::TypeDeclaration(type_decl) => {
            typecheck_type_decl(type_decl, symbollib, checker_ctx)
        }
        _ => {}
    }
}

fn typecheck_type_decl(
    type_decl: &mut crate::TypedTypeDeclaration,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
) {
    if let Some(SemanticSymbolKind::TypeName {
        generic_params,
        value,
        ..
    }) = symbollib.get(type_decl.name).map(|symbol| &symbol.kind)
    {
        typecheck_generic_params(generic_params, symbollib, checker_ctx);
        let assigned = evaluate(
            value,
            symbollib,
            None,
            &mut checker_ctx.tracker(value.span()),
            0,
        );
        // Only concrete types (models, enums, generics) are allowed as type declaration
        // values. (Ternaries already check for concreteness in the evaluate() function.)
        if !assigned.is_concrete() && !value.is_ternary() {
            let name = symbollib.format_evaluated_type(&assigned);
            checker_ctx.add_diagnostic(errors::expected_implementable(name, value.span()));
        }
    }
}

/// Typechecks a for loop.
fn typecheck_for_loop(
    forloop: &mut crate::TypedForStatement,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) {
    let iterator_type =
        expressions::typecheck_expression(&mut forloop.iterator, checker_ctx, symbollib);
    if symbollib.iteratable.is_none() || symbollib.asiter.is_none() {
        let span = checker_ctx.span_of_expr(&forloop.iterator, symbollib);
        checker_ctx.add_diagnostic(errors::missing_intrinsic("Iteration".to_owned(), span));
        typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
        return;
    }
    // No variables created.
    // Check the body and return.
    if forloop.items.len() == 0 {
        let body = &mut forloop.body;
        typecheck_for_loop_body(body, checker_ctx, symbollib);
    }
    // if the instance implements both AsIterator and Iteratable, AsIterator will be preferred.
    let asiter = symbollib.asiter.unwrap();
    let iteratable = symbollib.iteratable.unwrap();
    let asiter_generic = symbollib
        .get(asiter)
        .map(|symbol| match &symbol.kind {
            SemanticSymbolKind::Interface { generic_params, .. } => Some(generic_params.get(0)?),
            _ => None,
        })
        .flatten()
        .copied();
    let iteratable_generic = symbollib
        .get(iteratable)
        .map(|symbol| match &symbol.kind {
            SemanticSymbolKind::Interface { generic_params, .. } => Some(generic_params.get(0)?),
            _ => None,
        })
        .flatten()
        .copied();
    let implementation = get_implementation_of(asiter, &iterator_type, symbollib)
        .or_else(|| get_implementation_of(iteratable, &iterator_type, symbollib));
    if implementation.is_none() {
        let illegal_type = symbollib.format_evaluated_type(&iterator_type);
        let span = checker_ctx.span_of_expr(&forloop.iterator, symbollib);
        checker_ctx.add_diagnostic(errors::illegal_iterator(illegal_type, span));
        typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
        return;
    }
    let implementation = implementation.unwrap();
    // Evaluate and determine the unit type for iteration.
    let final_type = match implementation {
        EvaluatedType::InterfaceInstance {
            generic_arguments, ..
        } => {
            let generic_solution = generic_arguments.into_iter().find(|generic| {
                Some(generic.0) == asiter_generic || Some(generic.0) == iteratable_generic
            });
            if generic_solution.is_none() {
                let name = symbollib.format_evaluated_type(&iterator_type);
                let span = checker_ctx.span_of_expr(&forloop.iterator, symbollib);
                checker_ctx.add_diagnostic(errors::illegal_iterator(name, span));
                EvaluatedType::Unknown
            } else {
                let evaluated_type = generic_solution.unwrap().1;
                let full_generic_list = match iterator_type {
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
        }
        _ => EvaluatedType::Unknown,
    };
    // Pattern resolutions.
    for name in &forloop.items {
        let symbol = symbollib.get_mut(*name).unwrap();
        let pattern_type = if let SemanticSymbolKind::LoopVariable {
            pattern_type,
            inferred_type,
            ..
        } = &mut symbol.kind
        {
            if pattern_type.is_normal() {
                // There is only one name to infer.
                *inferred_type = final_type;
                typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
                return;
            }
            pattern_type.clone()
        } else {
            continue;
        };
        let mut pattern_result = EvaluatedType::Unknown;
        match pattern_type {
            crate::VariablePatternForm::DestructuredFromObject {
                from_property: property_symbol_idx,
            } => {
                match &final_type {
                    EvaluatedType::ModelInstance {
                        model,
                        generic_arguments,
                        is_invariant,
                    } => {
                        let property_span = symbollib
                            .get(property_symbol_idx)
                            .map(|sym| sym.ident_span())
                            .unwrap_or_default();
                        let property_type = expressions::search_for_property(
                            checker_ctx,
                            symbollib,
                            *model,
                            property_symbol_idx,
                            generic_arguments.clone(),
                            true,
                            *is_invariant,
                            property_span,
                        );
                        let get_model_name = || symbollib.get(*model).unwrap().name.clone();
                        let get_property_name =
                            || symbollib.get(property_symbol_idx).unwrap().name.clone();
                        if property_type.is_none() {
                            let property_name = get_property_name();
                            let model_name = get_model_name();
                            checker_ctx.add_diagnostic(errors::unknown_property(
                                model_name,
                                property_name,
                                property_span,
                            ));
                        } else {
                            pattern_result = property_type.unwrap();
                            if pattern_result.is_method_instance() {
                                let property_name = get_property_name();
                                let model_name = get_model_name();
                                checker_ctx.add_diagnostic(errors::destructuring_method(
                                    model_name,
                                    property_name,
                                    property_span,
                                ))
                            }
                        }
                    }
                    _ => {
                        checker_ctx.add_diagnostic(errors::illegal_model_destructure(
                            symbollib.format_evaluated_type(&final_type),
                            forloop.span,
                        ));
                        // No point in checking other patterns.
                        typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
                        return;
                    }
                }
            }
            crate::VariablePatternForm::DestructuredFromArray => match &final_type {
                EvaluatedType::ModelInstance {
                    generic_arguments, ..
                } if is_array(&final_type, symbollib) => {
                    pattern_result = generic_arguments.first().unwrap().1.clone()
                }
                _ => {
                    checker_ctx.add_diagnostic(errors::illegal_array_destructure(
                        symbollib.format_evaluated_type(&final_type),
                        forloop.span,
                    ));
                    // No point in checking other patterns.
                    typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
                    return;
                }
            },
            _ => {}
        }
        if let SemanticSymbolKind::LoopVariable { inferred_type, .. } =
            &mut symbollib.get_mut(*name).unwrap().kind
        {
            *inferred_type = pattern_result;
        }
    }
    typecheck_for_loop_body(&mut forloop.body, checker_ctx, symbollib);
}

fn typecheck_for_loop_body(
    body: &mut crate::TypedBlock,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) {
    push_scopetype(checker_ctx, ScopeType::Other);
    let block_type = expressions::typecheck_block(body, false, checker_ctx, symbollib);
    pop_scopetype(checker_ctx);

    // For loop blocks should not have implicit returns.
    // If partials are allowed because having to add the semicolon can get annoying after sometime.
    if !block_type.is_void()
        && !block_type.is_unknown()
        && !block_type.is_never()
        && !(matches!(
            &block_type,
            EvaluatedType::Partial { types } if types.iter().all(|unittype| unittype.is_void() || unittype.is_never())
        ))
    {
        let err_span = body
            .statements
            .last()
            .map(|stmnt| checker_ctx.span_of_stmnt(stmnt, symbollib))
            .unwrap_or(body.span);
        let type_as_string = symbollib.format_evaluated_type(&block_type);
        checker_ctx.add_diagnostic(errors::implicit_loop_return(type_as_string, err_span));
    }
}

/// Typechecks a function or method body.
fn typecheck_function_body(
    method_or_function_symbol_idx: SymbolIndex,
    checker_ctx: &mut TypecheckerContext<'_>,
    return_type: EvaluatedType,
    body: &mut TypedBlock,
    symbollib: &mut SymbolLibrary,
    return_type_span: Option<Span>,
) {
    // If the function is a method, its type constraints must be assumed to be true for the
    // duration of its typechecking. This is done by selecting the generics in the type
    // clause and adding scoped type environments to symbollib (not the checker_ctx). This ensures that
    // the assumptions do not clash with already established properties, and
    // I do not have to rewrite the unification and evaluation functions to pass around the
    // checker_ctx as an argument.
    let symbol = unwrap_or_return!(symbollib.get_forwarded(method_or_function_symbol_idx));
    let mut environment = None;
    if let SemanticSymbolKind::Method {
        constraint: Some((constraint, span)),
        ..
    } = &symbol.kind
    {
        environment = assume_verity(constraint, checker_ctx, symbollib, *span);
    };
    if let Some(environment) = environment {
        // symbollib.apply_type_environment(environment);
    }
    // The function context keeps track of the return type (for checking deeply nested return statements)
    // and whether or not the function is named (for function expressions).
    checker_ctx
        .current_function_context
        .push(CurrentFunctionContext {
            is_named: true,
            return_type: return_type.clone(),
        });
    // todo: be more explicit that the `scopetype` used here is for tracking attribute assignment.
    push_scopetype(checker_ctx, ScopeType::Other);
    let mut block_return_type = expressions::typecheck_block(body, true, checker_ctx, symbollib);
    pop_scopetype(checker_ctx);
    if !block_return_type.is_generic()
        && !block_return_type
            .contains_child_for_which(&|child| matches!(child, EvaluatedType::HardGeneric { .. }))
    {
        block_return_type = coerce_all_generics(&block_return_type, EvaluatedType::Never);
    }
    if body
        .statements
        .last()
        .is_some_and(|statement| matches!(statement, TypedStmnt::ReturnStatement(_)))
    {
        return;
    }
    // Ignore unreachable nested generics.
    // if last statement was a return, there is no need to check type again, since it will still show the apprioprate errors.
    match unify_types(
        &return_type,
        &block_return_type,
        &symbollib,
        UnifyOptions::Return,
        None,
    ) {
        Ok(final_type) => {
            if let Some(TypedStmnt::FreeExpression(expression)) = body.statements.last_mut() {
                let empty = vec![];
                let generic_arguments = get_type_generics(&final_type, &empty);
                update_expression_type(
                    expression,
                    symbollib,
                    checker_ctx.literals,
                    &generic_arguments,
                    Some(&final_type),
                );
            }
        }
        Err(typeerrortype) => {
            let span = body
                .statements
                .last()
                .map(|s| checker_ctx.span_of_stmnt(s, symbollib))
                .or(return_type_span)
                .unwrap_or_else(|| body.span);
            let main_error = TypeErrorType::MismatchedReturnType {
                found: symbollib.format_evaluated_type(&block_return_type),
                expected: symbollib.format_evaluated_type(&return_type),
            };
            checker_ctx.add_diagnostic(errors::composite_type_error(
                main_error,
                typeerrortype,
                span,
            ));
        }
    }
    checker_ctx.current_function_context.pop();
    // symbollib.remove_last_type_environment();
}

fn show_interface_as_type_error(
    symbollib: &SymbolLibrary,
    interface_: SymbolIndex,
    checker_ctx: &mut TypecheckerContext<'_>,
    span: Span,
) {
    let symbol = symbollib.get(interface_);
    checker_ctx.add_diagnostic(errors::interface_as_type(
        symbol
            .map(|symbol| symbol.name.clone())
            .unwrap_or(String::from("{Interface}")),
        span,
    ));
}

/// Typechecks a variable declaration.
fn typecheck_variable_declaration(
    variable: &mut crate::TypedVariableDeclaration,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) {
    let names = &variable.names;
    if variable.names.len() == 0 {
        if let Some(expr) = variable.value.as_mut() {
            expressions::typecheck_expression(expr, checker_ctx, symbollib);
            // for continuity.
        }
        return;
    }
    // If there is no value assigned, there must be a type label, and the given type must implement Default.
    // If there is no type assigned, there must be a value assigned.
    // Bidirectional inferencing is still needed, so the first variable declared is used as a foundation
    // for checking others.
    let declared_type = {
        let symbol = symbollib.get_forwarded(variable.names[0]).unwrap();
        let declared_type = if let SemanticSymbolKind::Variable { declared_type, .. } = &symbol.kind
        {
            declared_type
        } else {
            if let Some(expr) = variable.value.as_mut() {
                expressions::typecheck_expression(expr, checker_ctx, symbollib);
                // for continuity.
            }
            return;
        };
        if declared_type.is_some() {
            // Currently on the first type.
            let declared_type = declared_type.as_ref().unwrap();
            let span = declared_type.span();
            Some(evaluate(
                declared_type,
                symbollib,
                None,
                &mut checker_ctx.tracker(span),
                0,
            ))
        } else {
            None
        }
    };
    // If both variable and label are present, inferencing is possible.
    if declared_type.is_some() && variable.value.is_some() {
        infer_ahead(
            &mut variable.value.as_mut().unwrap(),
            &declared_type.as_ref().unwrap(),
            symbollib,
        );
    }
    let mut inferred_result = variable
        .value
        .as_mut()
        .map(|expr| expressions::typecheck_expression(expr, checker_ctx, symbollib));
    // if value and label are available, unification can be done early.
    // so that the focus later will be the extraction of array and model types.
    if declared_type.is_some() && inferred_result.is_some() {
        let declared = declared_type.as_ref().unwrap();
        // Interfaces are not allowed in type contexts.
        if let EvaluatedType::InterfaceInstance { interface_, .. } = &declared {
            let symbol = symbollib.get(*interface_);
            checker_ctx.add_diagnostic(errors::interface_as_type(
                symbol
                    .map(|symbol| symbol.name.clone())
                    .unwrap_or(String::from("{Interface}")),
                variable.span,
            ));
            return;
        }
        // Never types are not allowed in type contexts.
        if declared.contains(&EvaluatedType::Never) {
            checker_ctx.add_diagnostic(errors::never_as_declared(variable.span));
        }
        let type_of_value = inferred_result.as_ref().unwrap();
        match unify_freely(declared, &type_of_value, symbollib, None) {
            Ok(unified_type) => inferred_result = Some(unified_type),
            Err(errortypes) => {
                for error in errortypes {
                    checker_ctx.add_diagnostic(TypeError {
                        _type: error,
                        span: variable.span,
                    });
                }
            }
        };
    }
    // if label is present but no value:
    if declared_type.is_some() && variable.value.is_none() {
        let declared = declared_type.as_ref().unwrap();
        if let Some(default) = symbollib.default {
            let default_is_implemented =
                get_implementation_of(default, declared, symbollib).is_some();
            if !default_is_implemented {
                checker_ctx.add_diagnostic(errors::no_default(
                    symbollib.format_evaluated_type(declared),
                    variable.span,
                ));
            }
        }
    }
    // if neither is available, nothing can be done.
    if declared_type.is_none() && variable.value.is_none() {
        checker_ctx.add_diagnostic(errors::missing_annotations(variable.span));
        return;
    }
    let final_type = inferred_result.unwrap_or_else(|| declared_type.unwrap());
    let span = variable.span;
    ensure_assignment_validity(&final_type, checker_ctx, span);
    // Pattern resolutions.
    for name in names {
        let symbol = symbollib.get_mut(*name).unwrap();
        // Only pure, immutable and literal types should be allowed as global variables.
        if variable.value.is_some() && symbol.origin_scope_id.is_some_and(|id| id.0 == 0) {
            let expression = variable.value.as_ref().unwrap();
            if !is_pure(expression) {
                let span = checker_ctx.span_of_expr(expression, symbollib);
                checker_ctx.add_diagnostic(errors::non_pure_global(span));
                return;
            }
        }
        let pattern_type = if let SemanticSymbolKind::Variable {
            pattern_type,
            inferred_type,
            ..
        } = &mut symbol.kind
        {
            if pattern_type.is_normal() {
                // There is only one name to infer.
                *inferred_type = final_type;
                return;
            }
            pattern_type.clone()
        } else {
            continue;
        };
        let mut pattern_result = EvaluatedType::Unknown;
        match pattern_type {
            crate::VariablePatternForm::DestructuredFromObject {
                from_property: property_symbol_idx,
            } => {
                match &final_type {
                    EvaluatedType::ModelInstance {
                        model,
                        generic_arguments,
                        is_invariant,
                    } => {
                        let property_span = symbollib
                            .get(property_symbol_idx)
                            .map(|sym| sym.ident_span())
                            .unwrap_or_default();
                        let property_type = expressions::search_for_property(
                            checker_ctx,
                            symbollib,
                            *model,
                            property_symbol_idx,
                            generic_arguments.clone(),
                            true,
                            *is_invariant,
                            property_span,
                        );
                        let get_model_name = || symbollib.get(*model).unwrap().name.clone();
                        let get_property_name =
                            || symbollib.get(property_symbol_idx).unwrap().name.clone();
                        if property_type.is_none() {
                            let property_name = get_property_name();
                            let model_name = get_model_name();
                            checker_ctx.add_diagnostic(errors::unknown_property(
                                model_name,
                                property_name,
                                span,
                            ));
                        } else {
                            pattern_result = property_type.unwrap();
                            if pattern_result.is_method_instance() {
                                let property_name = get_property_name();
                                let model_name = get_model_name();
                                checker_ctx.add_diagnostic(errors::destructuring_method(
                                    model_name,
                                    property_name,
                                    span,
                                ))
                            }
                        }
                    }
                    _ => {
                        checker_ctx.add_diagnostic(errors::illegal_model_destructure(
                            symbollib.format_evaluated_type(&final_type),
                            variable.span,
                        ));
                        // No point in checking other patterns.
                        return;
                    }
                }
            }
            crate::VariablePatternForm::DestructuredFromArray => match &final_type {
                EvaluatedType::ModelInstance {
                    generic_arguments, ..
                } if is_array(&final_type, symbollib) => {
                    pattern_result = generic_arguments.first().unwrap().1.clone()
                }
                _ => {
                    checker_ctx.add_diagnostic(errors::illegal_array_destructure(
                        symbollib.format_evaluated_type(&final_type),
                        variable.span,
                    ));
                    // No point in checking other patterns.
                    return;
                }
            },
            _ => {}
        }
        if let SemanticSymbolKind::Variable { inferred_type, .. } =
            &mut symbollib.get_mut(*name).unwrap().kind
        {
            *inferred_type = pattern_result;
        }
    }
}

fn is_pure(expression: &TypedExpression) -> bool {
    match expression {
        TypedExpression::Literal(_) | TypedExpression::FnExpr(_) => true,
        TypedExpression::ArrayExpr(array) => array.elements.iter().all(|element| is_pure(element)),
        TypedExpression::UnaryExpr(unary) => is_pure(&unary.operand),
        TypedExpression::LogicExpr(logic) => is_pure(&logic.left) && is_pure(&logic.right),
        TypedExpression::BinaryExpr(binexp) => is_pure(&binexp.left) && is_pure(&binexp.right),
        TypedExpression::UpdateExpr(update) => is_pure(&update.operand),
        _ => false,
    }
}

/// Typechecks a while statement.
fn typecheck_while_statement(
    whil: &mut crate::TypedWhileStatement,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) {
    let condition_type =
        expressions::typecheck_expression(&mut whil.condition, checker_ctx, symbollib);
    if !is_boolean(&condition_type, symbollib) && !condition_type.is_unknown() {
        checker_ctx.add_diagnostic(errors::non_boolean_logic(
            symbollib.format_evaluated_type(&condition_type),
            checker_ctx.span_of_expr(&whil.condition, symbollib),
        ))
    }
    push_scopetype(checker_ctx, ScopeType::Other);
    typecheck_block(&mut whil.body, false, checker_ctx, symbollib);
    pop_scopetype(checker_ctx);
}

/// Typechecks a shorthand variable declaration.
pub fn typecheck_shorthand_variable_declaration(
    shorthand_variable: &mut crate::TypedShorthandVariableDeclaration,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
) {
    let name = shorthand_variable.name;
    let symbol = symbollib.get_forwarded(name).unwrap();
    // First evaluate the declared type, if any,
    let declared_type = if let SemanticSymbolKind::Variable { declared_type, .. } = &symbol.kind {
        declared_type.as_ref().map(|typ| {
            evaluate(
                typ,
                symbollib,
                None,
                &mut checker_ctx.tracker(typ.span()),
                0,
            )
        })
    } else {
        None
    };
    if declared_type.is_some() {
        // Try to guess the type of the value.
        let declared_type = declared_type.as_ref().unwrap();
        infer_ahead(&mut shorthand_variable.value, &declared_type, symbollib);
    }
    let type_of_value =
        expressions::typecheck_expression(&mut shorthand_variable.value, checker_ctx, symbollib);

    // if no declared type, just assign to variable.
    // else attempt unification.
    // If unification fails, then carry on with the declared type value.
    let inference_result = if let Some(declared) = declared_type {
        if let EvaluatedType::InterfaceInstance { interface_, .. } = &declared {
            let symbol = symbollib.get(*interface_);
            checker_ctx.add_diagnostic(errors::interface_as_type(
                symbol
                    .map(|symbol| symbol.name.clone())
                    .unwrap_or(String::from("{Interface}")),
                shorthand_variable.span,
            ));
            return;
        }
        if declared.contains(&EvaluatedType::Never) {
            checker_ctx.add_diagnostic(errors::never_as_declared(shorthand_variable.span))
        }
        match unify_freely(&declared, &type_of_value, symbollib, None) {
            Ok(eval_type) => eval_type,
            Err(errortypes) => {
                for error in errortypes {
                    checker_ctx.add_diagnostic(TypeError {
                        _type: error,
                        span: shorthand_variable.span,
                    });
                }
                declared
            }
        }
    } else {
        type_of_value
    };
    let span = shorthand_variable.span;
    ensure_assignment_validity(&inference_result, checker_ctx, span);
    if let SemanticSymbolKind::Variable { inferred_type, .. } =
        &mut symbollib.get_mut(name).unwrap().kind
    {
        *inferred_type = inference_result;
    }
}

/// Typechecks a return statement.
pub fn typecheck_return_statement(
    retstat: &mut TypedReturnStatement,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
) {
    let mut function_context = checker_ctx.current_function_context.last().cloned();
    let maybe_eval_expr = retstat.value.as_mut().map(|expr| {
        if let Some(ctx) = &function_context {
            // try to guess the type for the returned expression.
            infer_ahead(expr, &ctx.return_type, symbollib);
        }
        // Coerce unresolved nested generic types to never, since they are no longer resolvable.
        let mut return_type = expressions::typecheck_expression(expr, checker_ctx, symbollib);
        if !return_type.is_generic()
            && !return_type.contains_child_for_which(&|child| {
                matches!(child, EvaluatedType::HardGeneric { .. })
            })
            && function_context.as_ref().is_some_and(|ctx| ctx.is_named)
        {
            return_type = coerce_all_generics(&return_type, EvaluatedType::Never)
        }
        return_type
    });
    let function_context = function_context.as_mut();
    if let Some(eval_type) = &maybe_eval_expr {
        if function_context.is_none()
            || function_context
                .as_ref()
                .is_some_and(|ctx| ctx.is_named && ctx.return_type.is_void())
        {
            // returns with a value, but no value was requested.
            checker_ctx.add_diagnostic(TypeError {
                _type: TypeErrorType::MismatchedReturnType {
                    expected: symbollib.format_evaluated_type(&EvaluatedType::Void),
                    found: symbollib.format_evaluated_type(eval_type),
                },
                span: retstat.span,
            });
            return;
        }
        if function_context
            .as_ref()
            .is_some_and(|ctx| ctx.return_type.is_unknown())
        {
            // If the current function context is unknown,
            // but the result type of this return statement is known,
            // coerce the function context's return type to whatever type is produced here.
            let prior_evaluated_type = checker_ctx.current_function_context.last_mut().unwrap();
            prior_evaluated_type.return_type = maybe_eval_expr.unwrap();
            return;
        }
        let ctx_return_type = function_context
            .map(|ctx| &ctx.return_type)
            .unwrap_or_else(|| &EvaluatedType::Void);

        // returns with a value, and a type is assigned.
        // coerce both types to match.
        match unify_types(
            ctx_return_type,
            eval_type,
            symbollib,
            UnifyOptions::Return,
            None,
        ) {
            // Unification was successful and return type can be updated.
            Ok(typ) => {
                let prior_evaluated_type = checker_ctx.current_function_context.last_mut().unwrap();
                if let Some(expr) = retstat.value.as_mut() {
                    let empty = vec![];
                    let generic_arguments = get_type_generics(&typ, &empty);
                    update_expression_type(
                        expr,
                        symbollib,
                        checker_ctx.literals,
                        generic_arguments,
                        Some(&typ),
                    );
                }
                prior_evaluated_type.return_type = typ;
            }
            // Unification failed.
            Err(errortype) => {
                for errortype in errortype {
                    checker_ctx.add_diagnostic(TypeError {
                        _type: errortype,
                        span: retstat.span,
                    });
                }
                checker_ctx.add_diagnostic(TypeError {
                    _type: TypeErrorType::MismatchedReturnType {
                        expected: symbollib.format_evaluated_type(&ctx_return_type),
                        found: symbollib.format_evaluated_type(eval_type),
                    },
                    span: retstat.span,
                });
            }
        }
    } else {
        // does not return a value.
        if let Some(return_type) = function_context.map(|ctx| &mut ctx.return_type) {
            if !return_type.is_void() {
                if return_type.is_unknown() {
                    // does not return a value and not value was requested.
                    *return_type = EvaluatedType::Void;
                    return;
                }
                checker_ctx.add_diagnostic(TypeError {
                    _type: TypeErrorType::MismatchedReturnType {
                        expected: symbollib.format_evaluated_type(return_type),
                        found: symbollib.format_evaluated_type(&EvaluatedType::Void),
                    },
                    span: retstat.span,
                })
            }
        }
    }
}

/// Typechecks a function.
pub fn typecheck_function(
    function: &mut TypedFunctionDeclaration,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
) {
    let symbol = symbollib.get_forwarded(function.name).unwrap();
    let (evaluated_param_types, return_type, return_type_span) =
        if let SemanticSymbolKind::Function {
            params,
            generic_params,
            return_type,
            ..
        } = &symbol.kind
        {
            typecheck_generic_params(generic_params, symbollib, checker_ctx);
            let generic_arguments = evaluate_generic_params(generic_params, true);
            let mut evaluated_param_types = vec![];
            for param in params {
                let parameter_symbol = symbollib.get(*param).unwrap();
                let inferred_type = match &parameter_symbol.kind {
                    SemanticSymbolKind::Parameter { param_type, .. } => {
                        if let Some(declared_type) = param_type {
                            evaluate(
                                declared_type,
                                symbollib,
                                Some(&generic_arguments),
                                &mut checker_ctx.tracker(declared_type.span()),
                                0,
                            )
                        } else {
                            EvaluatedType::Unknown
                        }
                    }
                    _ => EvaluatedType::Unknown,
                };
                evaluated_param_types.push((*param, inferred_type));
            }
            let return_type = return_type.as_ref();
            (
                evaluated_param_types,
                return_type
                    .map(|typ| {
                        evaluate(
                            typ,
                            &symbollib,
                            None,
                            &mut checker_ctx.tracker(typ.span()),
                            0,
                        )
                    })
                    .unwrap_or_else(|| EvaluatedType::Void),
                return_type.map(|typ| typ.span()),
            )
        } else {
            (vec![], EvaluatedType::Void, None)
        };
    validate_return_type_and_params(
        &return_type,
        symbollib,
        checker_ctx,
        return_type_span,
        evaluated_param_types,
        false,
    );
    typecheck_function_body(
        function.name,
        checker_ctx,
        return_type,
        &mut function.body,
        symbollib,
        return_type_span,
    );
}

/// Confirms that a list of generic params do not have impls that are not interface instances.
pub fn typecheck_generic_params(
    generic_params: &Vec<SymbolIndex>,
    symbollib: &SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
) {
    for param in generic_params.iter() {
        let symbol = unwrap_or_continue!(symbollib.get(*param));
        if let SemanticSymbolKind::GenericParameter {
            interfaces,
            default_value,
        } = &symbol.kind
        {
            for interface in interfaces {
                let interface_type = evaluate(
                    interface,
                    symbollib,
                    None,
                    &mut checker_ctx.tracker(interface.span()),
                    0,
                );
                if !interface_type.is_interface_instance() {
                    let name = symbollib.format_evaluated_type(&interface_type);
                    checker_ctx.add_diagnostic(errors::interface_expected(name, interface.span()))
                }
            }
            if let Some(default_value) = default_value.as_ref() {
                let default_value_evaled = evaluate(
                    default_value,
                    symbollib,
                    None,
                    &mut checker_ctx.tracker(default_value.span()),
                    0,
                );
                if !&default_value_evaled.is_concrete() {
                    let name = symbollib.format_evaluated_type(&default_value_evaled);
                    checker_ctx
                        .add_diagnostic(errors::expected_implementable(name, default_value.span()))
                } else if !default_value_evaled.is_never() {
                    // Assert that the default value is assignable based on the constraints given.
                    let main_generic_evaled = EvaluatedType::Generic { base: *param };
                    if let Err(errors) = unify_types(
                        &main_generic_evaled,
                        &default_value_evaled,
                        symbollib,
                        UnifyOptions::Conform,
                        None,
                    ) {
                        let name = symbollib.format_evaluated_type(&default_value_evaled);
                        let main_error = TypeErrorType::InvalidDefaultType {
                            name,
                            generic: symbol.name.to_owned(),
                        };
                        checker_ctx.add_diagnostic(errors::composite_type_error(
                            main_error,
                            errors,
                            default_value.span(),
                        ))
                    }
                }
            }
        }
    }
}

fn validate_return_type_and_params(
    return_type: &EvaluatedType,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
    return_type_span: Option<Span>,
    evaluated_param_types: Vec<(SymbolIndex, EvaluatedType)>,
    allow_interface: bool,
) {
    // Interfaces cannot be used as return types.
    if let EvaluatedType::InterfaceInstance { interface_, .. } = return_type {
        if !allow_interface {
            show_interface_as_type_error(
                symbollib,
                *interface_,
                checker_ctx,
                return_type_span.unwrap_or_default(),
            )
        }
    }
    for (param_idx, mut new_type) in evaluated_param_types {
        let mut interface_in_label = None;
        if let SemanticSymbolKind::Parameter {
            inferred_type,
            param_type,
            ..
        } = &mut symbollib.get_mut(param_idx).unwrap().kind
        {
            // Interfaces cannot be used as parameter types.
            if let EvaluatedType::InterfaceInstance { interface_, .. } = &new_type {
                interface_in_label = Some((
                    *interface_,
                    param_type.as_ref().map(|p| p.span()).unwrap_or_default(),
                ));
            }
            new_type.set_invariance(true);
            *inferred_type = new_type;
        }
        if let Some((interface_, span)) = interface_in_label {
            if !allow_interface {
                show_interface_as_type_error(symbollib, interface_, checker_ctx, span)
            }
        }
    }
}
