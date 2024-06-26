use super::{expressions::typecheck_block, *};
use crate::{utils::assume_clause_verity, VariablePatternForm};
use ast::{unwrap_or_continue, unwrap_or_return, WhirlString};

mod enum_declaration;
mod forloop;
mod function;
mod interface_declaration;
mod model_declaration;

pub use enum_declaration::typecheck_enum_declaration;
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
        TypedStmnt::EnumDeclaration(enum_) => {
            typecheck_enum_declaration(enum_, checker_ctx, symbollib)
        }
        TypedStmnt::VariableDeclaration(variable) => {
            typecheck_variable_declaration(variable, checker_ctx, symbollib)
        }
        TypedStmnt::ShorthandVariableDeclaration(shorthand_variable) => {
            typecheck_shorthand_variable_declaration(shorthand_variable, checker_ctx, symbollib)
        }
        TypedStmnt::FunctionDeclaration(function) => {
            function::typecheck_function(function, checker_ctx, symbollib)
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
        TypedStmnt::ForStatement(forloop) => {
            forloop::typecheck_for_loop(forloop, checker_ctx, symbollib)
        }
        TypedStmnt::WhileStatement(whil) => typecheck_while_statement(whil, checker_ctx, symbollib),
        TypedStmnt::ModelDeclaration(model) => {
            typecheck_model_declaration(model, checker_ctx, symbollib)
        }
        TypedStmnt::TypedTypeEquation(type_decl) => {
            typecheck_type_decl(type_decl, symbollib, checker_ctx)
        }
        TypedStmnt::ImportDeclaration(import_decl) => {
            typecheck_import_declaration(import_decl, symbollib, checker_ctx)
        }
        _ => {}
    }
}

/// Typechecks an import declaration.
fn typecheck_import_declaration(
    import_decl: &mut crate::TypedImportDeclaration,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
) {
    // Import declarations have only three rules:
    // - The formation of functions should match the correct formation of function signatures.
    // - There should be no duplicate import literals.
    // - They should have no generic parameters.
    let mut found_literals = vec![];
    for (literal_idx, symbolidx) in &import_decl.imports {
        // If there is already an import signature with the same literal name, cause an error.
        match found_literals.iter().find_map(|previous_literal| {
            let previous_literal = checker_ctx.literals.get(*previous_literal);
            let current_literal = checker_ctx.literals.get(*literal_idx);
            match previous_literal.zip(current_literal) {
                Some((
                    Literal::StringLiteral {
                        value:
                            WhirlString {
                                value: previous_value,
                                ..
                            },
                        ..
                    },
                    Literal::StringLiteral {
                        value:
                            WhirlString {
                                value,
                                span: current_value_span,
                            },
                        ..
                    },
                )) if previous_value == value => Some((previous_value, current_value_span)),
                _ => None,
            }
        }) {
            Some((previous_name, span)) => {
                checker_ctx.add_error(errors::duplicate_import_name(previous_name.clone(), *span))
            }
            None => found_literals.push(*literal_idx),
        }
        let symbol = unwrap_or_continue!(symbollib.get_forwarded(*symbolidx));
        let (evaluated_param_types, return_type, return_type_span) =
            if let SemanticSymbolKind::Function {
                params,
                generic_params,
                return_type,
                ..
            } = &symbol.kind
            {
                if generic_params.len() > 0 {
                    checker_ctx.add_error(errors::generic_function_import(symbol.ident_span()))
                }
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
    }
}

fn typecheck_type_decl(
    type_decl: &mut crate::TypedTypeEquation,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
) {
    let assigned = if let Some(SemanticSymbolKind::TypeName {
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
        // Only concrete types (models, enums, generics) and function expression types
        // are allowed as type declaration values.
        // (Ternaries already check for concreteness in the evaluate() function.)
        if !assigned.is_concrete() && !value.is_ternary() && !value.is_function_type() {
            let name = symbollib.format_evaluated_type(&assigned);
            checker_ctx.add_error(errors::expected_implementable(name, value.span()));
            EvaluatedType::Unknown
        } else {
            assigned
        }
    } else {
        return;
    };
    // Update inferred type.
    if let Some(SemanticSymbolKind::TypeName { inferred_type, .. }) = symbollib
        .get_mut(type_decl.name)
        .map(|symbol| &mut symbol.kind)
    {
        *inferred_type = assigned;
    }
}

fn show_interface_as_type_error(
    symbollib: &SymbolLibrary,
    interface_: SymbolIndex,
    checker_ctx: &mut TypecheckerContext<'_>,
    span: Span,
) {
    let symbol = symbollib.get(interface_);
    checker_ctx.add_error(errors::interface_as_type(
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
    // Block non-function meta type assignments.
    if let Some(
        EvaluatedType::Model(_)
        | EvaluatedType::Enum(_)
        | EvaluatedType::Module(_)
        | EvaluatedType::Interface(_),
    ) = &inferred_result
    {
        checker_ctx.add_error(errors::invalid_assignment_target(variable.span));
        return;
    }
    // if value and label are available, unification can be done early.
    // so that the focus later will be the extraction of array and model types.
    if declared_type.is_some() && inferred_result.is_some() {
        let declared = declared_type.as_ref().unwrap();
        // Interfaces are not allowed in type contexts.
        if let EvaluatedType::InterfaceInstance { interface_, .. } = &declared {
            let symbol = symbollib.get(*interface_);
            checker_ctx.add_error(errors::interface_as_type(
                symbol
                    .map(|symbol| symbol.name.clone())
                    .unwrap_or(String::from("{Interface}")),
                variable.span,
            ));
            return;
        }
        // Never types are not allowed in type contexts.
        if declared.contains(&EvaluatedType::Never) {
            checker_ctx.add_error(errors::never_as_declared(variable.span));
        }
        let type_of_value = inferred_result.as_ref().unwrap();
        match unify_freely(declared, &type_of_value, symbollib, None) {
            Ok(unified_type) => inferred_result = Some(unified_type),
            Err(errortypes) => {
                for error in errortypes {
                    checker_ctx.add_error(TypeError {
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
                checker_ctx.add_error(errors::no_default(
                    symbollib.format_evaluated_type(declared),
                    variable.span,
                ));
            }
        }
    }
    // if neither is available, nothing can be done.
    if declared_type.is_none() && variable.value.is_none() {
        checker_ctx.add_error(errors::missing_annotations(variable.span));
        return;
    }
    let final_type = inferred_result.unwrap_or_else(|| declared_type.unwrap());
    let span = variable.span;
    ensure_assignment_validity(&final_type, checker_ctx, span);
    // Pattern resolutions.
    for name in names {
        if variable.value.is_some() {
            let symbol = symbollib.get(*name).unwrap();
            let expression = variable.value.as_ref().unwrap();
            let expression_span = checker_ctx.span_of_expr(expression, symbollib);
            // Only pure, immutable and literal types should be allowed as global variables.
            if symbol.origin_scope_id.is_some_and(|id| id.0 == 0) {
                if !is_pure(expression) {
                    checker_ctx.add_error(errors::non_pure_global(expression_span));
                    return;
                }
            }
            // Variables should not be self referential in their declarations.
            // We can check this using the span range of the expression and
            // checking that it does not contain a reference to the variable being declared.
            symbol
                .references
                .iter()
                .filter(|list| list.module_path == checker_ctx.path_idx)
                .map(|referencelist| {
                    referencelist
                        .starts
                        .iter()
                        .map(|start| Span::on_line(*start, symbol.name.len() as u32))
                })
                .flatten()
                .for_each(|span| {
                    if expression_span.encloses(span) {
                        checker_ctx.diagnostics.push(ProgramDiagnostic {
                            offending_file: checker_ctx.path_idx,
                            _type: DiagnosticType::Error(crate::Error::Typing(
                                errors::self_reference(symbol.name.clone(), span),
                            )),
                        })
                    }
                });
        }
        let symbol = symbollib.get_mut(*name).unwrap();
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
            VariablePatternForm::DestructuredFromObject {
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
                            checker_ctx.add_error(errors::unknown_property(
                                model_name,
                                property_name,
                                span,
                            ));
                        } else {
                            pattern_result = property_type.unwrap();
                            if pattern_result.is_method_instance() {
                                let property_name = get_property_name();
                                let model_name = get_model_name();
                                checker_ctx.add_error(errors::destructuring_method(
                                    model_name,
                                    property_name,
                                    span,
                                ))
                            }
                        }
                    }
                    _ => {
                        checker_ctx.add_error(errors::illegal_model_destructure(
                            symbollib.format_evaluated_type(&final_type),
                            variable.span,
                        ));
                        // No point in checking other patterns.
                        return;
                    }
                }
            }
            VariablePatternForm::DestructuredFromArray => match &final_type {
                EvaluatedType::ModelInstance {
                    generic_arguments, ..
                } if is_array(&final_type, symbollib) => {
                    pattern_result = generic_arguments.first().unwrap().1.clone()
                }
                _ => {
                    checker_ctx.add_error(errors::illegal_array_destructure(
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
        checker_ctx.add_error(errors::non_boolean_logic(
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

    // Variables should not be self referential in their declarations.
    // We can check this using the span range of the expression and
    // checking that it does not contain a reference to the variable being declared.
    let symbol = unwrap_or_return!(symbollib.get(shorthand_variable.name));
    let expression_span = checker_ctx.span_of_expr(&shorthand_variable.value, symbollib);
    symbol
        .references
        .iter()
        .filter(|list| list.module_path == checker_ctx.path_idx)
        .map(|referencelist| {
            referencelist
                .starts
                .iter()
                .map(|start| Span::on_line(*start, symbol.name.len() as u32))
        })
        .flatten()
        .for_each(|span| {
            if expression_span.encloses(span) {
                checker_ctx.diagnostics.push(ProgramDiagnostic {
                    offending_file: checker_ctx.path_idx,
                    _type: DiagnosticType::Error(crate::Error::Typing(errors::self_reference(
                        symbol.name.clone(),
                        span,
                    ))),
                })
            }
        });

    // if no declared type, just assign to variable.
    // else attempt unification.
    // If unification fails, then carry on with the declared type value.
    let inference_result = if let Some(declared) = declared_type {
        if let EvaluatedType::InterfaceInstance { interface_, .. } = &declared {
            let symbol = symbollib.get(*interface_);
            checker_ctx.add_error(errors::interface_as_type(
                symbol
                    .map(|symbol| symbol.name.clone())
                    .unwrap_or(String::from("{Interface}")),
                shorthand_variable.span,
            ));
            return;
        }
        if declared.contains(&EvaluatedType::Never) {
            checker_ctx.add_error(errors::never_as_declared(shorthand_variable.span))
        }
        match unify_freely(&declared, &type_of_value, symbollib, None) {
            Ok(eval_type) => eval_type,
            Err(errortypes) => {
                for error in errortypes {
                    checker_ctx.add_error(TypeError {
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

    // Block non-function meta type assignments.
    if let EvaluatedType::Model(_)
    | EvaluatedType::Enum(_)
    | EvaluatedType::Module(_)
    | EvaluatedType::Interface(_) = &inference_result
    {
        checker_ctx.add_error(errors::invalid_assignment_target(span));
        return;
    }
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
            checker_ctx.add_error(TypeError {
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
                    checker_ctx.add_error(TypeError {
                        _type: errortype,
                        span: retstat.span,
                    });
                }
                checker_ctx.add_error(TypeError {
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
                checker_ctx.add_error(TypeError {
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
                    checker_ctx.add_error(errors::interface_expected(name, interface.span()))
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
                        .add_error(errors::expected_implementable(name, default_value.span()))
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
                        checker_ctx.add_error(errors::composite_type_error(
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
    validate_parameters(
        evaluated_param_types,
        symbollib,
        allow_interface,
        checker_ctx,
        true,
    );
}

pub fn validate_parameters(
    evaluated_param_types: Vec<(SymbolIndex, EvaluatedType)>,
    symbollib: &mut SymbolLibrary,
    allow_interface: bool,
    checker_ctx: &mut TypecheckerContext,
    invariance: bool,
) {
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
            new_type.set_invariance(invariance);
            *inferred_type = new_type;
        }
        if let Some((interface_, span)) = interface_in_label {
            if !allow_interface {
                show_interface_as_type_error(symbollib, interface_, checker_ctx, span)
            }
        }
    }
}
