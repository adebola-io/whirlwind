use super::*;

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

/// Typechecks a function or method body.
pub fn typecheck_function_body(
    method_or_function_symbol_idx: SymbolIndex,
    checker_ctx: &mut TypecheckerContext<'_>,
    return_type: EvaluatedType,
    body: &mut TypedBlock,
    symbollib: &mut SymbolLibrary,
    return_type_span: Option<Span>,
) {
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
        environment = assume_clause_verity(constraint, checker_ctx, symbollib, *span, body.scopeid);
    };
    if let Some(environment) = environment {
        symbollib.push_type_environment_stack(environment);
    }
    let mut block_return_type = expressions::typecheck_block(body, true, checker_ctx, symbollib);
    pop_scopetype(checker_ctx);
    // Ignore unreachable nested generics.
    // if last statement was a return, there is no need to check type again, since it will still show the apprioprate errors.
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
            checker_ctx.add_error(errors::composite_type_error(
                main_error,
                typeerrortype,
                span,
            ));
        }
    }
    symbollib.pop_type_environment_stack(body.scopeid);
    checker_ctx.current_function_context.pop();
}
