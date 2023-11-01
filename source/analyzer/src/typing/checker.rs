use crate::{
    evaluate,
    unify::unify_types,
    utils::{coerce, maybify, prospectify},
    EvaluatedType, Literal, ParameterType, PathIndex, ProgramError, SemanticSymbolKind,
    SymbolIndex, SymbolTable, TypedAccessExpr, TypedBlock, TypedCallExpr, TypedExpression,
    TypedFnExpr, TypedFunctionDeclaration, TypedIdent, TypedModule, TypedReturnStatement,
    TypedStmnt,
};
use ast::Span;
use errors::{TypeError, TypeErrorType};

/// The typechecker is the second pass of the analyzer,
/// used for infering and validating the use of data types within a module.
/// It also does some control flow analysis, validates return types,
/// solves generics and ensures valid property assignment and use.
pub struct TypecheckerContext<'a> {
    path_idx: PathIndex,
    /// The return type of the closest function scope currently being typechecked.
    current_function_return_type: Vec<CurrentFunctionContext>,
    /// List of errors from the standpoint.
    errors: &'a mut Vec<ProgramError>,
    /// List of literal types from the standpoint.
    literals: &'a [Literal],
}

#[derive(Clone)]
pub struct CurrentFunctionContext {
    /// Whether it is a named function or a function expression.
    is_named: bool,
    return_type: EvaluatedType,
}

impl<'a> TypecheckerContext<'a> {
    pub fn add_error(&mut self, error: TypeError) {
        self.errors.push(ProgramError {
            offending_file: self.path_idx,
            error_type: crate::ProgramErrorType::Typing(error),
        })
    }

    pub fn tracker(&mut self) -> Option<(&mut Vec<ProgramError>, PathIndex)> {
        Some((self.errors, self.path_idx))
    }

    fn span_of_expr(&self, expression: &TypedExpression, symboltable: &SymbolTable) -> Span {
        match expression {
            TypedExpression::Identifier(i) => {
                let symbol = symboltable.get(i.value).unwrap();
                Span::on_line(i.start, symbol.name.len() as u32)
            }
            TypedExpression::Literal(l) => match &self.literals[l.0] {
                Literal::StringLiteral { value, .. } => value.span,
                Literal::NumericLiteral { value, .. } => value.span,
                Literal::BooleanLiteral {
                    value,
                    start_line,
                    start_character,
                    ..
                } => Span {
                    start: [*start_line, *start_character],
                    end: [*start_line, *start_character + if *value { 4 } else { 5 }],
                },
            },
            TypedExpression::CallExpr(c) => c.span,
            TypedExpression::FnExpr(f) => f.span,
            TypedExpression::Block(b) => b.span,
            TypedExpression::IfExpr(i) => i.span,
            TypedExpression::ArrayExpr(a) => a.span,
            TypedExpression::IndexExpr(i) => i.span,
            TypedExpression::BinaryExpr(b) => b.span,
            TypedExpression::AssignmentExpr(a) => a.span,
            TypedExpression::UnaryExpr(u) => u.span,
            TypedExpression::LogicExpr(l) => l.span,
            TypedExpression::AccessExpr(a) => a.span,
            TypedExpression::ThisExpr(t) => Span {
                start: [t.start_line, t.start_character],
                end: [t.start_line, t.start_character + 4],
            },
            TypedExpression::NewExpr(n) => n.span,
            TypedExpression::UpdateExpr(u) => u.span,
        }
    }

    fn span_of_stmnt(&self, s: &TypedStmnt, symboltable: &mut SymbolTable) -> Span {
        match s {
            TypedStmnt::TestDeclaration(t) => t.span,
            TypedStmnt::UseDeclaration(u) => u.span,
            TypedStmnt::VariableDeclaration(v) => v.span,
            TypedStmnt::ConstantDeclaration(c) => c.span,
            TypedStmnt::ModelDeclaration(c) => c.span,
            TypedStmnt::FunctionDeclaration(f) => f.span,
            TypedStmnt::RecordDeclaration => todo!(),
            TypedStmnt::TraitDeclaration(t) => t.span,
            TypedStmnt::EnumDeclaration(e) => e.span,
            TypedStmnt::TypeDeclaration(t) => t.span,
            TypedStmnt::WhileStatement(w) => w.span,
            TypedStmnt::ForStatement(f) => f.span,
            TypedStmnt::ExpressionStatement(e) | TypedStmnt::FreeExpression(e) => {
                self.span_of_expr(e, symboltable)
            }
            TypedStmnt::ShorthandVariableDeclaration(v) => v.span,
            TypedStmnt::ModuleDeclaration(m) => m.span,
            TypedStmnt::ReturnStatement(r) => r.span,
            TypedStmnt::ContinueStatement(c) => c.span,
            TypedStmnt::BreakStatement(b) => b.span,
        }
    }
}

/// Typechecks a module.
pub fn typecheck(
    module: &mut TypedModule,
    symboltable: &mut SymbolTable,
    errors: &mut Vec<ProgramError>,
    literals: &[Literal],
) {
    let mut checker_ctx = TypecheckerContext {
        path_idx: module.path_idx,
        current_function_return_type: vec![], // unknowns: vec![],
        errors,
        literals,
    };
    for statement in &mut module.statements {
        statements::typecheck_statement(statement, &mut checker_ctx, symboltable);
    }
}

mod statements {
    use super::*;

    pub fn typecheck_statement(
        statement: &mut TypedStmnt,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) {
        match statement {
            // TypedStmnt::RecordDeclaration => todo!(),
            // TypedStmnt::TestDeclaration(_) => todo!(),
            // TypedStmnt::EnumDeclaration(_) => todo!(),
            // TypedStmnt::UseDeclaration(_) => todo!(),
            // TypedStmnt::VariableDeclaration(_) => todo!(),
            TypedStmnt::ShorthandVariableDeclaration(shorthand_variable) => {
                typecheck_shorthand_variable_declaration(
                    shorthand_variable,
                    checker_ctx,
                    symboltable,
                )
            }
            // TypedStmnt::ConstantDeclaration(_) => todo!(),
            // TypedStmnt::TypeDeclaration(_) => todo!(),
            // TypedStmnt::ModelDeclaration(_) => todo!(),
            // TypedStmnt::ModuleDeclaration(_) => todo!(),
            TypedStmnt::FunctionDeclaration(function) => {
                typecheck_function(function, checker_ctx, symboltable)
            }
            // TypedStmnt::TraitDeclaration(_) => todo!(),
            TypedStmnt::ExpressionStatement(expression)
            | TypedStmnt::FreeExpression(expression) => {
                expressions::typecheck_expression(expression, checker_ctx, symboltable);
            }
            TypedStmnt::ReturnStatement(retstat) => {
                typecheck_return_statement(retstat, checker_ctx, symboltable);
            }
            // TypedStmnt::BreakStatement(_) => todo!(),
            // TypedStmnt::ForStatement(_) => todo!(),
            // TypedStmnt::WhileStatement(_) => todo!(),
            // TypedStmnt::ContinueStatement(_) => todo!(),
            _ => {}
        }
    }

    /// Typechecks a shorthand variable declaration.
    fn typecheck_shorthand_variable_declaration(
        shorthand_variable: &mut crate::TypedShorthandVariableDeclaration,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) {
        let name = shorthand_variable.name;
        let symbol = symboltable.get_forwarded(name).unwrap();
        // First evaluate the declared type, if any,
        let declared_type = if let SemanticSymbolKind::Variable { declared_type, .. } = &symbol.kind
        {
            declared_type.as_ref().map(|typ| {
                evaluate(
                    typ,
                    symboltable,
                    None,
                    &mut Some((checker_ctx.errors, checker_ctx.path_idx)),
                )
            })
        } else {
            None
        };
        let type_of_value = expressions::typecheck_expression(
            &mut shorthand_variable.value,
            checker_ctx,
            symboltable,
        );
        // if no declared type, just assign to variable.
        // else attempt unification.
        // If unification fails, then carry on with the declared type value.
        let inference_result = if let Some(declared) = declared_type {
            match unify_types(&declared, &type_of_value, symboltable, None) {
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
        // todo: block partially evaluated types, such as those produced by conditional expressions.
        if let SemanticSymbolKind::Variable { inferred_type, .. } =
            &mut symboltable.get_mut(name).unwrap().kind
        {
            *inferred_type = inference_result;
        }
    }

    /// Typechecks a return statement.
    fn typecheck_return_statement(
        retstat: &mut TypedReturnStatement,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) {
        let maybe_eval_expr = retstat
            .value
            .as_mut()
            .map(|expr| expressions::typecheck_expression(expr, checker_ctx, symboltable));
        let function_context = checker_ctx.current_function_return_type.last().cloned();
        let function_context = function_context.as_ref();
        if let Some(eval_type) = &maybe_eval_expr {
            if function_context.is_none()
                || function_context.is_some_and(|ctx| ctx.is_named && ctx.return_type.is_void())
            {
                // returns with a value, but no value was requested.
                checker_ctx.add_error(TypeError {
                    _type: TypeErrorType::MismatchedReturnType {
                        expected: symboltable.format_evaluated_type(&EvaluatedType::Void),
                        found: symboltable.format_evaluated_type(eval_type),
                    },
                    span: retstat.span,
                })
            } else if function_context.is_some_and(|ctx| ctx.return_type.is_unknown()) {
                // If the current function context is unknown,
                // but the result type of this return statement is known,
                // coerce the function context's return type to whatever type is produced here.
                let prior_evaluated_type =
                    checker_ctx.current_function_return_type.last_mut().unwrap();
                prior_evaluated_type.return_type = maybe_eval_expr.unwrap();
            } else {
                let ctx_return_type = function_context
                    .map(|ctx| &ctx.return_type)
                    .unwrap_or_else(|| &EvaluatedType::Void);
                // returns with a value, and a type is assigned.
                // coerce both types to match.
                match unify_types(eval_type, ctx_return_type, symboltable, None) {
                    // Unification was successful and return type can be updated.
                    Ok(typ) => {
                        let prior_evaluated_type =
                            checker_ctx.current_function_return_type.last_mut().unwrap();
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
                                expected: symboltable.format_evaluated_type(&ctx_return_type),
                                found: symboltable.format_evaluated_type(eval_type),
                            },
                            span: retstat.span,
                        })
                    }
                }
            }
        }
    }

    /// Typechecks a function.
    fn typecheck_function(
        function: &mut TypedFunctionDeclaration,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) {
        let symbol = symboltable.get_forwarded(function.name).unwrap();
        let (evaluated_param_types, return_type, return_type_span) =
            if let SemanticSymbolKind::Function {
                params,
                generic_params,
                return_type,
                ..
            } = &symbol.kind
            {
                let generic_arguments = generic_params
                    .iter()
                    .map(|param| (*param, EvaluatedType::Generic { base: *param }))
                    .collect::<Vec<_>>();
                let mut evaluated_param_types = vec![];
                for param in params {
                    let parameter_symbol = symboltable.get(*param).unwrap();
                    let inferred_type = match &parameter_symbol.kind {
                        SemanticSymbolKind::Parameter { param_type, .. } => {
                            if let Some(declared_type) = param_type {
                                evaluate(
                                    declared_type,
                                    symboltable,
                                    Some(&generic_arguments),
                                    &mut Some((checker_ctx.errors, checker_ctx.path_idx)),
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
                        .map(|typ| evaluate(typ, &symboltable, None, &mut checker_ctx.tracker()))
                        .unwrap_or_else(|| EvaluatedType::Void),
                    return_type.map(|typ| typ.span()),
                )
            } else {
                (vec![], EvaluatedType::Void, None)
            };

        for (param_idx, new_type) in evaluated_param_types {
            if let SemanticSymbolKind::Parameter { inferred_type, .. } =
                &mut symboltable.get_mut(param_idx).unwrap().kind
            {
                *inferred_type = new_type;
            }
        }
        checker_ctx
            .current_function_return_type
            .push(CurrentFunctionContext {
                is_named: true,
                return_type: return_type.clone(),
            });
        let block_return_type =
            expressions::typecheck_block(&mut function.body, true, checker_ctx, symboltable);
        if let Err(typeerrortype) =
            unify_types(&block_return_type, &return_type, &symboltable, None)
        {
            let span = return_type_span
                .or_else(|| {
                    function
                        .body
                        .statements
                        .last()
                        .map(|s| checker_ctx.span_of_stmnt(s, symboltable))
                })
                .unwrap_or_else(|| function.body.span);

            for errortype in typeerrortype {
                checker_ctx.add_error(TypeError {
                    _type: errortype,
                    span,
                });
            }
            checker_ctx.add_error(TypeError {
                _type: TypeErrorType::MismatchedReturnType {
                    found: symboltable.format_evaluated_type(&block_return_type),
                    expected: symboltable.format_evaluated_type(&return_type),
                },
                span,
            });
        }
        checker_ctx.current_function_return_type.pop();
    }
}

mod expressions {
    use std::collections::HashMap;

    use errors::missing_intrinsic;

    use crate::{
        utils::{arrify, is_array},
        TypedIndexExpr, TypedNewExpr,
    };

    use super::*;

    /// Typechecks an expression.
    pub fn typecheck_expression(
        expression: &mut crate::TypedExpression,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        match expression {
            TypedExpression::Identifier(i) => {
                // Ensure that identifiers refer to values, not types.
                match typecheck_identifier(i, symboltable) {
                    Ok(evaluated_type) => evaluated_type,
                    Err(error_type) => {
                        checker_ctx.add_error(TypeError {
                            _type: error_type,
                            span: checker_ctx.span_of_expr(expression, &symboltable),
                        });
                        EvaluatedType::Unknown
                    }
                }
            }
            TypedExpression::Literal(l) => typecheck_literal(l, checker_ctx, symboltable),
            TypedExpression::NewExpr(newexp) => {
                typecheck_new_expression(&mut *newexp, symboltable, checker_ctx)
            }
            // TypedExpression::ThisExpr(_) => todo!(),
            TypedExpression::CallExpr(c) => {
                typecheck_call_expression(&mut *c, symboltable, checker_ctx)
            }
            TypedExpression::FnExpr(f) => {
                typecheck_function_expression(&mut *f, symboltable, checker_ctx)
            }
            TypedExpression::Block(body) => typecheck_block(body, false, checker_ctx, symboltable),
            // TypedExpression::IfExpr(_) => todo!(),
            TypedExpression::AccessExpr(access) => {
                typecheck_access_expression(&mut *access, symboltable, checker_ctx)
            }
            TypedExpression::ArrayExpr(array) => {
                typecheck_array_expression(array, symboltable, checker_ctx)
            }
            TypedExpression::IndexExpr(indexexp) => {
                typecheck_index_expression(indexexp, symboltable, checker_ctx)
            }
            // TypedExpression::BinaryExpr(_) => todo!(),
            // TypedExpression::AssignmentExpr(_) => todo!(),
            // TypedExpression::UnaryExpr(_) => todo!(),
            // TypedExpression::LogicExpr(_) => todo!(),
            // TypedExpression::UpdateExpr(_) => todo!(),
            _ => EvaluatedType::Unknown,
        }
    }

    /// Typechecks a new expression.
    fn typecheck_new_expression(
        newexp: &mut TypedNewExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        match &mut newexp.value {
            // Helper to fix code if new X is called without parenthesis.
            TypedExpression::Identifier(ident) => {
                let symbol = symboltable.get_forwarded(ident.value).unwrap();
                if matches!(symbol.kind, SemanticSymbolKind::Model { .. }) {
                    checker_ctx.add_error(errors::calling_new_on_identifier(
                        symbol.name.clone(),
                        Span::on_line(ident.start, symbol.name.len() as u32),
                    ));
                }
                return EvaluatedType::Unknown;
            }
            TypedExpression::CallExpr(callexp) => {
                let caller = &mut callexp.caller;
                let evaluated_caller = typecheck_expression(caller, checker_ctx, symboltable);
                let evaluated_args = callexp
                    .arguments
                    .iter_mut()
                    .map(|arg| typecheck_expression(arg, checker_ctx, symboltable))
                    .collect::<Vec<_>>();
                match evaluated_caller {
                    EvaluatedType::Model(model) => {
                        let model_symbol = symboltable.get_forwarded(model).unwrap();
                        let (mut generic_arguments, parameter_types) =
                            if let SemanticSymbolKind::Model {
                                generic_params,
                                is_constructable,
                                constructor_parameters,
                                ..
                            } = &model_symbol.kind
                            {
                                // if model does not have a new() function.
                                let name = model_symbol.name.clone();
                                let span = newexp.span;
                                if !*is_constructable {
                                    checker_ctx
                                        .add_error(errors::model_not_constructable(name, span));
                                    return EvaluatedType::Unknown;
                                }
                                let generic_arguments = evaluate_generic_params(generic_params);
                                let parameter_types = convert_param_list_to_type(
                                    constructor_parameters.as_ref().unwrap_or(&vec![]),
                                    symboltable,
                                    &generic_arguments,
                                    checker_ctx,
                                );
                                (generic_arguments, parameter_types)
                            } else {
                                unreachable!()
                            };
                        zip_arguments(
                            parameter_types,
                            evaluated_args,
                            checker_ctx,
                            &callexp,
                            symboltable,
                            &mut generic_arguments,
                        );
                        let result_model_instance = EvaluatedType::ModelInstance {
                            model,
                            generic_arguments,
                        };
                        return result_model_instance;
                    }
                    _ => {
                        checker_ctx.add_error(errors::invalid_new_expression(
                            checker_ctx.span_of_expr(&callexp.caller, &symboltable),
                        ));
                        return EvaluatedType::Unknown;
                    }
                }
            }
            // Invalid new expressions.
            _ => {
                checker_ctx.add_error(errors::invalid_new_expression(
                    checker_ctx.span_of_expr(&newexp.value, &symboltable),
                ));
                typecheck_expression(&mut newexp.value, checker_ctx, symboltable);
                return EvaluatedType::Unknown;
            }
        }
    }

    /// Converts a list of generic parameter indexes to a list of evaluated generic argument types.
    fn evaluate_generic_params(
        generic_params: &Vec<SymbolIndex>,
    ) -> Vec<(SymbolIndex, EvaluatedType)> {
        generic_params
            .iter()
            .map(|idx| (*idx, EvaluatedType::Generic { base: *idx }))
            .collect()
    }

    /// Typechecks an index expression.
    fn typecheck_index_expression(
        indexexp: &mut TypedIndexExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        let type_of_indexed = typecheck_expression(&mut indexexp.object, checker_ctx, symboltable);
        let _type_of_indexer = typecheck_expression(&mut indexexp.index, checker_ctx, symboltable);
        // todo: handle Index trait overloading.
        if !is_array(&type_of_indexed, symboltable) {
            checker_ctx.add_error(errors::invalid_index_subject(
                symboltable.format_evaluated_type(&type_of_indexed),
                indexexp.span,
            ));
            return EvaluatedType::Unknown;
        }
        // todo: check type of indexer as UnsignedInt.
        match type_of_indexed {
            EvaluatedType::ModelInstance {
                mut generic_arguments,
                ..
            } => {
                if generic_arguments.len() != 1 {
                    EvaluatedType::Unknown
                } else {
                    generic_arguments.remove(0).1
                }
            }
            _ => EvaluatedType::Unknown,
        }
    }

    /// Typechecks an array expression.
    fn typecheck_array_expression(
        array: &mut crate::TypedArrayExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        if symboltable.array_symbol.is_none() {
            checker_ctx.add_error(missing_intrinsic(format!("Array"), array.span));
        }
        let mut element_types = vec![];
        for element in &mut array.elements {
            element_types.push(typecheck_expression(element, checker_ctx, symboltable));
        }
        if element_types.len() == 0 {
            return arrify(EvaluatedType::Unknown, &symboltable);
        }
        // Reduce individual types to determine final array form.
        let mut next_type = element_types.remove(0);
        let mut i = 1;
        let mut errors_gotten = vec![];
        for evaluated_type in element_types {
            match unify_types(&next_type, &evaluated_type, symboltable, None) {
                Ok(new_type) => next_type = new_type,
                Err(mut errortypes) => {
                    errors_gotten.append(&mut errortypes);
                }
            };
            i += 1;
        }
        if errors_gotten.len() > 0 {
            checker_ctx.add_error(TypeError {
                _type: TypeErrorType::HeterogeneousArray,
                span: array.span,
            });
            for error in errors_gotten {
                checker_ctx.add_error(TypeError {
                    _type: error,
                    span: checker_ctx.span_of_expr(&array.elements[i], &symboltable),
                });
            }
        }
        arrify(next_type, symboltable)
    }

    /// Typechecks a literal value.
    fn typecheck_literal(
        l: &mut crate::LiteralIndex,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        match &checker_ctx.literals[l.0] {
            Literal::StringLiteral { value, .. } => {
                typecheck_string_literal(value, checker_ctx, symboltable)
            }
            Literal::NumericLiteral { .. } => EvaluatedType::Unknown, // todo.
            Literal::BooleanLiteral {
                value,
                start_line,
                start_character,
                ..
            } => typecheck_boolean_literal(
                symboltable,
                checker_ctx,
                start_line,
                start_character,
                value,
            ),
        }
    }

    /// Typechecks a bool literal by matching it with the bool intrinsic symbol.
    fn typecheck_boolean_literal(
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
        start_line: &u32,
        start_character: &u32,
        value: &bool,
    ) -> EvaluatedType {
        if let Some(bool_index) = symboltable.bool_symbol {
            return EvaluatedType::ModelInstance {
                model: bool_index,
                generic_arguments: vec![],
            };
        } else {
            checker_ctx.add_error(errors::missing_intrinsic(
                format!("Bool"),
                Span::on_line([*start_line, *start_character], if *value { 4 } else { 5 }),
            ));
            EvaluatedType::Unknown
        }
    }

    /// Typechecks a string literal by matching it with the string intrinsic symbol.
    fn typecheck_string_literal(
        value: &ast::WhirlString,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        if let Some(string_index) = symboltable.string_symbol {
            return EvaluatedType::ModelInstance {
                model: string_index,
                generic_arguments: vec![],
            };
        }
        // println!("could not find string in {:?}!!!", checker_ctx.path_idx);
        checker_ctx.add_error(errors::missing_intrinsic(format!("String"), value.span));
        return EvaluatedType::Unknown;
    }

    /// Typechecks a call expression.
    fn typecheck_call_expression(
        callexp: &mut TypedCallExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        let caller = typecheck_expression(&mut callexp.caller, checker_ctx, symboltable);
        let caller_span = checker_ctx.span_of_expr(&callexp.caller, &symboltable);
        let caller = extract_call_of(caller, symboltable, checker_ctx, caller_span);
        let evaluated_args = callexp
            .arguments
            .iter_mut()
            .map(|arg| typecheck_expression(arg, checker_ctx, symboltable))
            .collect::<Vec<_>>();
        if caller.is_unknown() {
            return caller;
        }
        // Extract parameters, generic arguments and return_type from caller.
        let (is_async, parameter_types, mut generic_arguments, mut return_type) = match caller {
            EvaluatedType::MethodInstance {
                method,
                mut generic_arguments,
            }
            | EvaluatedType::FunctionInstance {
                function: method,
                mut generic_arguments,
            } => {
                let method_symbol = symboltable.get(method).unwrap();
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
                            symboltable,
                            &generic_arguments,
                            checker_ctx,
                        );
                        let return_type = return_type
                            .as_ref()
                            .map(|typ| evaluate(typ, symboltable, None, &mut checker_ctx.tracker()))
                            .unwrap_or(EvaluatedType::Void);
                        (
                            *is_async,
                            parameter_types,
                            {
                                generic_arguments
                                    .append(&mut evaluate_generic_params(generic_params));
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
            } => (is_async, params, generic_args, *return_type),
            _ => {
                unreachable!("{caller:?} cannot be distilled, because it is not a functional type.")
            }
        };
        // Account for async functions.
        if is_async {
            return_type = prospectify(return_type, symboltable);
        }
        zip_arguments(
            parameter_types,
            evaluated_args,
            checker_ctx,
            &callexp,
            symboltable,
            &mut generic_arguments,
        );
        coerce(return_type, &generic_arguments)
    }

    /// This function unifies a list of function parameters with a list of call arguments
    /// And updates a generic argument with inference results.
    fn zip_arguments(
        parameter_types: Vec<ParameterType>,
        evaluated_args: Vec<EvaluatedType>,
        checker_ctx: &mut TypecheckerContext,
        callexp: &TypedCallExpr,
        symboltable: &mut SymbolTable,
        generic_arguments: &mut Vec<(SymbolIndex, EvaluatedType)>,
    ) {
        // mismatched arguments. It checks if the parameter list is longer, so it can account for optional parameters.
        if parameter_types.len() < evaluated_args.len() {
            checker_ctx.add_error(errors::mismatched_function_args(
                callexp.span,
                parameter_types.len(),
                evaluated_args.len(),
                None,
            ));
            return;
        }
        let mut generic_map = HashMap::new();
        let mut i = 0;
        while i < parameter_types.len() {
            let parameter_type = &parameter_types[i].inferred_type;
            // Account for optional types.
            let is_optional = parameter_types[i].is_optional;
            let argument_type = match evaluated_args.get(i) {
                Some(evaled_typ) => evaled_typ,
                None => {
                    if !is_optional {
                        checker_ctx.add_error(errors::mismatched_function_args(
                            callexp.span,
                            parameter_types.len(),
                            evaluated_args.len(),
                            parameter_types.iter().position(|param| param.is_optional),
                        ))
                    };
                    break;
                }
            };
            let unification = if is_optional {
                unify_types(
                    &maybify(parameter_type.clone(), symboltable),
                    argument_type,
                    symboltable,
                    Some(&mut generic_map),
                )
            } else {
                unify_types(
                    parameter_type,
                    argument_type,
                    symboltable,
                    Some(&mut generic_map),
                )
            };
            if let Err(errortype) = unification {
                for errortype in errortype {
                    checker_ctx.add_error(TypeError {
                        _type: errortype,
                        span: checker_ctx.span_of_expr(&callexp.arguments[i], &symboltable),
                    })
                }
            }
            i += 1;
        }
        // Solve generics with new evaluated types.
        for (generic, assigned_type) in generic_map {
            if let Some(entry) = generic_arguments
                .iter_mut()
                .find(|prior| prior.0 == generic)
            {
                entry.1 = assigned_type;
            } else {
                generic_arguments.push((generic, assigned_type));
            }
        }
    }

    fn convert_param_list_to_type(
        params: &Vec<SymbolIndex>,
        symboltable: &SymbolTable,
        solved_generics: &Vec<(SymbolIndex, EvaluatedType)>,
        checker_ctx: &mut TypecheckerContext,
    ) -> Vec<ParameterType> {
        params
            .iter()
            .map(|param| {
                let parameter_symbol = symboltable.get(*param).unwrap();
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
                                symboltable,
                                Some(solved_generics),
                                &mut checker_ctx.tracker(),
                            )
                        })
                        .unwrap_or(EvaluatedType::Unknown),
                }
            })
            .collect::<Vec<_>>()
    }

    fn extract_call_of(
        caller: EvaluatedType,
        symboltable: &mut SymbolTable,
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
                let symbol = symboltable.get_forwarded(base).unwrap();
                checker_ctx.add_error(errors::illegal_model_call(symbol.name.clone(), caller_span));
                EvaluatedType::Unknown
            }
            EvaluatedType::Module(_)
            | EvaluatedType::ModelInstance { .. }
            | EvaluatedType::TraitInstance { .. }
            | EvaluatedType::Trait(_)
            | EvaluatedType::Enum(_)
            | EvaluatedType::Generic { .. }
            | EvaluatedType::Void
            | EvaluatedType::Never
            | EvaluatedType::OpaqueTypeInstance { .. } => {
                checker_ctx.add_error(errors::not_callable(
                    symboltable.format_evaluated_type(&caller),
                    caller_span,
                ));
                EvaluatedType::Unknown
            }
            EvaluatedType::Borrowed { base } => {
                let mut caller = *base;
                while let EvaluatedType::Borrowed { base: inner } = caller {
                    caller = *inner
                }
                extract_call_of(caller, symboltable, checker_ctx, caller_span)
            }
            _ => EvaluatedType::Unknown,
        };
        caller
    }

    /// Typechecks a function expression.
    fn typecheck_function_expression(
        f: &mut TypedFnExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        let mut parameter_types = vec![];
        for param in &f.params {
            let parameter_symbol = symboltable.get_forwarded(*param).unwrap();
            let (param_type, is_optional) = match &parameter_symbol.kind {
                SemanticSymbolKind::Parameter {
                    param_type,
                    is_optional,
                    ..
                } => (param_type, *is_optional),
                _ => unreachable!(),
            };
            parameter_types.push(ParameterType {
                name: parameter_symbol.name.clone(),
                is_optional,
                type_label: param_type.clone(),
                inferred_type: param_type
                    .as_ref()
                    .map(|typ| evaluate(typ, symboltable, None, &mut checker_ctx.tracker()))
                    .unwrap_or(EvaluatedType::Unknown),
            });
        }
        let return_type = f
            .return_type
            .as_ref()
            .map(|typ| evaluate(typ, &symboltable, None, &mut checker_ctx.tracker()));
        let inferred_return_type = match &mut f.body {
            // Function body is scoped to block.
            TypedExpression::Block(block) => {
                checker_ctx
                    .current_function_return_type
                    .push(CurrentFunctionContext {
                        is_named: false,
                        return_type: return_type.unwrap_or(EvaluatedType::Unknown),
                    });
                let ev_typ = typecheck_block(block, true, checker_ctx, symboltable);
                checker_ctx.current_function_return_type.pop();
                ev_typ
            }
            expression => typecheck_expression(expression, checker_ctx, symboltable),
        };
        EvaluatedType::FunctionExpressionInstance {
            is_async: f.is_async,
            params: parameter_types,
            generic_args: vec![], // todo.
            return_type: Box::new(inferred_return_type),
        }
    }

    /// Typechecks an access expression.
    fn typecheck_access_expression(
        access: &mut TypedAccessExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        let object_type = typecheck_expression(&mut access.object, checker_ctx, symboltable);
        let property_symbol_idx = match &access.property {
            TypedExpression::Identifier(i) => i.value,
            _ => unreachable!(),
        };
        extract_property_of(
            object_type,
            symboltable,
            property_symbol_idx,
            checker_ctx,
            access,
        )
    }

    /// Extract a property based on the value of its object.
    fn extract_property_of(
        object_type: EvaluatedType,
        symboltable: &mut SymbolTable,
        property_symbol_idx: SymbolIndex,
        checker_ctx: &mut TypecheckerContext,
        access: &mut TypedAccessExpr,
    ) -> EvaluatedType {
        let property_span = symboltable.get(property_symbol_idx).unwrap().ident_span();
        match object_type {
            // Accessing a property on a model instance.
            EvaluatedType::ModelInstance {
                model,
                ref generic_arguments,
            } => search_model_for_property(
                checker_ctx,
                symboltable,
                model,
                property_symbol_idx,
                generic_arguments.clone(),
                true,
                property_span,
            ),
            EvaluatedType::Model(model) => {
                let symbol = symboltable.get_forwarded(model).unwrap();
                let object_is_instance = false;
                // The generic arguments is an unknown list from the generic parameters.
                let generic_arguments = match &symbol.kind {
                    SemanticSymbolKind::Model { generic_params, .. } => {
                        evaluate_generic_params(generic_params)
                    }
                    _ => vec![],
                };
                search_model_for_property(
                    checker_ctx,
                    symboltable,
                    model,
                    property_symbol_idx,
                    generic_arguments,
                    object_is_instance,
                    property_span,
                )
            }
            // EvaluatedType::Trait(_) => todo!(),
            // EvaluatedType::Enum(_) => todo!(),
            // EvaluatedType::Module(_) => todo!(),
            // EvaluatedType::OpaqueType {
            //     methods,
            //     properties,
            //     implementations,
            //     collaborators,
            // } => todo!(),
            EvaluatedType::Borrowed { base } => {
                return extract_property_of(
                    *base,
                    symboltable,
                    property_symbol_idx,
                    checker_ctx,
                    access,
                )
            }
            _ => None,
        }
        .unwrap_or_else(|| {
            let property_symbol = symboltable.get_forwarded(property_symbol_idx).unwrap();
            let error = TypeErrorType::NoSuchProperty {
                base_type: symboltable.format_evaluated_type(&object_type),
                property: property_symbol.name.clone(),
            };
            checker_ctx.add_error(TypeError {
                _type: error,
                span: checker_ctx.span_of_expr(&access.property, &symboltable),
            });
            EvaluatedType::Unknown
        })
    }

    fn search_model_for_property(
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
        model: SymbolIndex,
        property_symbol_idx: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
        object_is_instance: bool,
        property_span: Span,
    ) -> Option<EvaluatedType> {
        let base_model_symbol = symboltable.get_forwarded(model).unwrap();
        let property_symbol = symboltable.get_forwarded(property_symbol_idx).unwrap();
        let (methods, attributes) = match &base_model_symbol.kind {
            SemanticSymbolKind::Model {
                methods,
                attributes,
                ..
            } => (methods, attributes),
            _ => return Some(EvaluatedType::Unknown),
        };
        for method in methods {
            let method = *method;
            let method_symbol = symboltable.get_forwarded(method).unwrap();
            if method_symbol.name == property_symbol.name {
                let method_is_static = match &method_symbol.kind {
                    SemanticSymbolKind::Method { is_static, .. } => *is_static,
                    _ => false,
                };
                if method_is_static && object_is_instance {
                    checker_ctx.add_error(errors::instance_static_method_access(
                        base_model_symbol.name.clone(),
                        method_symbol.name.clone(),
                        property_span,
                    ))
                } else if !method_is_static && !object_is_instance {
                    checker_ctx.add_error(errors::contructor_non_static_method_access(
                        base_model_symbol.name.clone(),
                        method_symbol.name.clone(),
                        property_span,
                    ))
                }
                // get mutably.
                let property_symbol = symboltable.get_mut(property_symbol_idx).unwrap();
                if let SemanticSymbolKind::Property { resolved } = &mut property_symbol.kind {
                    *resolved = Some(method)
                }
                // todo: Is method public?
                return Some(EvaluatedType::MethodInstance {
                    method,
                    generic_arguments,
                });
            }
        }
        // Is property an attribute?
        for attribute in attributes {
            let attribute = *attribute;
            let attribute_symbol = symboltable.get_forwarded(attribute).unwrap();
            if attribute_symbol.name == property_symbol.name {
                let result_type = match &attribute_symbol.kind {
                    // todo: Is attribute public?
                    SemanticSymbolKind::Attribute { declared_type, .. } => evaluate(
                        &declared_type,
                        symboltable,
                        Some(&generic_arguments),
                        &mut checker_ctx.tracker(),
                    ),
                    _ => return Some(EvaluatedType::Unknown),
                };
                // get mutably.
                let property_symbol = symboltable.get_mut(property_symbol_idx).unwrap();
                if let SemanticSymbolKind::Property { resolved } = &mut property_symbol.kind {
                    *resolved = Some(attribute)
                }
                return Some(result_type);
            }
        }
        None
    }

    /// Typechecks an identifier.
    fn typecheck_identifier(
        i: &mut TypedIdent,
        symboltable: &mut SymbolTable,
    ) -> Result<EvaluatedType, TypeErrorType> {
        let name = symboltable.forward(i.value);
        let symbol = symboltable.get_forwarded(name).unwrap();
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
            SemanticSymbolKind::Attribute { .. } | SemanticSymbolKind::Method { .. } => {
                unreachable!("properties cannot exist independent of a model.")
            }
            SemanticSymbolKind::Parameter {
                is_optional,
                inferred_type,
                ..
            } => {
                if *is_optional {
                    maybify(inferred_type.clone(), symboltable)
                } else {
                    inferred_type.clone()
                }
            }
            SemanticSymbolKind::GenericParameter { .. } | SemanticSymbolKind::TypeName { .. } => {
                return Err(TypeErrorType::TypeAsValue {
                    type_: symbol.name.clone(),
                });
            }
            SemanticSymbolKind::Function { generic_params, .. } => {
                EvaluatedType::FunctionInstance {
                    function: name,
                    generic_arguments: evaluate_generic_params(generic_params),
                }
            } //TODO
            _ => EvaluatedType::Unknown,
        };
        Ok(eval_type)
    }

    /// Typechecks a block expression.
    pub fn typecheck_block(
        body: &mut TypedBlock,
        is_function_block: bool,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        let mut loopindex = 0;
        let statements = &mut body.statements;
        let no_of_statements = statements.len();
        while loopindex < no_of_statements {
            let statement = &mut statements[loopindex];
            if loopindex == no_of_statements - 1 {
                // Returns the type of the last expression in the block.
                // todo: also check for expression statements for diagnostics.
                if let TypedStmnt::FreeExpression(expression) = statement {
                    let expression_type =
                        typecheck_expression(expression, checker_ctx, symboltable);
                    return expression_type;
                } else if is_function_block {
                    // todo: should never out.
                    if let TypedStmnt::ReturnStatement(TypedReturnStatement {
                        value: Some(expression),
                        ..
                    }) = statement
                    {
                        let expression_type =
                            typecheck_expression(expression, checker_ctx, symboltable);
                        return expression_type;
                    }
                }
            }
            statements::typecheck_statement(statement, checker_ctx, symboltable);
            loopindex += 1;
        }
        EvaluatedType::Void
    }
}
