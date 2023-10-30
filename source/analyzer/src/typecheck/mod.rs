#![allow(unused)]
mod tests;
pub mod unify;

use ast::Span;
use errors::{TypeError, TypeErrorType};
use unify::unify;

use crate::{
    EvaluatedType, Literal, PathIndex, ProgramError, SemanticSymbolKind, SymbolIndex, SymbolTable,
    TypedBlock, TypedExpression, TypedFunctionDeclaration, TypedModule, TypedStmnt,
};

/// The typechecker is the second pass of the analyzer,
/// used for infering and validating the use of data types within a module.
/// It also does some control flow analysis, validates return types,
/// solves generics and ensures valid property assignment and use.
pub struct TypecheckerContext {
    path_idx: PathIndex,
    /// The return type of the closest function scope currently being typechecked.
    current_function_return_type: Vec<CurrentFunctionContext>,
    // /// The list of symbols in the current block that still have a type that resolves to unknown.
    // never_type: Vec<SymbolIndex>,
}

pub struct CurrentFunctionContext {
    /// Whether it is a named function or a function expression.
    is_named: bool,
    return_type: EvaluatedType,
}

impl TypecheckerContext {
    pub fn add_error(&self, error: TypeError, errors: &mut Vec<ProgramError>) {
        errors.push(ProgramError {
            offending_file: self.path_idx,
            error_type: crate::ProgramErrorType::Typing(error),
        })
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
        current_function_return_type: vec![],
        // unknowns: vec![],
    };
    for statement in &mut module.statements {
        statements::typecheck_statement(statement, &mut checker_ctx, symboltable, errors, literals);
    }
}

fn span_of_expr(expression: &TypedExpression, table: &SymbolTable, literals: &[Literal]) -> Span {
    match expression {
        TypedExpression::Identifier(i) => {
            let symbol = table.get(i.value).unwrap();
            Span::on_line(i.start, symbol.name.len() as u32)
        }
        TypedExpression::Literal(l) => match &literals[l.0] {
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

fn span_of_statement(s: &TypedStmnt, table: &mut SymbolTable, literals: &[Literal]) -> Span {
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
            span_of_expr(e, table, literals)
        }
        TypedStmnt::ShorthandVariableDeclaration(v) => v.span,
        TypedStmnt::ModuleDeclaration(m) => m.span,
        TypedStmnt::ReturnStatement(r) => r.span,
        TypedStmnt::ContinueStatement(c) => c.span,
        TypedStmnt::BreakStatement(b) => b.span,
    }
}

mod statements {
    use super::*;
    use crate::{IntermediateType, TypedReturnStatement};
    use errors::TypeErrorType;

    pub fn typecheck_statement(
        statement: &mut TypedStmnt,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &[Literal],
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
                    errors,
                    literals,
                )
            }
            // TypedStmnt::ConstantDeclaration(_) => todo!(),
            // TypedStmnt::TypeDeclaration(_) => todo!(),
            // TypedStmnt::ModelDeclaration(_) => todo!(),
            // TypedStmnt::ModuleDeclaration(_) => todo!(),
            TypedStmnt::FunctionDeclaration(function) => {
                typecheck_function(function, checker_ctx, symboltable, errors, literals)
            }
            // TypedStmnt::TraitDeclaration(_) => todo!(),
            TypedStmnt::ExpressionStatement(expression)
            | TypedStmnt::FreeExpression(expression) => {
                expressions::typecheck_expression(
                    expression,
                    checker_ctx,
                    symboltable,
                    errors,
                    literals,
                );
            }
            TypedStmnt::ReturnStatement(retstat) => {
                typecheck_return_statement(retstat, checker_ctx, symboltable, errors, literals);
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
        errors: &mut Vec<ProgramError>,
        literals: &[Literal],
    ) {
        let name = shorthand_variable.name;
        let symbol = symboltable.get(name).unwrap();
        // First evaluate the declared type, if any,
        let declared_type = if let SemanticSymbolKind::Variable {
            pattern_type,
            declared_type,
            inferred_type,
            ..
        } = &symbol.kind
        {
            declared_type
                .as_ref()
                .map(|typ| unify::evaluate(typ, symboltable, None))
        } else {
            None
        };
        let type_of_value = expressions::typecheck_expression(
            &mut shorthand_variable.value,
            checker_ctx,
            symboltable,
            errors,
            literals,
        );
        // if no declared type, just assign to variable.
        // else attempt unification.
        // If unification fails, then carry on with the declared type value.
        let inference_result = if let Some(value) = declared_type {
            match unify(&value, &type_of_value, symboltable) {
                Ok(eval_type) => eval_type,
                Err(error) => {
                    checker_ctx.add_error(
                        TypeError {
                            _type: error,
                            span: shorthand_variable.span,
                        },
                        errors,
                    );
                    value
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
        errors: &mut Vec<ProgramError>,
        literals: &[Literal],
    ) {
        let maybe_eval_expr = retstat.value.as_mut().map(|expr| {
            expressions::typecheck_expression(expr, checker_ctx, symboltable, errors, literals)
        });
        let function_context = checker_ctx.current_function_return_type.last();
        if let Some(eval_type) = &maybe_eval_expr {
            if function_context.is_none()
                || function_context.is_some_and(|ctx| ctx.is_named && ctx.return_type.is_void())
            {
                // returns with a value, but no value was requested.
                checker_ctx.add_error(
                    TypeError {
                        _type: TypeErrorType::MismatchedReturnType {
                            expected: symboltable.format_evaluated_type(&EvaluatedType::Void),
                            found: symboltable.format_evaluated_type(eval_type),
                        },
                        span: retstat.span,
                    },
                    errors,
                )
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
                match unify(eval_type, ctx_return_type, symboltable) {
                    // Unification was successful and return type can be updated.
                    Ok(typ) => {
                        let prior_evaluated_type =
                            checker_ctx.current_function_return_type.last_mut().unwrap();
                        prior_evaluated_type.return_type = typ;
                    }
                    // Unification failed.
                    Err(errortype) => {
                        checker_ctx.add_error(
                            TypeError {
                                _type: errortype,
                                span: retstat.span,
                            },
                            errors,
                        );
                        checker_ctx.add_error(
                            TypeError {
                                _type: TypeErrorType::MismatchedReturnType {
                                    expected: symboltable.format_evaluated_type(&ctx_return_type),
                                    found: symboltable.format_evaluated_type(eval_type),
                                },
                                span: retstat.span,
                            },
                            errors,
                        )
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
        errors: &mut Vec<ProgramError>,
        literals: &[Literal],
    ) {
        let symbol = symboltable.get(function.name).unwrap();
        let (return_type, return_type_span) = if let SemanticSymbolKind::Function {
            is_async,
            params,
            generic_params,
            return_type,
            ..
        } = &symbol.kind
        {
            let return_type = return_type.as_ref();
            (
                return_type
                    .map(|typ| unify::evaluate(typ, &symboltable, None))
                    .unwrap_or_else(|| EvaluatedType::Void),
                return_type.map(|typ| typ.span()),
            )
        } else {
            (EvaluatedType::Void, None)
        };
        let is_named_function = true;
        checker_ctx
            .current_function_return_type
            .push(CurrentFunctionContext {
                is_named: true,
                return_type: return_type.clone(),
            });
        let block_return_type = expressions::typecheck_block(
            &mut function.body,
            true,
            checker_ctx,
            symboltable,
            errors,
            literals,
        );
        if let Err(typeerrortype) = unify(&block_return_type, &return_type, &symboltable) {
            let span = return_type_span
                .or_else(|| {
                    function
                        .body
                        .statements
                        .last()
                        .map(|s| span_of_statement(s, symboltable, literals))
                })
                .unwrap_or_else(|| function.body.span);
            checker_ctx.add_error(
                TypeError {
                    _type: typeerrortype,
                    span,
                },
                errors,
            );
            checker_ctx.add_error(
                TypeError {
                    _type: TypeErrorType::MismatchedReturnType {
                        found: symboltable.format_evaluated_type(&block_return_type),
                        expected: symboltable.format_evaluated_type(&return_type),
                    },
                    span,
                },
                errors,
            );
        }
        checker_ctx.current_function_return_type.pop();
    }

    // fn typecheck_generic_parameter()
}

mod expressions {
    use super::*;
    use crate::{unify::evaluate, TypedExpression, TypedFnExpr, TypedIdent, TypedReturnStatement};
    use ast::Span;

    /// Typechecks an expression.
    pub fn typecheck_expression(
        expression: &mut crate::TypedExpression,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &[Literal],
    ) -> EvaluatedType {
        match expression {
            TypedExpression::Identifier(i) => typecheck_identifier(i, symboltable),
            // TypedExpression::Literal(_) => todo!(),
            // TypedExpression::NewExpr(newexp) => todo!(),
            // TypedExpression::ThisExpr(_) => todo!(),
            // TypedExpression::CallExpr(_) => todo!(),
            TypedExpression::FnExpr(f) => {
                typecheck_function_expression(&mut *f, symboltable, checker_ctx, errors, literals)
            }
            // TypedExpression::Block(_) => todo!(),
            // TypedExpression::IfExpr(_) => todo!(),
            // TypedExpression::AccessExpr(_) => todo!(),
            // TypedExpression::ArrayExpr(_) => todo!(),
            // TypedExpression::IndexExpr(_) => todo!(),
            // TypedExpression::BinaryExpr(_) => todo!(),
            // TypedExpression::AssignmentExpr(_) => todo!(),
            // TypedExpression::UnaryExpr(_) => todo!(),
            // TypedExpression::LogicExpr(_) => todo!(),
            // TypedExpression::UpdateExpr(_) => todo!(),
            _ => EvaluatedType::Unknown,
        }
    }

    /// Typechecks a function expression.
    fn typecheck_function_expression(
        f: &mut TypedFnExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
        errors: &mut Vec<ProgramError>,
        literals: &[Literal],
    ) -> EvaluatedType {
        let mut evaluated_params = vec![];
        for param in &f.params {
            let parameter_symbol = symboltable.get(*param).unwrap();
            let param_type = match &parameter_symbol.kind {
                SemanticSymbolKind::Parameter {
                    is_optional,
                    param_type,
                } => param_type,
                _ => unreachable!(),
            };
            match param_type {
                Some(typ) => evaluated_params.push((*param, evaluate(typ, symboltable, None))),
                None => evaluated_params.push((*param, EvaluatedType::Unknown)),
            }
        }
        let return_type = f
            .return_type
            .as_ref()
            .map(|typ| evaluate(typ, &symboltable, None));
        let inferred_return_type = match &mut f.body {
            // Function body is scoped to block.
            TypedExpression::Block(block) => {
                checker_ctx
                    .current_function_return_type
                    .push(CurrentFunctionContext {
                        is_named: false,
                        return_type: return_type.unwrap_or(EvaluatedType::Unknown),
                    });
                let ev_typ =
                    typecheck_block(block, true, checker_ctx, symboltable, errors, literals);
                checker_ctx.current_function_return_type.pop();
                ev_typ
            }
            expression => {
                typecheck_expression(expression, checker_ctx, symboltable, errors, literals)
            }
        };
        EvaluatedType::FunctionExpression {
            is_async: f.is_async,
            params: evaluated_params,
            return_type: Box::new(inferred_return_type),
        }
    }

    /// Typechecks an identifier.
    fn typecheck_identifier(i: &mut TypedIdent, symboltable: &mut SymbolTable) -> EvaluatedType {
        let name = i.value;
        match &symboltable.get_forwarded(name).unwrap().kind {
            SemanticSymbolKind::Module { .. } => EvaluatedType::Module(name),
            SemanticSymbolKind::Trait { .. } => EvaluatedType::Trait(name),
            SemanticSymbolKind::Model { .. } => EvaluatedType::Module(name),
            SemanticSymbolKind::Enum { .. } => EvaluatedType::Enum(name),
            SemanticSymbolKind::Variant {
                owner_enum,
                variant_index,
                tagged_types,
            } => EvaluatedType::EnumInstance {
                enum_: *owner_enum,
                generic_arguments: {
                    // Try to create a space for unknown enum generics.
                    // todo: unify from tagged types.
                    let enum_symbol = symboltable.get(*owner_enum).unwrap();
                    match &enum_symbol.kind {
                        SemanticSymbolKind::Enum { generic_params, .. } => generic_params
                            .iter()
                            .map(|_| EvaluatedType::Unknown)
                            .collect(),
                        _ => vec![],
                    }
                },
            },
            SemanticSymbolKind::Variable { inferred_type, .. } => inferred_type.clone(),
            SemanticSymbolKind::Constant { inferred_type, .. } => inferred_type.clone(),
            SemanticSymbolKind::Attribute { inferred_type, .. } => inferred_type.clone(),
            SemanticSymbolKind::Method {
                owner_model_or_trait,
                generic_params,
                ..
            } => EvaluatedType::Method {
                method: name,
                trait_or_model_generic_arguments: {
                    let owner_symbol = symboltable.get(*owner_model_or_trait).unwrap();
                    match &owner_symbol.kind {
                        SemanticSymbolKind::Model { generic_params, .. }
                        | SemanticSymbolKind::Trait { generic_params, .. } => generic_params
                            .iter()
                            .map(|_| EvaluatedType::Unknown)
                            .collect(),
                        _ => vec![],
                    }
                },
                generic_arguments: generic_params
                    .iter()
                    .map(|_| EvaluatedType::Unknown)
                    .collect(),
            },
            SemanticSymbolKind::Parameter {
                is_optional,
                param_type,
            } => param_type
                .as_ref()
                .map(|typ| unify::evaluate(typ, symboltable, None))
                .unwrap_or(EvaluatedType::Unknown),
            SemanticSymbolKind::GenericParameter {
                traits,
                default_value,
            } => EvaluatedType::Generic {
                base: name,
                traits: traits
                    .iter()
                    .map(|_trait| unify::evaluate(_trait, symboltable, None))
                    .collect(),
            },
            SemanticSymbolKind::Function { generic_params, .. } => EvaluatedType::Function {
                function: name,
                generic_arguments: generic_params
                    .iter()
                    .map(|_| EvaluatedType::Unknown)
                    .collect(),
            },
            SemanticSymbolKind::TypeName {
                is_public,
                generic_params,
                value,
            } => EvaluatedType::Unknown, //TODO
            _ => EvaluatedType::Unknown,
        }
    }

    /// Typechecks a block expression.
    pub fn typecheck_block(
        body: &mut TypedBlock,
        is_function_block: bool,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &[Literal],
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
                    let expression_type = typecheck_expression(
                        expression,
                        checker_ctx,
                        symboltable,
                        errors,
                        literals,
                    );
                    return expression_type;
                } else if is_function_block {
                    // todo: should never out.
                    if let TypedStmnt::ReturnStatement(TypedReturnStatement {
                        value: Some(expression),
                        ..
                    }) = statement
                    {
                        let expression_type = typecheck_expression(
                            expression,
                            checker_ctx,
                            symboltable,
                            errors,
                            literals,
                        );
                        return expression_type;
                    }
                }
            } else {
                statements::typecheck_statement(
                    statement,
                    checker_ctx,
                    symboltable,
                    errors,
                    literals,
                );
            }
            loopindex += 1;
        }
        EvaluatedType::Void
    }
}
