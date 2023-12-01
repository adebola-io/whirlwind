use crate::{
    evaluate, span_of_typed_expression, span_of_typed_statement,
    unify::{unify_freely, unify_types},
    utils::{
        arrify, coerce, coerce_all_generics, ensure_assignment_validity, evaluate_generic_params,
        get_implementation_of, get_numeric_type, get_type_generics, infer_ahead, is_array,
        is_boolean, is_numeric_type, maybify, prospectify, symbol_to_type, update_expression_type,
    },
    EvaluatedType, Literal, LiteralMap, ParameterType, PathIndex, ProgramError, ScopeId,
    SemanticSymbolKind, SymbolIndex, SymbolTable, TypedAccessExpr, TypedAssignmentExpr, TypedBlock,
    TypedCallExpr, TypedExpression, TypedFnExpr, TypedFunctionDeclaration, TypedIdent, TypedIfExpr,
    TypedIndexExpr, TypedLogicExpr, TypedModule, TypedNewExpr, TypedReturnStatement, TypedStmnt,
    TypedThisExpr, UnifyOptions,
};
use ast::{Span, UnaryOperator};
use errors::{missing_intrinsic, TypeError, TypeErrorType};
use std::collections::HashMap;

/// The typechecker is the second pass of the analyzer,
/// used for infering and validating the use of data types within a module.
/// It also does some control flow analysis, validates return types,
/// solves generics and ensures valid property assignment and use.
pub struct TypecheckerContext<'a> {
    path_idx: PathIndex,
    /// The context of the closest function scope currently being typechecked.
    current_function_context: Vec<CurrentFunctionContext>,
    /// The context of the closest constructor currently being typechecked.
    current_constructor_context: Vec<CurrentConstructorContext>,
    /// The model or trait that encloses the current statement.
    enclosing_model_or_trait: Option<SymbolIndex>,
    /// List of errors from the standpoint.
    errors: &'a mut Vec<ProgramError>,
    /// List of literal types from the standpoint.
    literals: &'a LiteralMap,
    // /// Cached values of intermediate types,
    // /// so they do not have to be evaluated over and over.
    // intermediate_types: HashMap<IntermediateType, EvaluatedType>,
}

#[derive(Clone)]
pub struct CurrentFunctionContext {
    /// Whether it is a named function or a function expression.
    is_named: bool,
    return_type: EvaluatedType,
}

pub struct CurrentConstructorContext {
    scopes: Vec<ScopeType>,
    attributes: HashMap<SymbolIndex, Vec<AttributeAssignment>>,
}

pub enum ScopeType {
    IfBlock { id: ScopeId },
    ElseBlock { id_of_parent_if: ScopeId },
    Other,
}

pub enum AttributeAssignment {
    /// The attribute is propertly assigned in the constructor scope.
    Definite {
        span: Span,
    },
    /// The attribute is assigned somewhere in an if block.
    InIfBlock {
        id: ScopeId,
    },
    SomewhereElse,
}
impl AttributeAssignment {
    fn is_definite(&self) -> bool {
        matches!(self, AttributeAssignment::Definite { .. })
    }
}

impl<'a> TypecheckerContext<'a> {
    /// Adds a type error to the owner standpoint's list of errors.
    pub fn add_error(&mut self, error: TypeError) {
        self.errors.push(ProgramError {
            offending_file: self.path_idx,
            error_type: crate::ProgramErrorType::Typing(error),
        })
    }
    /// Returns a reference to the error list to be passed around by the evaluator.
    pub fn tracker(&mut self) -> Option<(&mut Vec<ProgramError>, PathIndex)> {
        Some((self.errors, self.path_idx))
    }
    /// Calculates the span of a typed expression using the symboltable and the list of literals.
    fn span_of_expr(&self, expression: &TypedExpression, symboltable: &SymbolTable) -> Span {
        span_of_typed_expression(expression, symboltable, self.literals)
    }
    /// Calculates the span of a statement using the symboltable and the list of literals.
    fn span_of_stmnt(&self, s: &TypedStmnt, symboltable: &mut SymbolTable) -> Span {
        span_of_typed_statement(s, symboltable, self.literals)
    }
}

/// Typechecks a module.
pub fn typecheck(
    module: &mut TypedModule,
    symboltable: &mut SymbolTable,
    errors: &mut Vec<ProgramError>,
    literals: &LiteralMap,
) {
    let mut checker_ctx = TypecheckerContext {
        path_idx: module.path_idx,
        current_function_context: Vec::new(),
        current_constructor_context: Vec::new(),
        enclosing_model_or_trait: None,
        errors,
        literals,
    };
    for statement in &mut module.statements {
        statements::typecheck_statement(statement, &mut checker_ctx, symboltable);
    }
}

pub fn pop_scopetype(checker_ctx: &mut TypecheckerContext<'_>) {
    let mut constructor_context = checker_ctx.current_constructor_context.last_mut();
    constructor_context.as_mut().map(|ctx| ctx.scopes.pop());
}

pub fn push_scopetype(checker_ctx: &mut TypecheckerContext<'_>, scope: ScopeType) {
    let mut constructor_context = checker_ctx.current_constructor_context.last_mut();
    constructor_context
        .as_mut()
        .map(|ctx| ctx.scopes.push(scope));
}

mod statements {
    use crate::TypedModelDeclaration;

    use super::{expressions::typecheck_block, *};
    pub fn typecheck_statement(
        statement: &mut TypedStmnt,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) {
        match statement {
            // TypedStmnt::RecordDeclaration => todo!(),
            TypedStmnt::TestDeclaration(test) => {
                push_scopetype(checker_ctx, ScopeType::Other);
                typecheck_block(&mut test.body, false, checker_ctx, symboltable);
                pop_scopetype(checker_ctx);
            }
            // TypedStmnt::EnumDeclaration(_) => todo!(),
            TypedStmnt::VariableDeclaration(variable) => {
                typecheck_variable_declaration(variable, checker_ctx, symboltable)
            }
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
            TypedStmnt::WhileStatement(whil) => {
                typecheck_while_statement(whil, checker_ctx, symboltable)
            }
            TypedStmnt::ModelDeclaration(model) => {
                typecheck_model_declaration(model, checker_ctx, symboltable)
            }
            _ => {}
        }
    }

    /// Typechecks a model declaration.
    fn typecheck_model_declaration(
        model: &mut TypedModelDeclaration,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) {
        let model_symbol = symboltable.get(model.name);
        if model_symbol.is_none() {
            return;
        }
        let mut model_symbol = model_symbol.unwrap();
        // Signifies to the checker context that we are now in model X, so private properties can be used.
        let former_enclosing_model_trait = checker_ctx.enclosing_model_or_trait.take();
        checker_ctx.enclosing_model_or_trait = Some(model.name);
        // If the model has a constructor:
        if let Some(constructor) = &mut model.body.constructor {
            // For a model to be validly constructed, all its attributes have been definitively assigned in its constructor.
            // i.e. for every attribute, there must be an assignment expression (with =) where the attribute is the lhs.
            // question: What about in cases where the attribute is used before it is assigned? e.g.:
            // this.a = this.b;
            // this.b = someValue;
            // answer: All instances of the attribute are recorded and tracked. If the first instance is not an assignment, error.
            // NOTE: if the type of the attribute implements Default, then this check is not needed.
            model_symbol = symboltable.get(model.name).unwrap();
            let attribute_idxs =
                if let SemanticSymbolKind::Model { attributes, .. } = &model_symbol.kind {
                    attributes
                } else {
                    return;
                };
            let mut attributes = HashMap::new();
            for idx in attribute_idxs {
                let idx = *idx;
                attributes.insert(idx, Vec::new());
            }
            checker_ctx
                .current_constructor_context
                .push(CurrentConstructorContext {
                    scopes: Vec::new(),
                    attributes,
                });
            // Constructors should always return void.
            checker_ctx
                .current_function_context
                .push(CurrentFunctionContext {
                    is_named: true,
                    return_type: EvaluatedType::Void,
                });
            let block_type = typecheck_block(constructor, true, checker_ctx, symboltable);
            if !block_type.is_void() && !block_type.is_never() {
                let span = constructor
                    .statements
                    .last()
                    .map(|statement| checker_ctx.span_of_stmnt(statement, symboltable))
                    .unwrap_or_else(|| constructor.span);
                checker_ctx.add_error(errors::return_from_constructor(span));
            }
            checker_ctx.current_function_context.pop();
            let constructor_context = checker_ctx.current_constructor_context.pop().unwrap();
            for (attribute_idx, assignments) in constructor_context.attributes {
                let attribute_symbol = symboltable.get(attribute_idx);
                if attribute_symbol.is_none() {
                    continue;
                }

                let attribute_symbol = attribute_symbol.unwrap();
                if let Some(AttributeAssignment::Definite { span }) = assignments
                    .iter()
                    .find(|assignment| matches!(assignment, AttributeAssignment::Definite { .. }))
                {
                    let assignment_span = *span;
                    // Checks for prior usage before assignment with the spans.
                    // todo: something about using spans is icky.
                    let symbol_reference_list = attribute_symbol.references.first().unwrap(); // References in this module.
                    let reference_starts_in_constructor_block =
                        symbol_reference_list.starts.iter().filter_map(|start| {
                            let start = *start;
                            constructor.span.contains(start).then(|| start)
                        });
                    for start in reference_starts_in_constructor_block {
                        let reference_span =
                            Span::on_line(start, attribute_symbol.name.len() as u32);
                        if reference_span.is_before(assignment_span) {
                            checker_ctx
                                .add_error(errors::using_attribute_before_assign(reference_span));
                            break;
                        }
                    }
                } else {
                    let mut should_error = true;
                    let inferred_type = match &attribute_symbol.kind {
                        SemanticSymbolKind::Attribute { declared_type, .. } => evaluate(
                            declared_type,
                            symboltable,
                            None,
                            &mut checker_ctx.tracker(),
                            0,
                        ),
                        _ => EvaluatedType::Unknown,
                    };
                    let default_symbol = symboltable.default;
                    // Type implements Default, so carry on.
                    if default_symbol.is_some() {
                        if get_implementation_of(
                            default_symbol.unwrap(),
                            &inferred_type,
                            symboltable,
                        )
                        .is_some()
                        {
                            should_error = false;
                        }
                    }
                    if should_error {
                        checker_ctx
                            .add_error(errors::unassigned_attribute(attribute_symbol.origin_span));
                    }
                }
            }
        }
        // Check that the method names inherited from traits do not clash with other.
        // if let SemanticSymbolKind::Model {
        //     is_constructable,
        //     generic_params,
        //     constructor_parameters,
        //     implementations,
        //     methods,
        //     attributes,
        //     ..
        // } = &model_symbol.kind
        // {}
        checker_ctx.enclosing_model_or_trait = former_enclosing_model_trait;
    }

    /// Typechecks a variable declaration.
    fn typecheck_variable_declaration(
        variable: &mut crate::TypedVariableDeclaration,
        checker_ctx: &mut TypecheckerContext<'_>,
        symboltable: &mut SymbolTable,
    ) {
        let names = &variable.names;
        if variable.names.len() == 0 {
            if let Some(expr) = variable.value.as_mut() {
                expressions::typecheck_expression(expr, checker_ctx, symboltable);
                // for continuity.
            }
            return;
        }
        // If there is no value assigned, there must be a type label, and the given type must implement Default.
        // If there is no type assigned, there must be a value assigned.
        // Bidirectional inferencing is still needed, so the first variable declared is used as a foundation
        // for checking others.
        let declared_type = {
            let symbol = symboltable.get_forwarded(variable.names[0]).unwrap();
            let declared_type =
                if let SemanticSymbolKind::Variable { declared_type, .. } = &symbol.kind {
                    declared_type
                } else {
                    if let Some(expr) = variable.value.as_mut() {
                        expressions::typecheck_expression(expr, checker_ctx, symboltable);
                        // for continuity.
                    }
                    return;
                };
            if declared_type.is_some() {
                // Currently on the first type.
                let declared_type = declared_type.as_ref().unwrap();
                Some(evaluate(
                    declared_type,
                    symboltable,
                    None,
                    &mut checker_ctx.tracker(),
                    0,
                ))
            } else {
                None
            }
        };
        // If both variable and label are present, bidirectional inferencing is possible.
        if declared_type.is_some() && variable.value.is_some() {
            infer_ahead(
                &mut variable.value.as_mut().unwrap(),
                &declared_type.as_ref().unwrap(),
                symboltable,
            );
        }
        let mut inferred_result = variable
            .value
            .as_mut()
            .map(|expr| expressions::typecheck_expression(expr, checker_ctx, symboltable));
        // if value and label are available, unification can be done early.
        // so that the focus later will be the extraction of array and model types.
        if declared_type.is_some() && inferred_result.is_some() {
            let declared = declared_type.as_ref().unwrap();
            // Traits are not allowed in type contexts.
            if let EvaluatedType::TraitInstance { trait_, .. } = &declared {
                let symbol = symboltable.get(*trait_);
                checker_ctx.add_error(errors::trait_as_type(
                    symbol
                        .map(|symbol| symbol.name.clone())
                        .unwrap_or(String::from("{Trait}")),
                    variable.span,
                ));
                return;
            }
            // Never types are not allowed in type contexts.
            if declared.contains(&EvaluatedType::Never) {
                checker_ctx.add_error(errors::never_as_declared(variable.span));
            }
            let type_of_value = inferred_result.as_ref().unwrap();
            match unify_freely(declared, &type_of_value, symboltable, None) {
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
            if let Some(default) = symboltable.default {
                let default_is_implemented =
                    get_implementation_of(default, declared, symboltable).is_some();
                if !default_is_implemented {
                    checker_ctx.add_error(errors::no_default(
                        symboltable.format_evaluated_type(declared),
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
            let symbol = symboltable.get_mut(*name).unwrap();
            // Only pure, immutable and literal types should be allowed as global variables.
            if variable.value.is_some() && symbol.origin_scope_id.is_some_and(|id| id.0 == 0) {
                let expression = variable.value.as_ref().unwrap();
                if !is_pure(expression) {
                    let span = checker_ctx.span_of_expr(expression, symboltable);
                    checker_ctx.add_error(errors::non_pure_global(span));
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
                        } => {
                            let property_span = symboltable
                                .get(property_symbol_idx)
                                .map(|sym| sym.ident_span())
                                .unwrap_or_default();
                            let property_type = expressions::search_for_property(
                                checker_ctx,
                                symboltable,
                                *model,
                                property_symbol_idx,
                                generic_arguments.clone(),
                                true,
                                property_span,
                            );
                            let get_model_name = || symboltable.get(*model).unwrap().name.clone();
                            let get_property_name =
                                || symboltable.get(property_symbol_idx).unwrap().name.clone();
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
                                symboltable.format_evaluated_type(&final_type),
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
                    } if is_array(&final_type, symboltable) => {
                        pattern_result = generic_arguments.first().unwrap().1.clone()
                    }
                    _ => {
                        checker_ctx.add_error(errors::illegal_array_destructure(
                            symboltable.format_evaluated_type(&final_type),
                            variable.span,
                        ));
                        // No point in checking other patterns.
                        return;
                    }
                },
                _ => {}
            }
            if let SemanticSymbolKind::Variable { inferred_type, .. } =
                &mut symboltable.get_mut(*name).unwrap().kind
            {
                *inferred_type = pattern_result;
            }
        }
    }

    fn is_pure(expression: &TypedExpression) -> bool {
        match expression {
            TypedExpression::Literal(_) | TypedExpression::FnExpr(_) => true,
            TypedExpression::ArrayExpr(array) => {
                array.elements.iter().all(|element| is_pure(element))
            }
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
        symboltable: &mut SymbolTable,
    ) {
        let condition_type =
            expressions::typecheck_expression(&mut whil.condition, checker_ctx, symboltable);
        if !is_boolean(&condition_type, symboltable) && !condition_type.is_unknown() {
            checker_ctx.add_error(errors::non_boolean_logic(
                symboltable.format_evaluated_type(&condition_type),
                checker_ctx.span_of_expr(&whil.condition, symboltable),
            ))
        }
        push_scopetype(checker_ctx, ScopeType::Other);
        typecheck_block(&mut whil.body, false, checker_ctx, symboltable);
        pop_scopetype(checker_ctx);
    }

    /// Typechecks a shorthand variable declaration.
    pub fn typecheck_shorthand_variable_declaration(
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
                    0,
                )
            })
        } else {
            None
        };
        if declared_type.is_some() {
            // Try to guess the type of the value.
            let declared_type = declared_type.as_ref().unwrap();
            infer_ahead(&mut shorthand_variable.value, &declared_type, symboltable);
        }
        let type_of_value = expressions::typecheck_expression(
            &mut shorthand_variable.value,
            checker_ctx,
            symboltable,
        );

        // if no declared type, just assign to variable.
        // else attempt unification.
        // If unification fails, then carry on with the declared type value.
        let inference_result = if let Some(declared) = declared_type {
            if let EvaluatedType::TraitInstance { trait_, .. } = &declared {
                let symbol = symboltable.get(*trait_);
                checker_ctx.add_error(errors::trait_as_type(
                    symbol
                        .map(|symbol| symbol.name.clone())
                        .unwrap_or(String::from("{Trait}")),
                    shorthand_variable.span,
                ));
                return;
            }
            if declared.contains(&EvaluatedType::Never) {
                checker_ctx.add_error(errors::never_as_declared(shorthand_variable.span))
            }
            match unify_freely(&declared, &type_of_value, symboltable, None) {
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
        if let SemanticSymbolKind::Variable { inferred_type, .. } =
            &mut symboltable.get_mut(name).unwrap().kind
        {
            *inferred_type = inference_result;
        }
    }

    /// Typechecks a return statement.
    pub fn typecheck_return_statement(
        retstat: &mut TypedReturnStatement,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) {
        let function_context = checker_ctx.current_function_context.last().cloned();
        let maybe_eval_expr = retstat.value.as_mut().map(|expr| {
            if let Some(ctx) = &function_context {
                // try to guess the type for the returned expression.
                infer_ahead(expr, &ctx.return_type, symboltable);
            }
            // Coerce unresolved nested generic types to never, since they are no longer resolvable.
            let mut return_type = expressions::typecheck_expression(expr, checker_ctx, symboltable);
            if !return_type.is_generic()
                && function_context.as_ref().is_some_and(|ctx| ctx.is_named)
            {
                return_type = coerce_all_generics(&return_type, EvaluatedType::Never)
            }
            return_type
        });
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
                });
                return;
            }
            if function_context.is_some_and(|ctx| ctx.return_type.is_unknown()) {
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
                symboltable,
                UnifyOptions::Return,
                None,
            ) {
                // Unification was successful and return type can be updated.
                Ok(typ) => {
                    let prior_evaluated_type =
                        checker_ctx.current_function_context.last_mut().unwrap();
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

    /// Typechecks a function.
    pub fn typecheck_function(
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
                    .map(|param| (*param, EvaluatedType::HardGeneric { base: *param }))
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
                        .map(|typ| evaluate(typ, &symboltable, None, &mut checker_ctx.tracker(), 0))
                        .unwrap_or_else(|| EvaluatedType::Void),
                    return_type.map(|typ| typ.span()),
                )
            } else {
                (vec![], EvaluatedType::Void, None)
            };
        // Traits cannot be used as return types.
        if let EvaluatedType::TraitInstance { trait_, .. } = &return_type {
            let symbol = symboltable.get(*trait_);
            checker_ctx.add_error(errors::trait_as_type(
                symbol
                    .map(|symbol| symbol.name.clone())
                    .unwrap_or(String::from("{Trait}")),
                return_type_span.unwrap_or_default(),
            ));
        }
        for (param_idx, new_type) in evaluated_param_types {
            let mut trait_in_label = None;
            if let SemanticSymbolKind::Parameter {
                inferred_type,
                param_type,
                ..
            } = &mut symboltable.get_mut(param_idx).unwrap().kind
            {
                // Traits cannot be used as parameter types.
                if let EvaluatedType::TraitInstance { trait_, .. } = &new_type {
                    trait_in_label = Some((
                        *trait_,
                        param_type.as_ref().map(|p| p.span()).unwrap_or_default(),
                    ));
                }
                *inferred_type = new_type;
            }
            if let Some((trait_, span)) = trait_in_label {
                let symbol = symboltable.get(trait_);
                checker_ctx.add_error(errors::trait_as_type(
                    symbol
                        .map(|symbol| symbol.name.clone())
                        .unwrap_or(String::from("{Trait}")),
                    span,
                ));
            }
        }
        checker_ctx
            .current_function_context
            .push(CurrentFunctionContext {
                is_named: true,
                return_type: return_type.clone(),
            });
        push_scopetype(checker_ctx, ScopeType::Other);
        let mut block_return_type =
            expressions::typecheck_block(&mut function.body, true, checker_ctx, symboltable);
        pop_scopetype(checker_ctx);
        // Ignore unreachable nested generics.
        if !block_return_type.is_generic() {
            block_return_type = coerce_all_generics(&block_return_type, EvaluatedType::Never);
        }
        checker_ctx.current_function_context.pop();
        // if last statement was a return, there is no need to check type again, since it will still show the apprioprate errors.
        if function
            .body
            .statements
            .last()
            .is_some_and(|statement| matches!(statement, TypedStmnt::ReturnStatement(_)))
        {
            return;
        }
        if let Err(typeerrortype) = unify_types(
            &return_type,
            &block_return_type,
            &symboltable,
            UnifyOptions::Return,
            None,
        ) {
            let span = function
                .body
                .statements
                .last()
                .map(|s| checker_ctx.span_of_stmnt(s, symboltable))
                .or(return_type_span)
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
    }
}

mod expressions {
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
            TypedExpression::ThisExpr(this) => typecheck_this_expression(this, symboltable),
            TypedExpression::CallExpr(c) => {
                typecheck_call_expression(&mut *c, symboltable, checker_ctx)
            }
            TypedExpression::FnExpr(f) => {
                typecheck_function_expression(&mut *f, symboltable, checker_ctx)
            }
            TypedExpression::Block(body) => typecheck_block(body, false, checker_ctx, symboltable),
            TypedExpression::IfExpr(ifexp) => {
                typecheck_if_expression(ifexp, checker_ctx, symboltable)
            }
            TypedExpression::AccessExpr(access) => {
                typecheck_access_expression(&mut *access, symboltable, checker_ctx)
            }
            TypedExpression::ArrayExpr(array) => {
                typecheck_array_expression(array, symboltable, checker_ctx)
            }
            TypedExpression::IndexExpr(indexexp) => {
                typecheck_index_expression(indexexp, symboltable, checker_ctx)
            }
            // TypedExpression::BinaryExpr(binexp) => typecheck_binary_expression(),
            TypedExpression::AssignmentExpr(assexp) => {
                typecheck_assignment_expression(assexp, checker_ctx, symboltable)
            }
            TypedExpression::UnaryExpr(unaryexp) => {
                typecheck_unary_expression(unaryexp, symboltable, checker_ctx)
            }
            TypedExpression::LogicExpr(logexp) => {
                typecheck_logic_expression(logexp, checker_ctx, symboltable)
            }
            TypedExpression::UpdateExpr(updateexp) => {
                typecheck_update_expression(updateexp, checker_ctx, symboltable)
            }
            _ => EvaluatedType::Unknown,
        }
    }

    /// Typechecks an update expression.
    fn typecheck_update_expression(
        updateexp: &mut crate::TypedUpdateExpr,
        checker_ctx: &mut TypecheckerContext<'_>,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        updateexp.inferred_type = (|| {
            let operand_type =
                typecheck_expression(&mut updateexp.operand, checker_ctx, symboltable);
            match updateexp.operator {
                // the ! operator.
                // Can only be used by models that implement Guaranteed.
                // It evaluates to the single generic type of the trait.
                ast::UpdateOperator::Assert => {
                    if let Some(guaranteed) = symboltable.guaranteed {
                        let guaranteed_generic = match &symboltable.get(guaranteed).unwrap().kind {
                            SemanticSymbolKind::Trait { generic_params, .. } => generic_params[0],
                            _ => return EvaluatedType::Unknown,
                        };
                        if let Some(implementation) =
                            get_implementation_of(guaranteed, &operand_type, &symboltable)
                        {
                            match implementation {
                                EvaluatedType::TraitInstance {
                                    generic_arguments, ..
                                } => {
                                    let generic_solution = generic_arguments
                                        .into_iter()
                                        .find(|generic| generic.0 == guaranteed_generic);
                                    if generic_solution.is_none() {
                                        let name = symboltable.format_evaluated_type(&operand_type);
                                        checker_ctx.add_error(errors::illegal_guarantee(
                                            name,
                                            updateexp.span,
                                        ));
                                        return EvaluatedType::Unknown;
                                    }
                                    let evaluated_type = generic_solution.unwrap().1;
                                    let full_generic_list = match operand_type {
                                        EvaluatedType::TraitInstance {
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
                            let name = symboltable.format_evaluated_type(&operand_type);
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
                // The Try trait has two generics, one for the value to be retreived,
                // and another for the value to be immediately returned.
                ast::UpdateOperator::TryFrom => {
                    if let Some(try_idx) = symboltable.try_s {
                        let (first_generic, second_generic) =
                            match &symboltable.get(try_idx).unwrap().kind {
                                SemanticSymbolKind::Trait { generic_params, .. } => {
                                    (generic_params[0], generic_params[1])
                                }
                                _ => return EvaluatedType::Unknown,
                            };
                        let implementation =
                            get_implementation_of(try_idx, &operand_type, &symboltable);
                        if implementation.is_none() {
                            let name = symboltable.format_evaluated_type(&operand_type);
                            checker_ctx.add_error(errors::illegal_try(name, updateexp.span));
                            return EvaluatedType::Unknown;
                        }
                        let implementation = implementation.unwrap();
                        if let EvaluatedType::TraitInstance {
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
                            if first_generic_solution.is_none() || second_generic_solution.is_none()
                            {
                                let name = symboltable.format_evaluated_type(&operand_type);
                                checker_ctx.add_error(errors::illegal_try(name, updateexp.span));
                                return EvaluatedType::Unknown;
                            }
                            let evaluated_type = first_generic_solution.unwrap().1;
                            let mut returned_type = second_generic_solution.unwrap().1;
                            let empty = vec![];
                            let full_generic_list = match &operand_type {
                                EvaluatedType::TraitInstance {
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
                                && function_ctx.is_some_and(|ctx| ctx.is_named)
                            {
                                returned_type =
                                    coerce_all_generics(&returned_type, EvaluatedType::Never)
                            }
                            if function_ctx.is_none() {
                                checker_ctx.add_error(TypeError {
                                    _type: TypeErrorType::MismatchedReturnType {
                                        expected: symboltable
                                            .format_evaluated_type(&EvaluatedType::Void),
                                        found: symboltable.format_evaluated_type(&returned_type),
                                    },
                                    span: updateexp.span,
                                });
                                return EvaluatedType::Unknown;
                            }
                            let function_ctx = function_ctx.unwrap();
                            match unify_types(
                                &function_ctx.return_type,
                                &returned_type,
                                &symboltable,
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
                                            expected: symboltable
                                                .format_evaluated_type(&function_ctx.return_type),
                                            found: symboltable
                                                .format_evaluated_type(&returned_type),
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
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext<'_>,
    ) -> EvaluatedType {
        unaryexp.inferred_type = (|| {
            let operand_type =
                typecheck_expression(&mut unaryexp.operand, checker_ctx, symboltable);
            match unaryexp.operator {
                UnaryOperator::Negation | UnaryOperator::NegationLiteral => {
                    if !is_boolean(&operand_type, symboltable) {
                        let name = symboltable.format_evaluated_type(&operand_type);
                        checker_ctx.add_error(TypeError {
                            _type: TypeErrorType::NonBooleanLogic { name },
                            span: unaryexp.span,
                        })
                    }
                    symboltable
                        .bool
                        .map(|boolean| EvaluatedType::ModelInstance {
                            model: boolean,
                            generic_arguments: vec![],
                        })
                        .unwrap_or(EvaluatedType::Unknown)
                }
                UnaryOperator::Ref => EvaluatedType::Borrowed {
                    base: Box::new(operand_type),
                },
                UnaryOperator::Deref => match operand_type {
                    EvaluatedType::Borrowed { base } => *base,
                    _ => {
                        let name = symboltable.format_evaluated_type(&operand_type);
                        checker_ctx.add_error(TypeError {
                            _type: TypeErrorType::InvalidDereference { name },
                            span: unaryexp.span,
                        });
                        EvaluatedType::Unknown
                    }
                },
                // UnaryOperator::Plus => todo!(),
                // UnaryOperator::Minus => todo!(),
                _ => EvaluatedType::Unknown,
            }
        })();
        unaryexp.inferred_type.clone()
    }

    /// Typechecks if expression.
    fn typecheck_if_expression(
        ifexp: &mut TypedIfExpr,
        checker_ctx: &mut TypecheckerContext<'_>,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        ifexp.inferred_type = {
            let condition_type =
                typecheck_expression(&mut ifexp.condition, checker_ctx, symboltable);
            if !is_boolean(&condition_type, symboltable) && !condition_type.is_unknown() {
                checker_ctx.add_error(errors::non_boolean_logic(
                    symboltable.format_evaluated_type(&condition_type),
                    checker_ctx.span_of_expr(&ifexp.condition, symboltable),
                ));
            }
            // If in a model's constructor, update the scope entered.
            push_scopetype(
                checker_ctx,
                ScopeType::IfBlock {
                    id: ifexp.consequent.scopeid,
                },
            );
            let block_type =
                typecheck_block(&mut ifexp.consequent, false, checker_ctx, symboltable);
            pop_scopetype(checker_ctx);
            if let Some(else_) = &mut ifexp.alternate {
                push_scopetype(
                    checker_ctx,
                    ScopeType::ElseBlock {
                        id_of_parent_if: ifexp.consequent.scopeid,
                    },
                );
                let else_type =
                    typecheck_expression(&mut else_.expression, checker_ctx, symboltable);
                pop_scopetype(checker_ctx);
                match unify_types(
                    &block_type,
                    &else_type,
                    symboltable,
                    UnifyOptions::AnyNever,
                    None,
                ) {
                    Ok(result) => result,
                    Err(errors) => {
                        checker_ctx.add_error(errors::separate_if_types(
                            ifexp.span,
                            symboltable.format_evaluated_type(&block_type),
                            symboltable.format_evaluated_type(&else_type),
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

    /// Typechecks an assignment expression.
    /// todo: handle constant values.
    fn typecheck_assignment_expression(
        assexp: &mut TypedAssignmentExpr,
        checker_ctx: &mut TypecheckerContext<'_>,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        assexp.inferred_type = (|| {
            let left_type = typecheck_expression(&mut assexp.left, checker_ctx, symboltable);
            let right_type = if !is_valid_lhs(&assexp.left) {
                checker_ctx.add_error(errors::invalid_assignment_target(assexp.span));
                typecheck_expression(&mut assexp.right, checker_ctx, symboltable); // For continuity.
                return EvaluatedType::Unknown;
            } else {
                // Try to guess the type of the right.
                infer_ahead(&mut assexp.right, &left_type, symboltable);
                typecheck_expression(&mut assexp.right, checker_ctx, symboltable)
            };
            if matches!(&assexp.left, TypedExpression::UnaryExpr(unexp) if matches!(unexp.operator, UnaryOperator::Ref))
            {
                checker_ctx.add_error(errors::assigning_to_reference(assexp.span));
                return EvaluatedType::Unknown;
            }
            let mut generic_hashmap = HashMap::new();
            let result_type = match &left_type {
                EvaluatedType::ModelInstance { .. }
                | EvaluatedType::EnumInstance { .. }
                | EvaluatedType::Unknown
                | EvaluatedType::HardGeneric { .. }
                | EvaluatedType::Generic { .. }
                | EvaluatedType::OpaqueTypeInstance { .. }
                | EvaluatedType::FunctionExpressionInstance { .. }
                | EvaluatedType::Borrowed { .. } => {
                    if matches!(left_type, EvaluatedType::EnumInstance { .. })
                        && !assexp.left.is_identifier()
                    {
                        checker_ctx.add_error(errors::invalid_assignment_target(assexp.span));
                        return EvaluatedType::Unknown;
                    }
                    match unify_types(
                        &left_type,
                        &right_type,
                        symboltable,
                        UnifyOptions::Conform,
                        Some(&mut generic_hashmap),
                    ) {
                        Ok(result_type) => result_type,
                        Err(errortypes) => {
                            for _type in errortypes {
                                checker_ctx.add_error(TypeError {
                                    _type,
                                    span: assexp.span,
                                })
                            }
                            return EvaluatedType::Unknown;
                        }
                    }
                }
                EvaluatedType::MethodInstance { method, .. } => {
                    let method_symbol = symboltable.get_forwarded(*method).unwrap();
                    let name = method_symbol.name.clone();
                    let owner = match &method_symbol.kind {
                        SemanticSymbolKind::Method {
                            owner_model_or_trait,
                            ..
                        } => symboltable.get(*owner_model_or_trait).unwrap().name.clone(),
                        _ => String::from("[Model]"),
                    };
                    checker_ctx.add_error(errors::mutating_method(owner, name, assexp.span));
                    return EvaluatedType::Unknown;
                }
                _ => {
                    checker_ctx.add_error(errors::invalid_assignment_target(assexp.span));
                    return EvaluatedType::Unknown;
                }
            };
            if let EvaluatedType::MethodInstance { method, .. } = right_type {
                let method_symbol = symboltable.get_forwarded(method).unwrap();
                let name = method_symbol.name.clone();
                let owner = match &method_symbol.kind {
                    SemanticSymbolKind::Method {
                        owner_model_or_trait,
                        ..
                    } => symboltable.get(*owner_model_or_trait).unwrap().name.clone(),
                    _ => String::from("[Model]"),
                };
                checker_ctx.add_error(errors::mutating_method(owner, name, assexp.span));
                return EvaluatedType::Unknown;
            }

            // If lhs is an attribute and we are in a constructor, rigorously check for attribute assignment..
            'check_for_attribute_assignment: {
                let expression_as_attrib = expression_is_attribute(&assexp.left, symboltable);
                if !matches!(assexp.operator, ast::AssignOperator::Assign) {
                    break 'check_for_attribute_assignment;
                }
                if expression_as_attrib.is_none() {
                    break 'check_for_attribute_assignment;
                }
                let attribute_idx = expression_as_attrib.unwrap();
                let contructor_ctx_opt = checker_ctx.current_constructor_context.last_mut();
                if contructor_ctx_opt.is_none() {
                    break 'check_for_attribute_assignment;
                }
                let constructor_ctx = contructor_ctx_opt.unwrap();
                let scopes = &constructor_ctx.scopes;
                let current_scope_type = scopes.last().clone();
                let targeted_attribute = constructor_ctx
                    .attributes
                    .iter_mut()
                    .find(|(idx, _)| **idx == attribute_idx);
                if targeted_attribute.is_none() {
                    break 'check_for_attribute_assignment;
                }
                let (_, prior_assignments) = targeted_attribute.unwrap();
                // If assignment has already been settled.
                if prior_assignments
                    .iter()
                    .any(|assignment| assignment.is_definite())
                {
                    break 'check_for_attribute_assignment;
                }
                // First check that the attribute does not reference itself in its initialization.
                let span_of_rhs =
                    span_of_typed_expression(&assexp.right, symboltable, checker_ctx.literals);
                let attribute_symbol = symboltable.get(attribute_idx).unwrap();
                if attribute_symbol
                    .references
                    .first()
                    .unwrap()
                    .starts
                    .iter()
                    .any(|start| span_of_rhs.contains(*start))
                {
                    checker_ctx.add_error(errors::using_attribute_before_assign(span_of_rhs));
                    break 'check_for_attribute_assignment;
                }
                let assignment_type = match current_scope_type {
                    // If we are in an if block.
                    // For the assignment to be definite, there should also be an assignment in the else block.
                    Some(ScopeType::IfBlock { id }) => AttributeAssignment::InIfBlock { id: *id },
                    // If we are in an else block.
                    // If assignment is definitive in the if block, and there is also an assignment here,
                    // then we can resolve both to produce a single assignment.
                    // But the assignment is not definitive overall, unless the branching expression is within the constructor.
                    Some(ScopeType::ElseBlock { id_of_parent_if }) => {
                        if prior_assignments.iter().any(|assignment| matches!(assignment, AttributeAssignment::InIfBlock { id } if id == id_of_parent_if)) {
                            // Branching assignment, but in the constructor scope.
                            if constructor_ctx.scopes.len() == 1 {
                                AttributeAssignment::Definite { span: span_of_typed_expression(&assexp.left, symboltable, checker_ctx.literals) }
                            } else {
                                // Branching assignment, but in another scope.
                                let previous_scope =scopes.get(scopes.len() -2 ).unwrap();
                                if let ScopeType::IfBlock { id } = previous_scope {
                                    AttributeAssignment::InIfBlock { id: *id }
                                } else {
                                    AttributeAssignment::SomewhereElse
                                }
                            }
                        } else {
                            AttributeAssignment::SomewhereElse
                        }
                    }
                    Some(_) => AttributeAssignment::SomewhereElse,
                    // Still within the constructor scope.
                    None => AttributeAssignment::Definite {
                        span: span_of_typed_expression(
                            &assexp.left,
                            symboltable,
                            checker_ctx.literals,
                        ),
                    },
                };
                prior_assignments.push(assignment_type);
            }
            let span = assexp.span;
            ensure_assignment_validity(&right_type, checker_ctx, span);

            // Update value of left hand side based on the inferred type.
            let generics = generic_hashmap.into_iter().collect::<Vec<_>>();
            update_expression_type(&mut assexp.left, symboltable, &generics);
            return EvaluatedType::Void;
        })();
        assexp.inferred_type.clone()
    }

    /// If the expression passed in refers to an attribute, it returns the symbol index of the attribute.
    /// Otherwise it returns none.
    fn expression_is_attribute(
        expr: &TypedExpression,
        symboltable: &mut SymbolTable,
    ) -> Option<SymbolIndex> {
        match expr {
            TypedExpression::AccessExpr(accessexp) => match &accessexp.object {
                TypedExpression::ThisExpr(this) => {
                    let property_name = match &accessexp.property {
                        TypedExpression::Identifier(ident) => {
                            symboltable.get(ident.value)?.name.as_str()
                        }
                        _ => return None,
                    };
                    let model_or_trait = symboltable.get(this.model_or_trait?)?;
                    let attributes = match &model_or_trait.kind {
                        SemanticSymbolKind::Model { attributes, .. } => attributes,
                        _ => return None,
                    };
                    for attribute_idx in attributes {
                        let attribute_idx = *attribute_idx;
                        let attribute = symboltable.get(attribute_idx)?;
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
            TypedExpression::Identifier(_) => true,
            TypedExpression::AccessExpr(accessexp) => {
                is_valid_lhs(&accessexp.object)
                    || matches!(accessexp.object, TypedExpression::ThisExpr(_))
            }
            TypedExpression::IndexExpr(indexp) => is_valid_lhs(&indexp.object),
            TypedExpression::UnaryExpr(expr) => matches!(&expr.operator, UnaryOperator::Deref),
            _ => false,
        }
    }

    fn typecheck_logic_expression(
        logexp: &mut TypedLogicExpr,
        checker_ctx: &mut TypecheckerContext<'_>,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        logexp.inferred_type = (|| {
            let left = typecheck_expression(&mut logexp.left, checker_ctx, symboltable);
            let right = typecheck_expression(&mut logexp.right, checker_ctx, symboltable);

            if !is_boolean(&left, symboltable) && !left.is_unknown() {
                checker_ctx.add_error(errors::non_boolean_logic(
                    symboltable.format_evaluated_type(&left),
                    checker_ctx.span_of_expr(&logexp.right, symboltable),
                ));
            }
            if !is_boolean(&right, symboltable) && !right.is_unknown() {
                checker_ctx.add_error(errors::non_boolean_logic(
                    symboltable.format_evaluated_type(&right),
                    checker_ctx.span_of_expr(&logexp.right, symboltable),
                ));
            }
            if let Some(boolean_idx) = symboltable.bool {
                return EvaluatedType::ModelInstance {
                    model: boolean_idx,
                    generic_arguments: vec![],
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
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        this.inferred_type = (|| {
            if this.model_or_trait.is_none() {
                // Error is already handled.
                return EvaluatedType::Unknown;
            }
            let model_or_trait = this.model_or_trait.unwrap();
            let symbol = symboltable.get_forwarded(model_or_trait).unwrap();
            match &symbol.kind {
                SemanticSymbolKind::Model { generic_params, .. } => EvaluatedType::ModelInstance {
                    model: model_or_trait,
                    generic_arguments: evaluate_generic_params(generic_params, false),
                },
                SemanticSymbolKind::Trait { generic_params, .. } => EvaluatedType::TraitInstance {
                    trait_: model_or_trait,
                    generic_arguments: evaluate_generic_params(generic_params, false),
                },
                _ => unreachable!("{symbol:#?} is not a model or trait."),
            }
        })();
        this.inferred_type.clone()
    }

    /// Typechecks a new expression.
    fn typecheck_new_expression(
        newexp: &mut TypedNewExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        newexp.inferred_type = (|| {
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
                                        checker_ctx
                                            .add_error(errors::model_not_constructable(name, span));
                                        return EvaluatedType::Unknown;
                                    }
                                    let generic_arguments =
                                        evaluate_generic_params(generic_params, false);
                                    let parameter_types = convert_param_list_to_type(
                                        constructor_parameters.as_ref().unwrap_or(&vec![]),
                                        symboltable,
                                        &generic_arguments,
                                        checker_ctx,
                                    );
                                    (generic_arguments, generic_params.clone(), parameter_types)
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
                                // ignore irrelevant generic transforms.
                                generic_arguments: generic_arguments
                                    .into_iter()
                                    .filter(|argument| {
                                        generic_params.iter().any(|base| *base == argument.0)
                                    })
                                    .collect(),
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
        })();
        newexp.inferred_type.clone()
    }

    /// Typechecks an index expression.
    fn typecheck_index_expression(
        indexexp: &mut TypedIndexExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        indexexp.inferred_type = (|| {
            let type_of_indexed =
                typecheck_expression(&mut indexexp.object, checker_ctx, symboltable);
            let _type_of_indexer =
                typecheck_expression(&mut indexexp.index, checker_ctx, symboltable);
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
        })();
        indexexp.inferred_type.clone()
    }

    /// Typechecks an array expression.
    fn typecheck_array_expression(
        array: &mut crate::TypedArrayExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        array.inferred_type = (|| {
            if symboltable.array.is_none() {
                checker_ctx.add_error(missing_intrinsic(format!("Array"), array.span));
            }
            if array.elements.len() == 0 {
                if symboltable.array.is_some() {
                    let empty = vec![];
                    let placeholder = arrify(EvaluatedType::Unknown, symboltable);
                    let generic = get_type_generics(&placeholder, &empty)
                        .first()
                        .map(|(idx, _)| idx);
                    if generic.is_some() {
                        return arrify(
                            EvaluatedType::Generic {
                                base: *generic.unwrap(),
                            },
                            symboltable,
                        );
                    }
                }
                return arrify(EvaluatedType::Unknown, &symboltable);
            }
            let mut element_types = vec![];

            let mut next_type = array
                .elements
                .first_mut()
                .map(|expression| typecheck_expression(expression, checker_ctx, symboltable))
                .unwrap_or(EvaluatedType::Unknown);
            // The first type is the base type and is assumed to be the true type for every other type in the array.
            for element in &mut array.elements.iter_mut().skip(1) {
                // Try to guess the type of the next element based on the first type.
                // todo: should the next type be progressively unified before inferring?
                infer_ahead(element, &next_type, symboltable);
                element_types.push(typecheck_expression(element, checker_ctx, symboltable));
            }
            // Reduce individual types to determine final array form.
            let mut i = 1;
            let mut errors_gotten = vec![];
            for evaluated_type in element_types {
                let mut unification = unify_types(
                    &next_type,
                    &evaluated_type,
                    symboltable,
                    UnifyOptions::None,
                    None,
                );
                // For numeric types, casting should occur bidirectionally.
                // So that elements will always scale the array upwards in size.
                if is_numeric_type(&next_type, symboltable)
                    && is_numeric_type(&evaluated_type, symboltable)
                {
                    unification = unification.or(unify_types(
                        &evaluated_type,
                        &next_type,
                        symboltable,
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
                                .map(|el| checker_ctx.span_of_expr(el, symboltable))
                                .unwrap_or(array.span),
                        });
                    }
                }
            }
            arrify(next_type, symboltable)
        })();
        array.inferred_type.clone()
    }

    /// Typechecks a literal value.
    fn typecheck_literal(
        l: &mut crate::LiteralIndex,
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        match &checker_ctx.literals.get(*l).unwrap() {
            Literal::StringLiteral { value, .. } => {
                typecheck_string_literal(value, checker_ctx, symboltable)
            }
            Literal::NumericLiteral { value, .. } => {
                typecheck_numeric_literal(value, checker_ctx, symboltable)
            } // todo.
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

    /// Typechecks a numeric literal.
    fn typecheck_numeric_literal(
        value: &ast::WhirlNumber,
        checker_ctx: &mut TypecheckerContext<'_>,
        symboltable: &mut SymbolTable,
    ) -> EvaluatedType {
        // todo: errors for missing intrinsics.
        // TODO. 😑
        get_numeric_type(symboltable, value, Some(checker_ctx))
    }

    /// Typechecks a bool literal by matching it with the bool intrinsic symbol.
    fn typecheck_boolean_literal(
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
        start_line: &u32,
        start_character: &u32,
        value: &bool,
    ) -> EvaluatedType {
        if let Some(bool_index) = symboltable.bool {
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
        if let Some(string_index) = symboltable.string {
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
        if caller.is_unknown() {
            callexp.arguments.iter_mut().for_each(|arg| {
                typecheck_expression(arg, checker_ctx, symboltable);
            }); // for continuity.
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
                            .map(|typ| {
                                evaluate(typ, symboltable, None, &mut checker_ctx.tracker(), 0)
                            })
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
            } => (is_async, params, generic_args, *return_type),
            _ => {
                return EvaluatedType::Unknown;
            }
        };
        for (index, argument) in callexp.arguments.iter_mut().enumerate() {
            // Try to preemptively guess the type of each argument.
            if let Some(parameter_type) = parameter_types.get(index) {
                infer_ahead(argument, &parameter_type.inferred_type, symboltable);
            }
        }
        let evaluated_args = callexp
            .arguments
            .iter_mut()
            .map(|arg| typecheck_expression(arg, checker_ctx, symboltable))
            .collect::<Vec<_>>();
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
        callexp.inferred_type = coerce(return_type, &generic_arguments);
        update_expression_type(&mut callexp.caller, symboltable, &generic_arguments);
        callexp.inferred_type.clone()
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
                    UnifyOptions::HardConform,
                    Some(&mut generic_map),
                )
            } else {
                unify_types(
                    parameter_type,
                    argument_type,
                    symboltable,
                    UnifyOptions::HardConform,
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
            | EvaluatedType::HardGeneric { .. }
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
        f.inferred_type = (|| {
            let mut parameter_types = vec![];
            let generic_args = evaluate_generic_params(&f.generic_params, true);
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
                let inferred_type = param_type
                    .as_ref()
                    .map(|typ| evaluate(typ, symboltable, None, &mut checker_ctx.tracker(), 0))
                    .unwrap_or(EvaluatedType::Unknown);
                // Traits cannot be used as parameter types.
                if let EvaluatedType::TraitInstance { trait_, .. } = &inferred_type {
                    let symbol = symboltable.get(*trait_);
                    checker_ctx.add_error(errors::trait_as_type(
                        symbol
                            .map(|symbol| symbol.name.clone())
                            .unwrap_or(String::from("{Trait}")),
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
                        &symboltable,
                        Some(&generic_args),
                        &mut checker_ctx.tracker(),
                        0,
                    ),
                    typ.span(),
                )
            });
            // Traits cannot be used as return types.
            if let Some((EvaluatedType::TraitInstance { trait_, .. }, _)) = &return_type {
                let symbol = symboltable.get(*trait_);
                checker_ctx.add_error(errors::trait_as_type(
                    symbol
                        .map(|symbol| symbol.name.clone())
                        .unwrap_or(String::from("{Trait}")),
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
                    let blocktype = typecheck_block(block, false, checker_ctx, symboltable);
                    pop_scopetype(checker_ctx);
                    if blocktype.is_void() {
                        checker_ctx
                            .current_function_context
                            .last_mut()
                            .unwrap()
                            .return_type = blocktype;
                    }
                    checker_ctx
                        .current_function_context
                        .pop()
                        .unwrap()
                        .return_type
                }
                expression => typecheck_expression(expression, checker_ctx, symboltable),
            };
            if return_type.is_some() {
                let (return_type, span) = return_type.unwrap();
                match unify_types(
                    &return_type,
                    &inferred_return_type,
                    symboltable,
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
                                expected: symboltable.format_evaluated_type(&return_type),
                                found: symboltable.format_evaluated_type(&inferred_return_type),
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
            }
        })();
        f.inferred_type.clone()
    }

    /// Typechecks an access expression.
    fn typecheck_access_expression(
        access: &mut TypedAccessExpr,
        symboltable: &mut SymbolTable,
        checker_ctx: &mut TypecheckerContext,
    ) -> EvaluatedType {
        access.inferred_type = (|| {
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
        })();
        access.inferred_type.clone()
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
            } => search_for_property(
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
                        evaluate_generic_params(generic_params, false)
                    }
                    _ => vec![],
                };
                search_for_property(
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
            EvaluatedType::Module(module) => {
                let module = symboltable.get_forwarded(module).unwrap();
                let property_symbol = symboltable.get_forwarded(property_symbol_idx).unwrap();
                match &module.kind {
                    SemanticSymbolKind::Module {
                        global_declaration_symbols,
                        ..
                    } => {
                        for idx in global_declaration_symbols {
                            let symbol = match symboltable.get(*idx) {
                                Some(symbol) => symbol,
                                None => continue,
                            };
                            if symbol.name == property_symbol.name {
                                if !symbol.kind.is_public() {
                                    checker_ctx.add_error(TypeError {
                                        _type: TypeErrorType::PrivateSymbolLeak {
                                            modulename: module.name.clone(),
                                            property: property_symbol.name.clone(),
                                        },
                                        span: property_span,
                                    });
                                }
                                let actualidx = symboltable.forward(*idx);
                                let actualsymbol = symboltable.get_forwarded(actualidx).unwrap();
                                let evaluated_type =
                                    symbol_to_type(actualsymbol, actualidx, symboltable)
                                        .ok()
                                        .unwrap_or(EvaluatedType::Unknown);
                                // get mutably and resolve.
                                let property_symbol =
                                    symboltable.get_mut(property_symbol_idx).unwrap();
                                if let SemanticSymbolKind::Property { resolved, .. } =
                                    &mut property_symbol.kind
                                {
                                    *resolved = Some(actualidx)
                                }
                                return evaluated_type;
                            }
                        }
                        // No such symbol found.
                        checker_ctx.add_error(TypeError {
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
                ref available_traits,
                ..
            } => {
                let mut generic_arguments = generic_arguments.clone();
                // Gather methods from all the implementations.
                let implementation_methods = available_traits
                    .iter()
                    .filter_map(|implementation| {
                        match implementation {
                            EvaluatedType::TraitInstance {
                                trait_,
                                generic_arguments: trait_generics,
                            } => {
                                let trait_ = *trait_;
                                // Update the solutions of the traits generics.
                                generic_arguments.append(&mut (trait_generics.clone()));
                                // Here a trait is treated as a generic argument and given a solution.
                                // This allows the `This` marker to refer to the implementing model, rather than the trait.
                                generic_arguments.push((trait_, object_type.clone()));
                                let trait_symbol = symboltable.get_forwarded(trait_)?;
                                match &trait_symbol.kind {
                                    SemanticSymbolKind::Trait { methods, .. } => Some(methods),
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
                let property_symbol = symboltable.get(property_symbol_idx).unwrap();
                for method in complete_method_list {
                    let method = *method;
                    let method_symbol = match symboltable.get(method) {
                        Some(sym) => sym,
                        None => continue,
                    };
                    if method_symbol.name == property_symbol.name && method_symbol.kind.is_public()
                    {
                        // get mutably and resolve.
                        let property_symbol = symboltable.get_mut(property_symbol_idx).unwrap();
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
                        };
                    }
                }
                None
            }
            EvaluatedType::Borrowed { base } => {
                return extract_property_of(
                    *base,
                    symboltable,
                    property_symbol_idx,
                    checker_ctx,
                    access,
                )
            }
            EvaluatedType::Generic { base } | EvaluatedType::HardGeneric { base } => {
                search_for_property(
                    checker_ctx,
                    symboltable,
                    base,
                    property_symbol_idx,
                    vec![],
                    true,
                    property_span,
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

    /// Look through all the possible methods and attributes of a model or generic
    /// to determine the property being referenced.
    pub fn search_for_property(
        checker_ctx: &mut TypecheckerContext,
        symboltable: &mut SymbolTable,
        model: SymbolIndex,
        property_symbol_idx: SymbolIndex,
        mut generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
        object_is_instance: bool,
        property_span: Span,
    ) -> Option<EvaluatedType> {
        // The base type of the model or generic.
        let base_symbol = symboltable.get_forwarded(model).unwrap();
        let property_symbol = symboltable.get_forwarded(property_symbol_idx).unwrap();
        let impls = match &base_symbol.kind {
            SemanticSymbolKind::Model {
                implementations, ..
            }
            | SemanticSymbolKind::GenericParameter {
                traits: implementations,
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
                    evaluate(int_typ, symboltable, Some(&generic_arguments), &mut None, 0);
                match implementation {
                    EvaluatedType::TraitInstance {
                        trait_,
                        generic_arguments: mut trait_generics,
                    } => {
                        // Update the solutions of the traits generics.
                        generic_arguments.append(&mut trait_generics);
                        // Here a trait is treated as a generic argument and given a solution.
                        // This allows the `This` marker to refer to the implementing model, rather than the trait.
                        generic_arguments.push((
                            trait_,
                            EvaluatedType::ModelInstance {
                                model,
                                generic_arguments: generic_arguments.clone(),
                            },
                        ));
                        let trait_symbol = symboltable.get_forwarded(trait_)?;
                        match &trait_symbol.kind {
                            SemanticSymbolKind::Trait { methods, .. } => Some(methods),
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
            let method_symbol = symboltable.get_forwarded(method).unwrap();
            if method_symbol.name == property_symbol.name {
                let method_is_static = match &method_symbol.kind {
                    SemanticSymbolKind::Method { is_static, .. } => *is_static,
                    _ => false,
                };
                if method_is_static && object_is_instance {
                    checker_ctx.add_error(errors::instance_static_method_access(
                        base_symbol.name.clone(),
                        method_symbol.name.clone(),
                        property_span,
                    ))
                } else if !method_is_static && !object_is_instance {
                    checker_ctx.add_error(errors::contructor_non_static_method_access(
                        base_symbol.name.clone(),
                        method_symbol.name.clone(),
                        property_span,
                    ))
                }
                // get mutably and resolve.
                let property_symbol = symboltable.get_mut(property_symbol_idx).unwrap();
                if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind {
                    *resolved = Some(method)
                }
                // Add reference on source.
                let method_symbol = symboltable.get_mut(method).unwrap();
                method_symbol.add_reference(checker_ctx.path_idx, property_span);
                // Block non-public access.
                if !method_symbol.kind.is_public()
                    && checker_ctx.enclosing_model_or_trait != Some(model)
                {
                    checker_ctx.add_error(errors::private_property_leak(
                        method_symbol.name.clone(),
                        property_span,
                    ));
                }
                return Some(EvaluatedType::MethodInstance {
                    method,
                    generic_arguments,
                });
            }
        }
        // Is property an attribute?
        if let Some((_, attributes)) = model_props {
            for attribute in attributes.iter() {
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
                            0,
                        ),
                        _ => return Some(EvaluatedType::Unknown),
                    };
                    // get mutably.
                    let property_symbol = symboltable.get_mut(property_symbol_idx).unwrap();
                    if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind
                    {
                        *resolved = Some(attribute)
                    }
                    // Add reference on source.
                    let attribute_symbol = symboltable.get_mut(attribute).unwrap();
                    attribute_symbol.add_reference(checker_ctx.path_idx, property_span);
                    if !attribute_symbol.kind.is_public()
                        && checker_ctx.enclosing_model_or_trait != Some(model)
                    {
                        checker_ctx.add_error(errors::private_property_leak(
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
                let attribute_symbol = symboltable.get_forwarded(attribute).unwrap();
                if attribute_symbol.name.to_lowercase() == property_symbol.name.to_lowercase() {
                    checker_ctx.add_error(errors::mispelled_name(
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
            let method_symbol = symboltable.get_forwarded(method).unwrap();
            if method_symbol.name.to_lowercase() == property_symbol.name.to_lowercase() {
                checker_ctx.add_error(errors::mispelled_name(
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
        symboltable: &mut SymbolTable,
    ) -> Result<EvaluatedType, TypeErrorType> {
        let name = symboltable.forward(i.value);
        let symbol = symboltable.get_forwarded(name).unwrap();
        let eval_type = match symbol_to_type(symbol, name, symboltable) {
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
        symboltable: &mut SymbolTable,
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
                        let expression_type =
                            typecheck_expression(expression, checker_ctx, symboltable);
                        return expression_type;
                    } else if let TypedStmnt::ReturnStatement(retstat) = statement {
                        statements::typecheck_return_statement(retstat, checker_ctx, symboltable);
                        if !is_function_block {
                            return EvaluatedType::Never;
                        } else {
                            loopindex += 1;
                            continue;
                        }
                    }
                }
                statements::typecheck_statement(statement, checker_ctx, symboltable);
                loopindex += 1;
            }

            EvaluatedType::Void
        })();
        body.inferred_type.clone()
    }
}
