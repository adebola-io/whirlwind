use crate::{
    evaluate, evaluate_parameter_idxs, span_of_typed_expression, span_of_typed_statement,
    unify::{unify_freely, unify_types},
    utils::{
        arrify, check_usage, coerce, coerce_all_generics, ensure_assignment_validity,
        evaluate_generic_params, get_implementation_of, get_numeric_type, get_type_generics,
        infer_ahead, is_array, is_boolean, is_numeric_type, maybify, prospectify, symbol_to_type,
        update_expression_type,
    },
    DiagnosticType, EvaluatedType, Literal, LiteralMap, ParameterType, PathIndex,
    ProgramDiagnostic, ScopeId, SemanticSymbolKind, SymbolIndex, SymbolLibrary, TypedAccessExpr,
    TypedAssignmentExpr, TypedBlock, TypedCallExpr, TypedExpression, TypedFnExpr,
    TypedFunctionDeclaration, TypedIdent, TypedIfExpr, TypedIndexExpr, TypedLogicExpr, TypedModule,
    TypedNewExpr, TypedReturnStatement, TypedStmnt, TypedThisExpr, UnifyOptions,
};
use ast::{Span, UnaryOperator};
use errors::{missing_intrinsic, TypeError, TypeErrorType};
use std::collections::HashMap;

mod expressions;
mod statements;

/// The typechecker is the second pass of the analyzer,
/// used for infering and validating the use of data types within a module.
/// It also does some control flow analysis, validates return types,
/// solves generics and ensures valid property assignment and use.
pub struct TypecheckerContext<'standpoint> {
    /// Current module being typechecked.
    pub path_idx: PathIndex,
    /// The context of the closest function scope currently being typechecked.
    current_function_context: Vec<CurrentFunctionContext>,
    /// The context of the closest constructor currently being typechecked.
    current_constructor_context: Vec<CurrentConstructorContext>,
    /// The model or interface that encloses the current statement.
    enclosing_model_or_interface: Option<SymbolIndex>,
    /// A marker for static model methods, to block the use of 'this'.
    current_function_is_static: Option<bool>,
    /// A marker for blocking the use of the `this` value standalone
    /// in a constructor block.
    current_expression_is_access: Option<bool>,
    /// List of errors from the standpoint.
    pub diagnostics: &'standpoint mut Vec<ProgramDiagnostic>,
    /// List of literal types from the standpoint.
    literals: &'standpoint LiteralMap,
    // /// Cached values of intermediate types,
    // /// so they do not have to be evaluated over and over.
    // intermediate_types: HashMap<IntermediateType, EvaluatedType>,
}

#[derive(Clone)]
struct CurrentFunctionContext {
    /// Whether it is a named function or a function expression.
    is_named: bool,
    return_type: EvaluatedType,
}

struct CurrentConstructorContext {
    model: SymbolIndex,
    scopes: Vec<ScopeType>,
    attributes: HashMap<SymbolIndex, Vec<AttributeAssignment>>,
}

enum ScopeType {
    IfBlock { id: ScopeId },
    ElseBlock { id_of_parent_if: ScopeId },
    Other,
}

enum AttributeAssignment {
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
    /// Adds a type error to the owner standpoint's list of diagnostics.
    pub fn add_diagnostic(&mut self, error: TypeError) {
        self.diagnostics.push(ProgramDiagnostic {
            offending_file: self.path_idx,
            _type: DiagnosticType::Error(crate::Error::Typing(error)),
        })
    }
    /// Returns a reference to the error list to be passed around by the evaluator.
    pub fn tracker(&mut self) -> Option<(&mut Vec<ProgramDiagnostic>, PathIndex)> {
        Some((self.diagnostics, self.path_idx))
    }
    /// Calculates the span of a typed expression using the symbollib and the list of literals.
    fn span_of_expr(&self, expression: &TypedExpression, symbollib: &SymbolLibrary) -> Span {
        span_of_typed_expression(expression, symbollib, self.literals)
    }
    /// Calculates the span of a statement using the symbollib and the list of literals.
    fn span_of_stmnt(&self, s: &TypedStmnt, symbollib: &mut SymbolLibrary) -> Span {
        span_of_typed_statement(s, symbollib, self.literals)
    }
}

/// Typechecks a module.
pub fn typecheck(
    module: &mut TypedModule,
    symbollib: &mut SymbolLibrary,
    diagnostics: &mut Vec<ProgramDiagnostic>,
    literals: &LiteralMap,
) {
    let mut checker_ctx = TypecheckerContext {
        path_idx: module.path_idx,
        current_function_context: Vec::new(),
        current_constructor_context: Vec::new(),
        enclosing_model_or_interface: None,
        current_function_is_static: None,
        current_expression_is_access: None,
        diagnostics,
        literals,
    };
    for statement in &mut module.statements {
        statements::typecheck_statement(statement, &mut checker_ctx, symbollib);
    }

    let is_not_definite = |child: &EvaluatedType| child.is_unknown() || child.is_soft_generic();
    // Final audit of all symbols in the module.
    for symbol in symbollib.in_module(module.path_idx) {
        // Block uninferrable generics and unknowns at the end.
        // todo: parameters
        if let SemanticSymbolKind::Variable { inferred_type, .. }
        | SemanticSymbolKind::LoopVariable { inferred_type, .. } = &symbol.kind
        {
            if inferred_type.contains_child_for_which(&is_not_definite) {
                checker_ctx.add_diagnostic(errors::uninferrable_variable(
                    symbol.name.to_owned(),
                    symbol.ident_span(),
                ));
            }
        }
        check_usage(symbol, &mut checker_ctx);
    }
}

fn pop_scopetype(checker_ctx: &mut TypecheckerContext<'_>) {
    let mut constructor_context = checker_ctx.current_constructor_context.last_mut();
    constructor_context.as_mut().map(|ctx| ctx.scopes.pop());
}

fn push_scopetype(checker_ctx: &mut TypecheckerContext<'_>, scope: ScopeType) {
    let mut constructor_context = checker_ctx.current_constructor_context.last_mut();
    constructor_context
        .as_mut()
        .map(|ctx| ctx.scopes.push(scope));
}
