use ast::Span;

use crate::{
    span_of_typed_expression, EvaluatedType, IntermediateType, LiteralIndex, LiteralMap, ScopeId,
    SymbolIndex, SymbolLibrary, TypedExpression, TypedIdent,
};

#[derive(Debug, PartialEq)]
pub enum TypedStmnt {
    RecordDeclaration,
    // Declarations.
    TestDeclaration(TypedTestDeclaration),
    EnumDeclaration(TypedEnumDeclaration),
    UseDeclaration(TypedUseDeclaration),
    VariableDeclaration(TypedVariableDeclaration),
    ShorthandVariableDeclaration(TypedShorthandVariableDeclaration),
    ConstantDeclaration(TypedConstantDeclaration),
    TypeDeclaration(TypedTypeDeclaration),
    ModelDeclaration(TypedModelDeclaration),
    ModuleDeclaration(TypedModuleDeclaration),
    FunctionDeclaration(TypedFunctionDeclaration),
    InterfaceDeclaration(TypedInterfaceDeclaration),
    // Expression statements.
    ExpressionStatement(TypedExpression),
    FreeExpression(TypedExpression),
    // Control statements.
    ReturnStatement(TypedReturnStatement),
    BreakStatement(TypedBreakStatement),
    ForStatement(TypedForStatement),
    WhileStatement(TypedWhileStatement),
    ContinueStatement(TypedContinueStatement),
}
impl TypedStmnt {
    /// Returns `true` if the typed stmnt is [`FreeExpression`].
    ///
    /// [`FreeExpression`]: TypedStmnt::FreeExpression
    #[must_use]
    pub fn is_free_expression(&self) -> bool {
        matches!(self, Self::FreeExpression(..))
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedUseDeclaration {
    pub is_public: bool,
    pub imports: Vec<SymbolIndex>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedVariableDeclaration {
    pub names: Vec<SymbolIndex>,
    pub value: Option<TypedExpression>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedEnumDeclaration {
    pub name: SymbolIndex,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedUseTarget {
    pub name: SymbolIndex,
    pub path: TypedUsePath,
}

#[derive(Debug, PartialEq)]
pub enum TypedUsePath {
    Me,
    Item(Box<TypedUseTarget>),
    List(Vec<TypedUseTarget>),
}

/// Node for a test block.
#[derive(Debug, PartialEq)]
pub struct TypedTestDeclaration {
    pub name: LiteralIndex,
    pub body: TypedBlock,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedTypeDeclaration {
    pub name: SymbolIndex,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedBlock {
    pub statements: Vec<TypedStmnt>,
    pub span: Span,
    pub scopeid: ScopeId,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedShorthandVariableDeclaration {
    pub name: SymbolIndex,
    pub value: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedFunctionDeclaration {
    pub name: SymbolIndex,
    pub body: TypedBlock,
    pub span: Span,
}

/// A node for a interface declaration in the AST.
#[derive(Debug, PartialEq)]
pub struct TypedInterfaceDeclaration {
    pub name: SymbolIndex,
    pub body: TypedInterfaceBody,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedInterfaceBody {
    pub properties: Vec<TypedInterfaceProperty>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedInterfaceProperty {
    pub name: SymbolIndex,
    pub _type: TypedInterfacePropertyType,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum TypedInterfacePropertyType {
    /// A method that is to be implemented.
    Signature,
    /// A method with a default implementation.
    Method { body: TypedBlock },
}

#[derive(Debug, PartialEq)]
pub struct TypedConstantDeclaration {
    pub name: SymbolIndex,
    pub value: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedModelDeclaration {
    pub name: SymbolIndex,
    pub body: TypedModelBody,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedModelBody {
    pub properties: Vec<TypedModelProperty>,
    pub constructor: Option<TypedBlock>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedModelProperty {
    pub name: SymbolIndex,
    pub _type: TypedModelPropertyType,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum TypedModelPropertyType {
    TypedAttribute,
    TypedMethod {
        body: TypedBlock,
    },
    InterfaceImpl {
        interface_target: Vec<IntermediateType>,
        body: TypedBlock,
    },
}

#[derive(Debug, PartialEq)]
pub struct TypedModuleDeclaration {
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedReturnStatement {
    pub value: Option<TypedExpression>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedBreakStatement {
    pub label: Option<TypedIdent>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedWhileStatement {
    pub condition: TypedExpression,
    pub body: TypedBlock,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedForStatement {
    pub items: Vec<SymbolIndex>,
    pub iterator: TypedExpression,
    pub label: Option<TypedIdent>,
    pub body: TypedBlock,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedContinueStatement {
    pub label: Option<TypedIdent>,
    pub span: Span,
}

pub fn span_of_typed_statement(
    s: &TypedStmnt,
    symbollib: &SymbolLibrary,
    literals: &LiteralMap,
) -> Span {
    match s {
        TypedStmnt::TestDeclaration(t) => t.span,
        TypedStmnt::UseDeclaration(u) => u.span,
        TypedStmnt::VariableDeclaration(v) => v.span,
        TypedStmnt::ConstantDeclaration(c) => c.span,
        TypedStmnt::ModelDeclaration(c) => c.span,
        TypedStmnt::FunctionDeclaration(f) => f.span,
        TypedStmnt::RecordDeclaration => todo!(),
        TypedStmnt::InterfaceDeclaration(t) => t.span,
        TypedStmnt::EnumDeclaration(e) => e.span,
        TypedStmnt::TypeDeclaration(t) => t.span,
        TypedStmnt::WhileStatement(w) => w.span,
        TypedStmnt::ForStatement(f) => f.span,
        TypedStmnt::ExpressionStatement(e) | TypedStmnt::FreeExpression(e) => {
            span_of_typed_expression(e, symbollib, literals)
        }
        TypedStmnt::ShorthandVariableDeclaration(v) => v.span,
        TypedStmnt::ModuleDeclaration(m) => m.span,
        TypedStmnt::ReturnStatement(r) => r.span,
        TypedStmnt::ContinueStatement(c) => c.span,
        TypedStmnt::BreakStatement(b) => b.span,
    }
}
