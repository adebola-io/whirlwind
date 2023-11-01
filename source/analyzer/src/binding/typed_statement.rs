use ast::Span;

use crate::{IntermediateType, LiteralIndex, SymbolIndex, TypedExpression, TypedIdent};

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
    TraitDeclaration(TypedTraitDeclaration),
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

/// A node for a trait declaration in the AST.
#[derive(Debug, PartialEq)]
pub struct TypedTraitDeclaration {
    pub name: SymbolIndex,
    pub body: TypedTraitBody,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedTraitBody {
    pub properties: Vec<TypedTraitProperty>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedTraitProperty {
    pub name: SymbolIndex,
    pub _type: TypedTraitPropertyType,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum TypedTraitPropertyType {
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
    TraitImpl {
        trait_target: Vec<IntermediateType>,
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
