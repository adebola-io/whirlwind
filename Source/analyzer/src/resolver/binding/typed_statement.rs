use ast::Span;

use crate::{EvaluatedType, IntermediateType, LiteralIndex, SymbolIndex, SymbolLocator, TypedExpr};

#[derive(Debug, PartialEq)]
pub enum TypedStmnt {
    // Declarations.
    TestDeclaration(TypedTestDeclaration),
    EnumDeclaration(TypedEnumDeclaration),
    UseDeclaration(TypedUseDeclaration),
    ShorthandVariableDeclaration(TypedShorthandVariableDeclaration),
    ConstantDeclaration(TypedConstantDeclaration),
    TypeDeclaration(TypedTypeDeclaration),
    ModelDeclaration(TypedModelDeclaration),
    ExpressionStatement(TypedExpr),
    FreeExpression(TypedExpr),
    ModuleDeclaration(TypedModuleDeclaration),
    ReturnStatement(TypedReturnStatement),
    WhileStatement(TypedWhileStatement),
}

#[derive(Debug, PartialEq)]
pub struct TypedUseDeclaration {
    pub is_public: bool,
    pub target: TypedUseTarget,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedEnumDeclaration {
    pub name: SymbolLocator,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedUseTarget {
    pub name: SymbolLocator,
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
    pub name: SymbolLocator,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedBlock {
    pub statements: Vec<TypedStmnt>,
    pub return_type: EvaluatedType,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedShorthandVariableDeclaration {
    pub name: SymbolLocator,
    pub value: TypedExpr,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedConstantDeclaration {
    pub name: SymbolLocator,
    pub value: TypedExpr,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedModelDeclaration {
    pub name: SymbolLocator,
    pub body: TypedModelBody,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedModelBody {
    pub properties: Vec<TypedModelProperty>,
    pub constructor: Option<TypedModelConstructor>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedModelConstructor {
    pub parameters: Vec<SymbolIndex>,
    pub block: TypedBlock,
}

#[derive(Debug, PartialEq)]
pub struct TypedModelProperty {
    pub name: SymbolLocator,
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
    pub value: Option<TypedExpr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedWhileStatement {
    pub condition: TypedExpr,
    pub body: TypedBlock,
    pub span: Span,
}
