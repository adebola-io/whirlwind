use super::{LiteralIndex, SymbolLocator};
use crate::{IntermediateType, SymbolIndex, TypedBlock};
use ast::{AssignOperator, BinOperator, LogicOperator, Span, UnaryOperator, UpdateOperator};

#[derive(Debug, PartialEq)]
pub enum TypedExpression {
    Identifier(TypedIdent),
    Literal(LiteralIndex),
    NewExpr(Box<TypedNewExpr>),
    ThisExpr(TypedThisExpr),
    CallExpr(Box<TypedCallExpr>),
    FnExpr(Box<TypedFnExpr>),
    Block(TypedBlock),
    IfExpr(Box<TypedIfExpr>),
    AccessExpr(Box<TypedAccessExpr>),
    ArrayExpr(TypedArrayExpr),
    IndexExpr(Box<TypedIndexExpr>),
    BinaryExpr(Box<TypedBinExpr>),
    AssignmentExpr(Box<TypedAssignmentExpr>),
    UnaryExpr(Box<TypedUnaryExpr>),
    LogicExpr(Box<TypedLogicExpr>),
    UpdateExpr(Box<TypedUpdateExpr>),
}

#[derive(Debug, PartialEq)]
pub struct TypedIdent {
    pub value: SymbolLocator,
}

#[derive(Debug, PartialEq)]
pub struct TypedNewExpr {
    pub value: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedThisExpr {
    pub model_or_trait: Option<SymbolIndex>,
    pub start_line: u32,
    pub start_character: u32,
}

#[derive(Debug, PartialEq)]
pub struct TypedCallExpr {
    pub caller: TypedExpression,
    pub arguments: Vec<TypedExpression>,
}

#[derive(Debug, PartialEq)]
pub struct TypedFnExpr {
    pub is_async: bool,
    pub generic_params: Vec<SymbolIndex>,
    pub params: Vec<SymbolIndex>,
    pub return_type: Option<IntermediateType>,
    pub body: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedBinExpr {
    pub left: TypedExpression,
    pub operator: BinOperator,
    pub right: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedLogicExpr {
    pub left: TypedExpression,
    pub operator: LogicOperator,
    pub right: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedUnaryExpr {
    pub operator: UnaryOperator,
    pub operand: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedUpdateExpr {
    pub operator: UpdateOperator,
    pub operand: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedIfExpr {
    pub condition: TypedExpression,
    pub consequent: TypedBlock,
    pub alternate: Option<TypedElse>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedAccessExpr {
    pub object: TypedExpression,
    pub property: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedElse {
    pub expression: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedArrayExpr {
    pub elements: Vec<TypedExpression>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedIndexExpr {
    pub object: TypedExpression,
    pub index: TypedExpression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedAssignmentExpr {
    pub left: TypedExpression,
    pub operator: AssignOperator,
    pub right: TypedExpression,
    pub span: Span,
}
