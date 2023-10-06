use crate::{IntermediateType, SymbolIndex, TypedBlock};

use super::{LiteralIndex, SymbolLocator};
use ast::{BinOperator, Span};

#[derive(Debug, PartialEq)]
pub enum TypedExpr {
    Ident(TypedIdent),
    Literal(LiteralIndex),
    NewExpr(Box<TypedNewExpr>),
    ThisExpr(TypedThisExpr),
    CallExpr(Box<TypedCallExpr>),
    FnExpr(Box<TypedFnExpr>),
    Block(TypedBlock),
    BinaryExpr(Box<TypedBinExpr>),
}

#[derive(Debug, PartialEq)]
pub struct TypedIdent {
    pub value: SymbolLocator,
}

#[derive(Debug, PartialEq)]
pub struct TypedNewExpr {
    pub value: TypedExpr,
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
    pub caller: TypedExpr,
    pub arguments: Vec<TypedExpr>,
}

#[derive(Debug, PartialEq)]
pub struct TypedFnExpr {
    pub is_async: bool,
    pub generic_params: Vec<SymbolIndex>,
    pub params: Vec<SymbolIndex>,
    pub return_type: Option<IntermediateType>,
    pub body: TypedExpr,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypedBinExpr {
    pub left: TypedExpr,
    pub operator: BinOperator,
    pub right: TypedExpr,
    pub span: Span,
}
