use crate::SymbolIndex;

use super::{LiteralIndex, SymbolLocator};
use whirl_ast::Span;

#[derive(Debug, PartialEq)]
pub enum TypedExpr {
    Ident(TypedIdent),
    Literal(LiteralIndex),
    NewExpr(Box<TypedNewExpr>),
    ThisExpr(TypedThisExpr),
    CallExpr(Box<TypedCallExpr>),
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
