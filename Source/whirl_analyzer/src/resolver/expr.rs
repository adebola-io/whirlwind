use whirl_ast::{LiteralIndex, Span, SymbolIndex, SymbolLocator};

pub enum Expr<'a> {
    Ident(Ident),
    Literal(LiteralIndex),
    NewExpr(NewExpr<'a>),
    ThisExpr(Ident),
    CallExpr(CallExpr<'a>),
}

pub struct Ident {
    pub value: SymbolLocator,
}

pub struct NewExpr<'a> {
    pub value: &'a Expr<'a>,
    pub span: Span,
}

pub struct ThisExprB {
    pub id: SymbolIndex,
    pub start_line: u32,
    pub start_char: u32,
}

pub struct CallExpr<'a> {
    pub caller: &'a Expr<'a>,
    pub arguments: Vec<Expr<'a>>,
}
