use crate::Span;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    StringLiteral(WhirlString),
    NumberLiteral(WhirlNumber),
}

#[derive(Debug, PartialEq)]
pub struct WhirlString {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct WhirlNumber {
    pub value: Number,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(PartialEq, Debug)]
pub enum Number {
    Binary(String),
    Octal(String),
    Hexadecimal(String),
    Decimal(String),
}

/// Chart for expression precedence in Whirl.
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub enum ExpressionPrecedence {
    Access = 1,           // a.b
    Call = 2,             // a(b)
    New = 3,              // new a
    PowerOf = 4,          // a ^ b
    MultiplyOrDivide = 5, // a * b, a / b
    AddOrSubtract = 6,    // a + b, a - b
    BitLogic = 7,         // a | b, a & b
    Logic = 8,            // a || b, a && b
    Equality = 9,         // a == b, a != b
    TypeUnion = 10,
}

impl Expression {
    pub fn span(&self) -> Span {
        todo!()
    }

    pub(crate) fn set_start(&mut self, start: [u32; 2]) {
        match self {
            Expression::Identifier(i) => i.span.start = start,
            Expression::StringLiteral(s) => s.span.start = start,
            Expression::NumberLiteral(n) => n.span.start = start,
        }
    }
}
