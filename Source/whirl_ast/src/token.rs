use crate::{
    AssignOperator, BinOperator, ExpressionPrecedence, LogicOperator, Number, Span, UnaryOperator,
};

/// A token is the smallest lexical unit of a Whirl program.
#[derive(PartialEq, Debug)]
pub struct Token {
    pub _type: TokenType,
    pub span: Span,
}

#[derive(PartialEq, Debug)]
pub enum TokenType {
    Comment(Comment),
    Keyword(Keyword),
    Operator(Operator),
    Ident(String),
    String(String),
    TemplateStringFragment(String),
    Number(Number),
    Bracket(Bracket),
    Invalid(char),
}

impl TokenType {
    pub fn block_comment(text: String) -> Self {
        TokenType::Comment(Comment::BlockComment(text))
    }
    pub fn line_comment(text: String) -> Self {
        TokenType::Comment(Comment::LineComment(text))
    }
    pub fn doc_comment(text: String) -> Self {
        TokenType::Comment(Comment::DocComment(text))
    }
    pub fn lparen() -> Self {
        TokenType::Bracket(Bracket::LParens)
    }
    pub fn rparens() -> Self {
        TokenType::Bracket(Bracket::RParens)
    }
}

#[derive(PartialEq, Debug)]
pub enum Comment {
    LineComment(String),
    BlockComment(String),
    DocComment(String),
}

#[derive(PartialEq, Debug)]
pub enum Bracket {
    LParens, // (
    RParens, // )
    LSquare, // [
    RSquare, // ]
    LCurly,  // {
    RCurly,  // }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Operator {
    Colon,              // :
    ColonAssign,        // :=
    SemiColon,          // ;
    Dot,                // .
    Range,              // ..
    Negator,            // !
    NotEqual,           // !=
    QuestionMark,       // ?
    Comma,              // ,
    Assign,             // =
    Arrow,              // =>
    And,                // and
    Not,                // not
    Or,                 // or
    Ampersand,          // &
    LogicalAnd,         // &&
    BitOr,              // |
    LogicalOr,          // ||
    Plus,               // +
    PlusAssign,         // +=
    Minus,              // -
    MinusAssign,        // -=
    Divide,             // /
    Multiply,           // *
    Percent,            // %
    Carat,              // ^
    LeftShift,          // <<
    RightShift,         // >>
    LesserThanOrEqual,  // <=
    GreaterThanOrEqual, // >=
    Equal,              // ==
    LesserThan,         // <
    GreaterThan,        // >
    Is,                 // is
}

impl From<Operator> for ExpressionPrecedence {
    fn from(value: Operator) -> Self {
        match value {
            Operator::Dot => Self::Access,
            Operator::Range => Self::Range,
            Operator::Negator | Operator::Not => Self::Negation,
            Operator::Equal | Operator::NotEqual => Self::Equality,
            Operator::Assign | Operator::MinusAssign | Operator::PlusAssign => Self::Assignment,
            Operator::And | Operator::Or | Operator::LogicalAnd | Operator::LogicalOr => {
                Self::Logic
            }
            Operator::Plus | Operator::Minus => Self::AddOrSubtract,
            Operator::BitOr | Operator::Ampersand => Self::BitLogic,
            Operator::Divide | Operator::Multiply | Operator::Percent => {
                Self::MultiplyDivideOrRemainder
            }
            Operator::Carat => Self::PowerOf,
            Operator::LeftShift | Operator::RightShift => Self::BitShift,
            Operator::LesserThanOrEqual
            | Operator::GreaterThanOrEqual
            | Operator::LesserThan
            | Operator::GreaterThan => Self::Ordering,
            Operator::Is => Self::ReferentialEquality,
            _ => panic!("Cannot convert {:?} to precedence.", value),
        }
    }
}

impl From<Operator> for BinOperator {
    fn from(value: Operator) -> Self {
        match value {
            Operator::Multiply => Self::Multiply,
            Operator::Divide => Self::Divide,
            Operator::Carat => Self::PowerOf,
            Operator::Ampersand => Self::BitAnd,
            Operator::Is => Self::Is,
            Operator::Equal => Self::Equals,
            Operator::NotEqual => Self::NotEquals,
            Operator::Percent => Self::Remainder,
            Operator::Plus => Self::Add,
            Operator::Minus => Self::Subtract,
            Operator::Range => Self::Range,
            _ => panic!("Cannot convert {:?} to binary operator!", value),
        }
    }
}

impl From<Operator> for LogicOperator {
    fn from(value: Operator) -> Self {
        match value {
            Operator::And => Self::AndLiteral,
            Operator::LogicalAnd => Self::And,
            Operator::Or => Self::OrLiteral,
            Operator::LogicalOr => Self::Or,
            _ => panic!("Cannot convert {:?} to logical operator!", value),
        }
    }
}

impl From<Operator> for UnaryOperator {
    fn from(value: Operator) -> Self {
        match value {
            Operator::Negator => Self::Negation,
            Operator::Not => Self::NegationLiteral,
            Operator::Plus => Self::Plus,
            Operator::Minus => Self::Minus,
            _ => panic!("Cannot convert {:?} to unary operator!", value),
        }
    }
}

impl From<Operator> for AssignOperator {
    fn from(value: Operator) -> Self {
        match value {
            Operator::Assign => Self::Assign,
            Operator::PlusAssign => Self::PlusAssign,
            Operator::MinusAssign => Self::MinusAssign,
            _ => panic!("Cannot convert {:?} to assignment operator!", value),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Keyword {
    As,
    Async,
    Break,
    Case,
    Const,
    Class,
    Continue,
    Else,
    Enum,
    Extends,
    False,
    For,
    Fn,
    Function,
    If,
    In,
    Implements,
    New,
    Public,
    Record,
    Return,
    Static,
    Switch,
    Test,
    This,
    Trait,
    True,
    Type,
    Use,
    Var,
    While,
}
