use crate::{
    AssignOperator, BinOperator, ExpressionPrecedence, LogicOperator, Number, Span, UnaryOperator,
    UpdateOperator,
};

pub fn is_valid_identifier(ch: char) -> bool {
    matches!(ch, '0'..='9' | 'A'..='Z' | 'a'..='z' | '_')
}
pub fn is_valid_identifier_start(ch: char) -> bool {
    matches!(ch, 'A'..='Z' | 'a'..='z' | '_')
}

/// A token is the smallest lexical unit of a Whirlwind program.
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
    Exclamation,        // !
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
    Asterisk,           // *
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
            Operator::Exclamation | Operator::Not => Self::Negation,
            Operator::Equal | Operator::NotEqual => Self::Equality,
            Operator::Assign | Operator::MinusAssign | Operator::PlusAssign => Self::Assignment,
            Operator::And | Operator::Or | Operator::LogicalAnd | Operator::LogicalOr => {
                Self::Logic
            }
            Operator::Plus | Operator::Minus => Self::AddOrSubtract,
            Operator::BitOr | Operator::Ampersand => Self::BitLogic,
            Operator::Divide | Operator::Asterisk | Operator::Percent => {
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
            Operator::Asterisk => Self::Multiply,
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
            Operator::LesserThan => Self::LessThan,
            Operator::GreaterThan => Self::GreaterThan,
            Operator::LesserThanOrEqual => Self::LessThanOrEquals,
            Operator::GreaterThanOrEqual => Self::GreaterThanOrEquals,
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
            Operator::Exclamation => Self::Negation,
            Operator::Not => Self::NegationLiteral,
            Operator::Plus => Self::Plus,
            Operator::Minus => Self::Minus,
            _ => panic!("Cannot convert {:?} to unary operator!", value),
        }
    }
}

impl From<Operator> for UpdateOperator {
    fn from(value: Operator) -> Self {
        match value {
            Operator::Exclamation => Self::Assert,
            Operator::QuestionMark => Self::TryFrom,
            _ => panic!("Cannot convert {:?} to an update operator!", value),
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
    Continue,
    Else,
    Enum,
    False,
    For,
    Fn,
    Function,
    If,
    In,
    Implements,
    Model,
    Module,
    New,
    Public,
    Record,
    Return,
    Static,
    Switch,
    Test,
    This,
    #[allow(non_camel_case_types)]
    _this,
    Interface,
    True,
    Type,
    Use,
    Var,
    While,
}

pub fn is_keyword_or_operator(text: &str) -> bool {
    match text {
        "as" | "and" | "async" | "break" | "case" | "const" | "model" | "continue" | "else"
        | "enum" | "false" | "for" | "fn" | "function" | "if" | "in" | "interface" | "is"
        | "implements" | "new" | "not" | "or" | "public" | "record" | "return" | "static"
        | "switch" | "test" | "This" | "this" | "true" | "type" | "use" | "var" | "while"
        | "module" => true,
        _ => false,
    }
}
