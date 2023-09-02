/// A token is the smallest lexical unit of a Whirl program.
#[derive(PartialEq, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

/// Represents a range in the input text.
#[derive(PartialEq, Debug)]
pub struct Span {
    pub start: [usize; 2],
    pub end: [usize; 2],
}

impl From<[usize; 4]> for Span {
    fn from(value: [usize; 4]) -> Self {
        Span {
            start: [value[0], value[1]],
            end: [value[2], value[3]],
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum TokenType {
    Comment(Comment),
    Keyword(Keyword),
    Operator(Operator),
    Ident(String),
    LineComment(String),
    String(String),
    TemplateStringFragment(String),
    Number(f64),
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

#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, Debug)]
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
    Trait,
    Type,
    Use,
    Var,
    While,
}
