/// A lexeme is the smallest lexical unit of a  program.
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
    TemplateString(Vec<TokenType>),
    Number(f64),
    Bracket(Bracket),
    EOF,
}

impl TokenType {
    pub fn block_comment(text: String) -> Self {
        TokenType::Comment(Comment::BlockComment(text))
    }
}

#[derive(PartialEq, Debug)]
pub enum Comment {
    LineComment(String),
    BlockComment(String),
}

#[derive(PartialEq, Debug)]
pub enum Bracket {
    LParens,
    RParens,
    LSquare,
    RSquare,
    LCurly,
    RCurly,
}

#[derive(PartialEq, Debug)]
pub enum Operator {
    Colon,
    SemiColon,
    Range,
    Dot,
    QuestionMark,
    Comma,
    Arrow,
    // Logical Operators.
    And,
    Not,
    Or,
    LogicalAnd,
    LogicalOr,
    // Assignment Operators.
    Assign,
    PlusAssign,
    SubtractAssign,
    ColonAssign,
    // Binary Operators.
    Add,
    Subtract,
    Divide,
    Multiply,
    Remainder,
    PowerOf,
    BitAnd,
    BitOr,
    BitLeftShift,
    BitRightShift,
    GreaterThan,
    LesserThan,
    Is,
    // Unary Operators.
    Negator,
    LogicalNot,
}

#[derive(PartialEq, Debug)]
pub enum Keyword {
    As,
    Async,
    Break,
    Case,
    Const,
    Continue,
    Colon,
    Class,
    Else,
    Enum,
    Extends,
    For,
    Fn,
    Function,
    If,
    In,
    Implements,
    LBracket,
    New,
    Public,
    RBracket,
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
