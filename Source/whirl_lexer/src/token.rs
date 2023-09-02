/// A lexeme is the smallest lexical unit of a  program.
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

/// Represents a range in the input text.
pub struct Span {
    pub start: [usize; 2],
    pub end: [usize; 2],
}

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

pub enum Comment {
    LineComment(String),
    BlockComment(String),
}

pub enum Bracket {
    LParens,
    RParens,
    LSquare,
    RSquare,
    LCurly,
    RCurly,
}

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
