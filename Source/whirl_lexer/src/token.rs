use whirl_ast::Span;

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
    This,
    Trait,
    Type,
    Use,
    Var,
    While,
}
