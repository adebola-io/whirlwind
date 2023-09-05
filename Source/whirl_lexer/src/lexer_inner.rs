use whirl_ast::Span;

use crate::{
    error::{LexError, LexErrorPos, LexErrorType},
    token::{Bracket, Comment, Keyword, Operator, Token, TokenType},
};

/// Shorthand to generate tokens, while checking if the next character matches a pattern, for multi-character tokens.
macro_rules! token {
    // Comments.
    (Comment::$comment: ident ($text: ident), $self: expr) => {
        Some(Token {
           _type: TokenType::Comment(Comment::$comment($text)),
            span: $self.report_span(),
        })
    };
    // Operators.
    (Operator::$operator: ident, $self: expr) => {
        Some(Token {
           _type: TokenType::Operator(Operator::$operator),
            span: $self.report_span(),
        })
    };
    // Multichar operators.
    (Operator::$default: ident, [$($match: expr, Operator::$result: ident),*], $self: expr) => {{
        let mut token;
        if let Some(ch) = $self.next_char() {
            match ch {
                $($match => token = token!(Operator::$result, $self).unwrap(),)*
                _ => {
                    token = token!(Operator::$default, $self).unwrap();
                    token.span.end[1] -= 1;
                    $self.stash(ch)
                }
            }
        } else {
            token = token!(Operator::$default, $self).unwrap()
        }
        Some(token)
    }};
    (Keyword::$keyword: ident, $self: expr) => {
        Token {
           _type: TokenType::Keyword(Keyword::$keyword),
            span: $self.report_span()
        }
    };
    // Brackets
    (Bracket::$bracket: ident, $self: expr) => {
        Some(Token {
           _type: TokenType::Bracket(Bracket::$bracket),
            span: $self.report_span(),
        })
    };
}

fn is_valid_identifier(ch: char) -> bool {
    matches!(ch, 'A'..='Z' | 'a'..='z' | '_')
}

pub trait LexerInner {
    fn next_token_inner(&mut self) -> Option<Token> {
        match self.remove_stashed() {
            Some(ch) => self.token(ch),
            None => self.next_char().map(|ch| self.token(ch)).flatten(),
        }
    }

    /// Scans a token.
    fn token(&mut self, ch: char) -> Option<Token> {
        self.start_span();
        match ch {
            // Lexing comments.
            '/' => {
                if let Some(ch) = self.next_char() {
                    match ch {
                        // Lex a block comment.
                        '*' => return self.block_comment(),
                        // Lex a line or doc comment.
                        '/' => return Some(self.line_or_doc_comment()),
                        _ => self.stash(ch),
                    }
                }
                token!(Operator::Divide, self)
            }
            // Lex a string.
            ch @ ('\'' | '"') => Some(self.string(ch)),
            // Operators.
            ':' => token!(Operator::Colon, ['=', Operator::ColonAssign], self),
            ';' => token!(Operator::SemiColon, self),
            '.' => token!(Operator::Dot, ['.', Operator::Range], self),
            '!' => token!(Operator::Negator, ['=', Operator::NotEqual], self),
            '?' => token!(Operator::QuestionMark, self),
            ',' => token!(Operator::Comma, self),
            '=' => token!(
                Operator::Assign,
                ['>', Operator::Arrow, '=', Operator::Equal],
                self
            ),
            '&' => token!(Operator::Ampersand, ['&', Operator::LogicalAnd], self),
            '|' => token!(Operator::BitOr, ['|', Operator::LogicalOr], self),
            '+' => token!(Operator::Plus, ['=', Operator::PlusAssign], self),
            '-' => token!(Operator::Minus, ['=', Operator::MinusAssign], self),
            '*' => token!(Operator::Multiply, self),
            '%' => token!(Operator::Percent, self),
            '^' => token!(Operator::Carat, self),
            '<' => token!(
                Operator::LesserThan,
                ['<', Operator::LeftShift, '=', Operator::LesserThanOrEqual],
                self
            ),
            '>' => token!(
                Operator::GreaterThan,
                ['>', Operator::RightShift, '=', Operator::GreaterThanOrEqual],
                self
            ),
            '(' => token!(Bracket::LParens, self),
            ')' => token!(Bracket::RParens, self),
            '[' => token!(Bracket::LSquare, self),
            ']' => token!(Bracket::RSquare, self),
            '{' => token!(Bracket::LCurly, self),
            '}' => token!(Bracket::RCurly, self),
            // Skip whitespaces.
            '\n' | '\t' | ' ' | '\r' => loop {
                match self.next_char() {
                    Some('\n' | '\t' | ' ' | '\r') => {}
                    Some(ch) => return self.token(ch),
                    None => return None,
                }
            },
            ch if is_valid_identifier(ch) => Some(self.ident_or_keyword(ch)),
            // Invalid characters.
            ch => {
                self.add_error(LexError {
                    error_type: LexErrorType::InvalidCharacter(ch),
                    position: LexErrorPos::Span(self.report_span()),
                });
                Some(Token {
                    _type: TokenType::Invalid(ch),
                    span: self.report_span(),
                })
            }
        }
    }

    /// Gets the next character from the text while advancing the cursor.
    fn next_char(&mut self) -> Option<char>;
    /// Marks the start of a new token
    fn start_span(&mut self);
    /// Returns the current position of the cursor.
    fn current_pos(&self) -> [u32; 2];
    /// Returns the span of the current token.
    fn report_span(&self) -> Span;
    /// Returns the length of each line that has been completed so far.
    /// For convenience, it is one-based rather than zero, i.e. the length of line 1 is `self.line_lengths[1]`.
    fn line_lengths(&self) -> &[u32];

    /// Removes the stashed character.
    fn remove_stashed(&mut self) -> Option<char>;
    /// Stashes a character.
    fn stash(&mut self, ch: char);

    /// Note an error.
    fn add_error(&mut self, error: LexError);

    /// Lexes a block comment. It assumes that the first `/*` has been encountered.
    fn block_comment(&mut self) -> Option<Token> {
        let mut comment_text = String::new();
        let mut text_ended = false;

        while !text_ended {
            match self.next_char() {
                // Maybe end.
                Some(ch1 @ '*') => match self.next_char() {
                    // End of comment
                    Some('/') => break,
                    Some(ch2) => {
                        comment_text.push(ch1);
                        comment_text.push(ch2);
                    }
                    None => text_ended = true,
                },
                Some(ch) => comment_text.push(ch),
                None => text_ended = true,
            }
        }

        if text_ended {
            self.add_error(LexError {
                error_type: LexErrorType::UnexpectedEndOfInput,
                position: LexErrorPos::Point(self.current_pos()),
            });
        }

        token!(Comment::BlockComment(comment_text), self)
    }

    /// Lexes a line comment. It assumes that `//` has already been encountered.
    fn line_or_doc_comment(&mut self) -> Token {
        let mut text = String::new();
        let mut is_doc_comment = false;

        if let Some(ch) = self.next_char() {
            if ch == '/' {
                is_doc_comment = true;
            } else {
                text.push(ch)
            }
        }

        loop {
            match self.next_char() {
                Some('\n') | None => break,
                Some(ch) => text.push(ch),
            }
        }
        Token {
            _type: if is_doc_comment {
                TokenType::doc_comment(text)
            } else {
                TokenType::line_comment(text)
            },
            span: self.report_span(),
        }
    }

    /// Lexes a string.
    fn string(&mut self, _quote_type: char) -> Token {
        todo!()
    }

    /// Lexes an identifier or a keyword. It also lexes word operators like `is`, `and`, `or` and `not`.
    fn ident_or_keyword(&mut self, first_char: char) -> Token {
        let mut ident_text = String::from(first_char);
        let mut is_ended = false;
        let mut encounted_newline = false;

        loop {
            match self.next_char() {
                Some(ch) if is_valid_identifier(ch) => ident_text.push(ch),
                Some(ch) => {
                    if ch == '\n' {
                        encounted_newline = true
                    }
                    self.stash(ch);
                    break;
                }
                None => {
                    is_ended = true;
                    break;
                }
            }
        }
        let mut token = match ident_text.as_str() {
            "as" => token!(Keyword::As, self),
            "and" => token!(Operator::And, self).unwrap(),
            "async" => token!(Keyword::Async, self),
            "case" => token!(Keyword::Case, self),
            "const" => token!(Keyword::Const, self),
            "class" => token!(Keyword::Class, self),
            "continue" => token!(Keyword::Continue, self),
            "else" => token!(Keyword::Else, self),
            "enum" => token!(Keyword::Enum, self),
            "extends" => token!(Keyword::Extends, self),
            "for" => token!(Keyword::For, self),
            "fn" => token!(Keyword::Fn, self),
            "function" => token!(Keyword::Function, self),
            "if" => token!(Keyword::If, self),
            "in" => token!(Keyword::In, self),
            "is" => token!(Operator::Is, self).unwrap(),
            "implements" => token!(Keyword::Implements, self),
            "new" => token!(Keyword::New, self),
            "not" => token!(Operator::Not, self).unwrap(),
            "or" => token!(Operator::Not, self).unwrap(),
            "public" => token!(Keyword::Public, self),
            "record" => token!(Keyword::Record, self),
            "return" => token!(Keyword::Return, self),
            "static" => token!(Keyword::Static, self),
            "switch" => token!(Keyword::Switch, self),
            "test" => token!(Keyword::Test, self),
            "trait" => token!(Keyword::Trait, self),
            "type" => token!(Keyword::Type, self),
            "use" => token!(Keyword::Use, self),
            "var" => token!(Keyword::Var, self),
            "while" => token!(Keyword::While, self),
            _ => Token {
                _type: TokenType::Ident(ident_text),
                span: self.report_span(),
            },
        };
        // Offset correction.
        if encounted_newline {
            token.span.end = [
                token.span.end[0] - 1,
                self.line_lengths()[(token.span.end[0] - 1) as usize],
            ];
        } else {
            token.span.end[1] -= if is_ended { 1 } else { 2 };
        }
        token
    }
}
