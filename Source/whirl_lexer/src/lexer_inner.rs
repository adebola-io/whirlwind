use crate::{
    error::{LexError, LexErrorPos, LexErrorType},
    token::{Comment, Operator, Span, Token, TokenType},
};

/// Shorthand to generate tokens, while checking if the next character matches a pattern, for multi-character tokens.
macro_rules! token {
    // Empty token.
    ($self: expr) => {
        Token {
            token_type: TokenType::EOF,
            span: $self.report_span(),
        }
    };
    (Operator::$operator: ident, $self: expr) => {
        Token {
            token_type: TokenType::Operator(Operator::$operator),
            span: $self.report_span(),
        }
    };
    (Comment::$comment: ident ($text: ident), $self: expr) => {
        Token {
            token_type: TokenType::Comment(Comment::$comment($text)),
            span: $self.report_span(),
        }
    };
    (Operator::$default: ident, [$($match: expr, Operator::$result: ident),*], $self: expr) => {{
        if let Some(ch) = $self.next_char() {
            match ch {
                $($match => return token!(Operator::$result, $self),)*
                _ => $self.stash(ch)
            }
        };
        let mut token = token!(Operator::$default, $self);
        token.span.end[1] -= 1;
        token
    }};
}

pub trait LexerInner {
    fn next_token_inner(&mut self) -> Option<Token> {
        match self.remove_stashed() {
            Some(ch) => Some(self.token(ch)),
            None => self.next_char().map(|ch| self.token(ch)),
        }
        .map(|token| {
            if token.token_type == TokenType::EOF {
                None
            } else {
                Some(token)
            }
        })
        .flatten()
    }

    fn token(&mut self, ch: char) -> Token {
        self.start_span();
        match ch {
            // Lexing comments.
            '/' => {
                if let Some(ch) = self.next_char() {
                    match ch {
                        // Lex a block comment.
                        '*' => return self.block_comment(),
                        // Lex a line or doc comment.
                        '/' => return self.line_or_doc_comment(),
                        _ => self.stash(ch),
                    }
                }
                token!(Operator::Divide, self)
            }
            // Lex a string.
            ch @ ('\'' | '"') => self.string(ch),
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
            // Skip whitespaces.
            '\n' | '\t' | ' ' | '\r' => loop {
                match self.next_char() {
                    Some('\n' | '\t' | ' ' | '\r') => {}
                    Some(ch) => return self.token(ch),
                    None => return token!(self),
                }
            },
            _ => todo!(),
        }
    }

    /// Gets the next character from the text while advancing the cursor.
    fn next_char(&mut self) -> Option<char>;
    /// Marks the start of a new token
    fn start_span(&mut self);
    /// Returns the current position of the cursor.
    fn current_pos(&self) -> [usize; 2];
    /// Returns the span of the current token.
    fn report_span(&self) -> Span;

    /// Removes the stashed character.
    fn remove_stashed(&mut self) -> Option<char>;
    /// Stashes a character.
    fn stash(&mut self, ch: char);

    /// Note an error.
    fn add_error(&mut self, error: LexError);

    /// Lexes a block comment. It assumes that the first `/*` has been encountered.
    fn block_comment(&mut self) -> Token {
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
            token_type: if is_doc_comment {
                TokenType::doc_comment(text)
            } else {
                TokenType::line_comment(text)
            },
            span: self.report_span(),
        }
    }
    // Lexes a string.
    fn string(&mut self, quote_type: char) -> Token {
        todo!()
    }
}
