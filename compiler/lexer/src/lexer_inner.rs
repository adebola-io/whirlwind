use ast::{
    is_valid_identifier, is_valid_identifier_start, Bracket, Comment, Keyword, Operator, Token,
    TokenType,
};
use ast::{Number, Span};

use errors::{LexError, LexErrorPos, LexErrorType};

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

pub trait LexerInner {
    fn next_token_inner(&mut self) -> Option<Token> {
        self.remove_saved().or_else(|| match self.remove_stashed() {
            Some(ch) => self.token(ch),
            None => self.next_char().map(|ch| self.token(ch)).flatten(),
        })
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
            ch @ ('\'' | '"' | '`') => Some(self.string(ch)),
            // Operators.
            ':' => token!(Operator::Colon, ['=', Operator::ColonAssign], self),
            ';' => token!(Operator::SemiColon, self),
            '.' => token!(Operator::Dot, ['.', Operator::Range], self),
            '!' => token!(Operator::Exclamation, ['=', Operator::NotEqual], self),
            '?' => token!(Operator::QuestionMark, self),
            ',' => token!(Operator::Comma, self),
            '@' => token!(Operator::At, self),
            '=' => token!(
                Operator::Assign,
                ['>', Operator::Arrow, '=', Operator::Equal],
                self
            ),
            '&' => token!(Operator::Ampersand, ['&', Operator::LogicalAnd], self),
            '|' => token!(
                Operator::BitOr,
                ['|', Operator::LogicalOr, '=', Operator::Constraint],
                self
            ),
            '+' => token!(Operator::Plus, ['=', Operator::PlusAssign], self),
            '-' => token!(
                Operator::Minus,
                ['=', Operator::MinusAssign, '>', Operator::Arrow],
                self
            ),
            '*' => token!(Operator::Asterisk, self),
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
            ch if ch.is_numeric() => Some(self.number(ch)),
            ch if is_valid_identifier_start(ch) => Some(self.ident_or_keyword(ch)),
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

    /// Store a token that was predictively parsed to be delivered on the next iteration.
    fn save_token(&mut self, token: Token);
    /// Remove the saved token.
    fn remove_saved(&mut self) -> Option<Token>;

    /// Gets the next character from the text while advancing the cursor.
    fn next_char(&mut self) -> Option<char>;
    /// Marks the start of a new token
    fn start_span(&mut self);
    /// Returns the current position of the cursor.
    fn current_pos(&self) -> [u32; 2];
    /// Returns the span of the current token.
    fn report_span(&self) -> Span;
    /// Returns the length of each line that has been completed so far.
    /// It is zero-based, meaning that the length of line 1 is `line_lengths[0]`.
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
        let mut is_ended = false;

        if let Some(ch) = self.next_char() {
            if ch == '/' {
                is_doc_comment = true;
            } else if ch == '\n' {
                is_ended = true;
            } else {
                text.push(ch)
            }
        }

        if !is_ended {
            loop {
                match self.next_char() {
                    Some('\n') | None => break,
                    Some(ch) => text.push(ch),
                }
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
        let mut value = String::new();
        let mut is_ended = false;

        loop {
            match self.next_char() {
                Some('\\') => value.push(self.escape()),
                Some(ch) => {
                    // End of string.
                    if ch == _quote_type {
                        break;
                    }
                    value.push(ch)
                }
                None => {
                    is_ended = true;
                    break;
                }
            }
        }
        // Unterminated string.
        if is_ended {
            self.add_error(LexError::unterminated_string(LexErrorPos::Point(
                self.report_span().end,
            )));
        }
        let span = self.report_span();
        Token {
            _type: TokenType::String(value),
            span,
        }
    }

    /// Parses an escaped sequence. It assumes the last char was a backslash.
    fn escape(&mut self) -> char {
        // Get the next character from the input string
        if let Some(c) = self.next_char() {
            match c {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '\'' => '\'',
                '\"' => '\"',
                _ => {
                    // Handle hexadecimal escape sequences like '\xHH'
                    if c == 'x' {
                        let mut hex_chars = String::new();
                        for _ in 0..2 {
                            if let Some(hex_char) = self.next_char() {
                                hex_chars.push(hex_char);
                            } else {
                                // Handle incomplete hexadecimal escape sequence
                                return '\0';
                            }
                        }
                        if let Ok(escaped_char) = u8::from_str_radix(&hex_chars, 16) {
                            return char::from(escaped_char);
                        } else {
                            // Handle invalid hexadecimal escape sequence
                            return '\0';
                        }
                    } else {
                        // Handle other escape sequences or invalid escapes
                        return '\0';
                    }
                }
            }
        } else {
            // Handle the case where there are no more characters to parse
            return '\0';
        }
    }

    /// Lexes a number.
    fn number(&mut self, first_char: char) -> Token {
        let mut value = String::new();

        // let mut is_ended = false;
        let mut encountered_newline = false;
        let mut encountered_decimal = false;

        let mut stored_char = None;
        // Check for binary, octal of hexadecimal numbers.
        let radix: u32 = if first_char == '0' {
            match self.next_char() {
                Some('x') => 16,
                Some('o') => 8,
                Some('b') => 2,
                Some(ch) => {
                    stored_char = Some(ch);
                    10
                }
                None => 10,
            }
        } else {
            10
        };

        if radix == 10 {
            value.push(first_char);
        }

        loop {
            let next = stored_char.take().or_else(|| self.next_char());
            match next {
                Some(ch) if ch.is_digit(radix) => value.push(ch),
                Some(ch) if ch == 'e' => {
                    // Only decimal numbers should have exponent.
                    if radix != 10 {
                        self.add_error(LexError {
                            error_type: LexErrorType::ExponentforInvalidBase,
                            position: LexErrorPos::Point(self.report_span().end),
                        })
                    }
                    let tuple = self.exponent();
                    value.push_str(&tuple.0);
                    match tuple.1 {
                        Some('\n') => encountered_newline = true,
                        // None => is_ended = true,
                        _ => {}
                    }
                    break;
                }
                Some(ch) if ch == '.' => {
                    if encountered_decimal {
                        // parse a range instead.
                        if value.ends_with('.') {
                            value.remove(value.len() - 1);
                            let mut token = token!(Operator::Range, self).unwrap();
                            // offset correction.
                            token.span.start[1] = token.span.end[1] - 2;
                            self.save_token(token);
                        } else {
                            self.stash(ch);
                        }
                        break;
                    }
                    value.push(ch);
                    encountered_decimal = true;
                }
                Some(ch) => {
                    if ch == '\n' {
                        encountered_newline = true;
                    }
                    self.stash(ch);
                    break;
                }
                None => {
                    // is_ended = true;
                    break;
                }
            }
        }
        let len = value.len();

        let mut token = Token {
            _type: TokenType::Number(match radix {
                2 => Number::Binary(value),
                8 => Number::Octal(value),
                10 => Number::Decimal(value),
                16 => Number::Hexadecimal(value),
                _ => unreachable!(),
            }),
            span: self.report_span(),
        };

        // Offset correction.
        if encountered_newline {
            token.span.end = [
                token.span.end[0] - 1,
                self.line_lengths()[(token.span.end[0] - 2) as usize],
            ];
        } else {
            token.span.end[1] = token.span.start[1] + len as u32 + if radix != 10 { 2 } else { 0 };
        }
        token
    }

    /// Parses an exponent.
    fn exponent(&mut self) -> (String, Option<char>) {
        let mut exponent_value = String::from('e');
        let mut last_ch = None;

        loop {
            match self.next_char() {
                Some('+') if exponent_value == "e" => exponent_value.push('+'),
                Some('-') if exponent_value == "e" => exponent_value.push('-'),
                Some(ch) if ch.is_digit(10) => exponent_value.push(ch),
                Some(ch) => {
                    if exponent_value.is_empty() {
                        self.add_error(LexError::no_value_after_exponent(LexErrorPos::Point(
                            self.report_span().end,
                        )))
                    }
                    self.stash(ch);
                    last_ch = Some(ch);
                    break;
                }
                None => {
                    if exponent_value.is_empty() {
                        self.add_error(LexError::no_value_after_exponent(LexErrorPos::Point(
                            self.report_span().end,
                        )))
                    }
                    break;
                }
            }
        }
        return (
            if exponent_value.is_empty() {
                format!("1")
            } else {
                exponent_value
            },
            last_ch,
        );
    }
    /// Lexes an identifier or a keyword. It also lexes word operators like `is`, `and`, `or` and `not`.
    fn ident_or_keyword(&mut self, first_char: char) -> Token {
        let mut ident_text = String::from(first_char);

        loop {
            match self.next_char() {
                Some(ch) if is_valid_identifier(ch) => ident_text.push(ch),
                Some(ch) => {
                    self.stash(ch);
                    break;
                }
                None => break,
            }
        }
        let len = ident_text.len();
        let mut token = match ident_text.as_str() {
            "as" => token!(Keyword::As, self),
            "and" => token!(Operator::And, self).unwrap(),
            "async" => token!(Keyword::Async, self),
            "break" => token!(Keyword::Break, self),
            "case" => token!(Keyword::Case, self),
            "const" => token!(Keyword::Const, self),
            "model" => token!(Keyword::Model, self),
            "continue" => token!(Keyword::Continue, self),
            "else" => token!(Keyword::Else, self),
            "enum" => token!(Keyword::Enum, self),
            "false" => token!(Keyword::False, self),
            "for" => token!(Keyword::For, self),
            "fn" => token!(Keyword::Fn, self),
            "function" => token!(Keyword::Function, self),
            "if" => token!(Keyword::If, self),
            "in" => token!(Keyword::In, self),
            "is" => token!(Keyword::Is, self),
            "implements" => token!(Keyword::Implements, self),
            "import" => token!(Keyword::Import, self),
            "new" => token!(Keyword::New, self),
            "not" => token!(Operator::Not, self).unwrap(),
            "or" => token!(Operator::Or, self).unwrap(),
            "public" => token!(Keyword::Public, self),
            "record" => token!(Keyword::Record, self),
            "return" => token!(Keyword::Return, self),
            "static" => token!(Keyword::Static, self),
            "switch" => token!(Keyword::Switch, self),
            "test" => token!(Keyword::Test, self),
            "This" => token!(Keyword::This, self),
            "this" => token!(Keyword::_this, self),
            "interface" => token!(Keyword::Interface, self),
            "true" => token!(Keyword::True, self),
            "type" => token!(Keyword::Type, self),
            "use" => token!(Keyword::Use, self),
            "var" => token!(Keyword::Var, self),
            "while" => token!(Keyword::While, self),
            "module" => token!(Keyword::Module, self),
            _ => Token {
                _type: TokenType::Ident(ident_text),
                span: self.report_span(),
            },
        };
        // Offset correction.
        // identifier chars always occupy the same line, so simple math can get its span.
        token.span.end = [token.span.start[0], token.span.start[1] + len as u32];

        token
    }
}
