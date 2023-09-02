use crate::{
    error::{LexError, LexErrorPos, LexErrorType},
    token::{Operator, Span, Token, TokenType},
};

pub trait LexerInner {
    fn next_token_inner(&mut self) -> Option<Token> {
        self.next_char().map(|char| match char {
            // Lexing comments.
            '/' => match self.next_char() {
                // Lex a block comment.
                Some('*') => self.block_comment(),
                // Lex a line or doc comment.
                Some('/') => self.line_or_doc_comment(),
                Some(_) => todo!(),
                None => Token {
                    token_type: TokenType::Operator(Operator::Divide),
                    span: self.report_span(),
                },
            },
            // Lex a string.
            ch @ ('\'' | '"') => self.string(ch),
            ';' => Token {
                token_type: TokenType::Operator(Operator::SemiColon),
                span: self.report_span(),
            },
            _ => todo!(),
        })
    }

    /// Gets the next character from the text while advancing the cursor.
    fn next_char(&mut self) -> Option<char>;
    /// Marks the start of a new token
    fn start_span(&mut self, offset: usize);
    /// Returns the current position of the cursor.
    fn current_pos(&self) -> [usize; 2];
    /// Returns the span of the current token.
    fn report_span(&self) -> Span;

    /// Note an error.
    fn add_error(&mut self, error: LexError);

    /// Lexes a block comment. It assumes that the first `/*` has been encountered.
    fn block_comment(&mut self) -> Token {
        self.start_span(2);
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

        Token {
            token_type: TokenType::block_comment(comment_text),
            span: self.report_span(),
        }
    }
    /// Lexes a line comment. It assumes that `//` has already been encountered.
    fn line_or_doc_comment(&mut self) -> Token {
        self.start_span(2);

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
