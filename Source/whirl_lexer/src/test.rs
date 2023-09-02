#![cfg(test)]

use crate::lex_text;
use crate::{
    token::{Span, Token, TokenType},
    Lexer,
};

#[test]
fn lex_block_comments() {
    // Single Line.
    let mut lexer1 = lex_text("/* Hello, world */");

    assert_eq!(
        lexer1.get_next_token(),
        Some(Token {
            token_type: TokenType::block_comment(format!(" Hello, world ")),
            span: Span::from([1, 1, 1, 19]),
        })
    );

    // Multi Line.
    let mut lexer2 = lex_text(
        "/* Hello, 
    world */",
    );

    assert_eq!(
        lexer2.get_next_token(),
        Some(Token {
            token_type: TokenType::block_comment(format!(" Hello, \n    world ")),
            span: Span::from([1, 1, 2, 13]),
        })
    );

    // With asterisk and slashes.
    let mut lexer3 = lex_text("/* Hello, *** world!//* / */");
    assert_eq!(
        lexer3.get_next_token(),
        Some(Token {
            token_type: TokenType::block_comment(format!(" Hello, *** world!//* / ")),
            span: Span::from([1, 1, 1, 29]),
        })
    );
}

#[test]
fn lex_line_or_doc_comments() {
    // Only token.
    let mut lexer = lex_text("// This is the world premiere!!");

    assert_eq!(
        lexer.get_next_token(),
        Some(Token {
            token_type: TokenType::line_comment(format!(" This is the world premiere!!")),
            span: Span::from([1, 1, 1, 32])
        })
    );

    // doc comment.
    lexer = lex_text("/// This is the world premiere!!");

    assert_eq!(
        lexer.get_next_token(),
        Some(Token {
            token_type: TokenType::doc_comment(format!(" This is the world premiere!!")),
            span: Span::from([1, 1, 1, 33])
        })
    );

    // With block comment.
    lexer = lex_text(
        "// Hello world.
/* This is the story of a new world.*/",
    );
    assert_eq!(lexer.count(), 2)
}
