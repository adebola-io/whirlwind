#![cfg(test)]

use crate::lex_text;
use crate::token::{Span, Token, TokenType};

#[test]
fn skip_whitespace_while_lexing() {
    let mut lexer = lex_text(
        "    
    
    
    \t
          
           
            
             \t\r\r\r\r",
    );
    assert!(lexer.next().is_none());
}

#[test]
fn lex_operators() {
    let lexer = lex_text(": += & && ! => := ? ! % ^ ==");
    assert_eq!(lexer.count(), 12)
}

#[test]
fn lex_block_comments() {
    // Single Line.
    let mut lexer = lex_text("/* Hello, world */");

    assert_eq!(
        lexer.next(),
        Some(Token {
            token_type: TokenType::block_comment(format!(" Hello, world ")),
            span: Span::from([1, 1, 1, 19]),
        })
    );

    // Multi Line.
    lexer = lex_text(
        "/* Hello, 
    world */",
    );

    assert_eq!(
        lexer.next(),
        Some(Token {
            token_type: TokenType::block_comment(format!(" Hello, \n    world ")),
            span: Span::from([1, 1, 2, 13]),
        })
    );

    // With asterisk and slashes.
    lexer = lex_text("/* Hello, *** world!//* / */");
    assert_eq!(
        lexer.next(),
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
        lexer.next(),
        Some(Token {
            token_type: TokenType::line_comment(format!(" This is the world premiere!!")),
            span: Span::from([1, 1, 1, 32])
        })
    );

    // doc comment.
    lexer = lex_text("/// This is the world premiere!!");

    assert_eq!(
        lexer.next(),
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
