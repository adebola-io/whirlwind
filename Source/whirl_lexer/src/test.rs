#![cfg(test)]

use whirl_ast::Span;

use crate::lex_text;
use crate::token::{Token, TokenType};

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
            _type: TokenType::block_comment(format!(" Hello, world ")),
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
            _type: TokenType::block_comment(format!(" Hello, \n    world ")),
            span: Span::from([1, 1, 2, 13]),
        })
    );

    // With asterisk and slashes.
    lexer = lex_text("/* Hello, *** world!//* / */");
    assert_eq!(
        lexer.next(),
        Some(Token {
            _type: TokenType::block_comment(format!(" Hello, *** world!//* / ")),
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
            _type: TokenType::line_comment(format!(" This is the world premiere!!")),
            span: Span::from([1, 1, 1, 32])
        })
    );

    // doc comment.
    lexer = lex_text("/// This is the world premiere!!");

    assert_eq!(
        lexer.next(),
        Some(Token {
            _type: TokenType::doc_comment(format!(" This is the world premiere!!")),
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

#[test]
fn lex_identifiers_and_keywords() {
    // Simple
    let mut lexer = lex_text("name");
    assert_eq!(
        lexer.next(),
        Some(Token {
            _type: TokenType::Ident(format!("name")),
            span: Span::from([1, 1, 1, 4])
        })
    );

    // With keyword
    lexer = lex_text("public function Add");
    assert_eq!(
        lexer.collect::<Vec<Token>>(),
        vec![
            Token {
                _type: TokenType::Keyword(crate::token::Keyword::Public),
                span: Span::from([1, 1, 1, 6])
            },
            Token {
                _type: TokenType::Keyword(crate::token::Keyword::Function),
                span: Span::from([1, 8, 1, 15])
            },
            Token {
                _type: TokenType::Ident(format!("Add")),
                span: Span::from([1, 17, 1, 19])
            }
        ],
    );

    // With keyword substring
    lexer = lex_text("publicised_forerunner");
    assert_eq!(
        lexer.next(),
        Some(Token {
            _type: TokenType::Ident(format!("publicised_forerunner")),
            span: Span::from([1, 1, 1, 21])
        })
    );

    // In expression
    lexer = lex_text("name + name");
    assert_eq!(
        lexer.collect::<Vec<Token>>(),
        vec![
            Token {
                _type: TokenType::Ident(format!("name")),
                span: Span::from([1, 1, 1, 4])
            },
            Token {
                _type: TokenType::Operator(crate::token::Operator::Plus),
                span: Span::from([1, 6, 1, 7])
            },
            Token {
                _type: TokenType::Ident(format!("name")),
                span: Span::from([1, 8, 1, 11])
            }
        ],
    )
}

#[test]
fn lex_brackets() {
    let lexer = lex_text("()[]{}");
    assert_eq!(lexer.count(), 6)
}

#[test]
fn lex_strings() {
    // Simple double quoted string.
    let mut lexer = lex_text("\"Hello, world!\"");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::String(format!("Hello, world!")),
            span: Span::from([1, 1, 1, 16])
        }
    );

    // Simple single quoted string.
    lexer = lex_text("'Hello, world!'");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::String(format!("Hello, world!")),
            span: Span::from([1, 1, 1, 16])
        }
    );

    // String with escapes.
    lexer = lex_text("\"He said, \\\"Hello, how do you do?\\\"\"");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::String(format!("He said, \"Hello, how do you do?\"")),
            span: Span::from([1, 1, 1, 37])
        }
    );
}