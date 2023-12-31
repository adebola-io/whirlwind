#![cfg(test)]

use ast::{Keyword, Number, Operator, Span, Token, TokenType};

use crate::lex_text;

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
            span: Span::from([1, 1, 1, 5])
        })
    );

    // With keyword
    lexer = lex_text("public function Add");
    assert_eq!(
        lexer.collect::<Vec<Token>>(),
        vec![
            Token {
                _type: TokenType::Keyword(Keyword::Public),
                span: Span::from([1, 1, 1, 7])
            },
            Token {
                _type: TokenType::Keyword(Keyword::Function),
                span: Span::from([1, 8, 1, 16])
            },
            Token {
                _type: TokenType::Ident(format!("Add")),
                span: Span::from([1, 17, 1, 20])
            }
        ],
    );

    // With keyword substring
    lexer = lex_text("publicised_forerunner");
    assert_eq!(
        lexer.next(),
        Some(Token {
            _type: TokenType::Ident(format!("publicised_forerunner")),
            span: Span::from([1, 1, 1, 22])
        })
    );

    // In expression
    lexer = lex_text("name + name");
    assert_eq!(
        lexer.collect::<Vec<Token>>(),
        vec![
            Token {
                _type: TokenType::Ident(format!("name")),
                span: Span::from([1, 1, 1, 5])
            },
            Token {
                _type: TokenType::Operator(Operator::Plus),
                span: Span::from([1, 6, 1, 7])
            },
            Token {
                _type: TokenType::Ident(format!("name")),
                span: Span::from([1, 8, 1, 12])
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

#[test]
fn lex_numbers() {
    // Decimal numbers.
    let mut lexer = lex_text("23456");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Number(Number::Decimal(format!("23456"))),
            span: Span::from([1, 1, 1, 6])
        }
    );
    // Binary numbers.
    lexer = lex_text("0b101010");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Number(Number::Binary(format!("101010"))),
            span: Span::from([1, 1, 1, 9])
        }
    );
    // Octal numbers.
    lexer = lex_text("0o163524");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Number(Number::Octal(format!("163524"))),
            span: Span::from([1, 1, 1, 9])
        }
    );
    // Hexadecimal numbers.
    lexer = lex_text("0x1283A83");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Number(Number::Hexadecimal(format!("1283A83"))),
            span: Span::from([1, 1, 1, 10])
        }
    );

    // Immediately before range.
    lexer = lex_text("1..3 + 4.5..4");
    assert_eq!(
        (lexer.nth(0).unwrap(), lexer.nth(3).unwrap()),
        (
            Token {
                _type: TokenType::Number(Number::Decimal(format!("1"))),
                span: Span::from([1, 1, 1, 2])
            },
            Token {
                _type: TokenType::Number(Number::Decimal(format!("4.5"))),
                span: Span::from([1, 8, 1, 11])
            }
        )
    );
}

#[test]
fn lex_numbers_with_decimals() {
    let mut lexer = lex_text("123.456");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Number(Number::Decimal(format!("123.456"))),
            span: Span::from([1, 1, 1, 8])
        }
    );
}

#[test]
fn lex_numbers_with_exponent() {
    // With exponents
    let mut lexer = lex_text("2e45");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Number(Number::Decimal(format!("2e45"))),
            span: Span::from([1, 1, 1, 5])
        }
    );
    // With decimals
    lexer = lex_text("20.5e45");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Number(Number::Decimal(format!("20.5e45"))),
            span: Span::from([1, 1, 1, 8])
        }
    );
    // With positive sign
    lexer = lex_text("429e+45");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Number(Number::Decimal(format!("429e+45"))),
            span: Span::from([1, 1, 1, 8])
        }
    );
    // With negative sign
    lexer = lex_text("902e-9");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Number(Number::Decimal(format!("902e-9"))),
            span: Span::from([1, 1, 1, 7])
        }
    );
}

#[test]
fn lex_booleans() {
    let mut lexer = lex_text("true false");
    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Keyword(Keyword::True),
            span: Span::from([1, 1, 1, 5])
        }
    );

    assert_eq!(
        lexer.next().unwrap(),
        Token {
            _type: TokenType::Keyword(Keyword::False),
            span: Span::from([1, 6, 1, 11])
        }
    )
}
