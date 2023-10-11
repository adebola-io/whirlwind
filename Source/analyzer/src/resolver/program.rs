use ast::{ModuleAmbience, Statement};
use errors::{LexError, ParseError};
use lexer::Lexer;
use parser::parse_text;
use std::cell::RefCell;

pub struct Ast {
    pub allow_prelude: bool,
    pub ambience: RefCell<ModuleAmbience>,
    pub statements: Vec<Statement>,
    pub lexical_errors: Vec<LexError>,
    pub syntax_errors: Vec<ParseError>,
    pub line_lengths: Vec<u32>,
}

impl Ast {
    /// Returns the ast ambience.
    pub fn ambience(&self) -> &mut ModuleAmbience {
        unsafe { &mut *(self.ambience.as_ptr()) }
    }
    /// lex, parse and build an ast from text.
    pub fn from_text(input: &str) -> Ast {
        let mut parser = parse_text(input);
        let mut syntax_errors = vec![];
        let mut statements = vec![];
        loop {
            match parser.next() {
                Some(mut partial) => {
                    syntax_errors.append(&mut partial.errors);
                    if let Some(statement) = partial.value {
                        statements.push(statement);
                    }
                }
                None => break,
            }
        }

        let ambience = RefCell::new(std::mem::take(parser.module_ambience()));
        let lexical_errors = std::mem::take(parser.lexer.borrow_mut().errors());
        let allow_prelude = parser.lexer.borrow().allows_prelude();
        let line_lengths = std::mem::take(&mut parser.lexer.borrow_mut().line_lengths);

        Ast {
            allow_prelude,
            statements,
            lexical_errors,
            syntax_errors,
            ambience,
            line_lengths,
        }
    }
}
