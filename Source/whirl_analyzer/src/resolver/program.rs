use std::cell::RefCell;
use whirl_ast::{ModuleAmbience, Statement};
use whirl_errors::{LexError, ParseError};
use whirl_lexer::Lexer;
use whirl_parser::parse_text;

pub struct Program {
    pub allow_prelude: bool,
    pub ambience: RefCell<ModuleAmbience>,
    pub statements: Vec<Statement>,
    pub lexical_errors: Vec<LexError>,
    pub syntax_errors: Vec<ParseError>,
    pub line_lengths: Vec<u32>,
}

impl Program {
    /// Returns the module ambience.
    pub fn ambience(&self) -> &mut ModuleAmbience {
        unsafe { &mut *(self.ambience.as_ptr()) }
    }
    /// lex, parse and build an inference machine from text.
    pub fn from_text(input: &str) -> Program {
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

        Program {
            allow_prelude,
            statements,
            lexical_errors,
            syntax_errors,
            ambience,
            line_lengths,
        }
    }
}
