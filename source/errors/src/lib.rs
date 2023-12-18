mod context_error;
mod import_error;
mod lex_error;
mod parse_error;
mod stringify;
mod type_error;

pub use context_error::*;
pub use import_error::*;
pub use lex_error::*;
pub use parse_error::*;
pub use stringify::*;
pub use type_error::*;

/// An error within a module.
#[derive(Debug)]
pub enum ModuleError<'a> {
    ParserError(&'a ParseError),
    LexerError(&'a LexError),
    TypeError(&'a TypeError),
    ImportError(&'a ImportError),
}

pub fn implicit_loop_return(rettype: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ImplicitLoopReturn { rettype },
        span,
    }
}

pub fn illegal_iterator(illegal_type: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::Illegalterator { illegal_type },
        span,
    }
}

pub fn using_this_before_construction(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UsingThisBeforeConstructor,
        span,
    }
}
