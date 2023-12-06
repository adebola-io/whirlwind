mod ambience;
mod context;
mod expression;
mod scope;
mod signature;
mod span;
mod statement;

mod token;
mod types;
mod visitor;

pub use ambience::*;
pub use context::*;
pub use expression::*;
pub use scope::*;
pub use signature::*;
pub use span::*;
pub use statement::*;
pub use token::*;
pub use types::*;
pub use visitor::*;

/// Exits a function and returns an option value if it is Some.
/// Basically the direct opposite of `impl Try for Option`.
#[macro_export]
macro_rules! maybe {
    ($exp: expr) => {
        if let Some(exp) = $exp {
            return Some(exp);
        }
    };
}

/// Exits a visition function if a position is not contained within a span.
#[macro_export]
macro_rules! within {
    ($span: expr, $self: expr) => {
        if !$span.contains($self.pos) {
            return None;
        }
    };
}

/// Helper macro that unwraps an Option, or continues in a for loop.
#[macro_export]
macro_rules! unwrap_or_continue {
    ($expr: expr) => {{
        match $expr {
            Some(value) => value,
            None => continue,
        }
    }};
}
