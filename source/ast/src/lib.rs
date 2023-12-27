mod ambience;
mod expression;
mod scope;
mod signature;
mod span;
mod statement;

mod token;
mod types;
mod visitor;

use std::{
    fs::File,
    io::{Error, Read, Seek, SeekFrom},
};

pub use ambience::*;
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

/// Returns a string containing the documentation comments from a particular range in a file.
pub fn get_documentation_at(
    path: &std::path::Path,
    line_lengths: &[u32],
    docs_span: Span,
) -> Result<String, Error> {
    let mut file = File::open(path)?;
    let range = Span::to_range(docs_span, line_lengths);

    file.seek(SeekFrom::Start(range.start as u64))?;
    let mut buffer = vec![0; range.end - range.start];

    file.read_exact(&mut buffer)?;

    let raw_str = String::from_utf8(buffer).unwrap_or_else(|_| String::new());
    let doc_str = raw_str
        .lines()
        .map(|line| {
            let trim = line.trim();
            if trim.ends_with("///") {
                return String::from("\n\n");
            } else {
                return String::from(trim.split_at(3).1) + "\n";
            }
        })
        .collect::<String>();
    Ok(doc_str)
}
