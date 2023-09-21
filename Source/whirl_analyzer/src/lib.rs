mod module;
mod primitive;
mod typechecker;

use primitive::Primitives;
pub use typechecker::*;

pub use module::Module;

/// Parses and typechecks text input.
pub fn analyze_text(input: &str) -> TypeInferrer {
    TypeInferrer::from_text(input, Primitives::create())
}
