mod inference;
mod test;
pub mod type_utils;

pub use inference::TypeInferrer;
pub use whirl_errors::{TypeError, TypeErrorType};
