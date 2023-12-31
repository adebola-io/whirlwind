use std::{
    fmt::{Debug, Display},
    sync::{Arc, Mutex},
};

use analyzer::SymbolIndex;

#[derive(Clone, Copy)]
pub struct Register(pub f64);

/// The smallest representation of a stored value in Whirlwind.
///
/// The smallest value created by the runtime will have
/// a size of 24 bytes, consequentially. `[sad trumpet noise.]`
///
/// Forgiveness is requested. It will be optimized later.
#[derive(Debug, Default, Clone)]
pub enum StackValue {
    HeapPointer(HeapPointer),
    Number(f64),
    Boolean(bool),
    Constant(usize),
    Function(usize),
    #[default]
    None,
}

impl Display for StackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                StackValue::HeapPointer(_) => String::from("HeapPointer"),
                StackValue::Number(num) => num.to_string(),
                _ => String::from("None"),
            }
        )
    }
}
impl From<u8> for StackValue {
    fn from(value: u8) -> Self {
        StackValue::Number(value as f64)
    }
}

#[derive(Debug)]
pub struct HeapPointer(pub Arc<Mutex<Vec<StackValue>>>);
impl Clone for HeapPointer {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
