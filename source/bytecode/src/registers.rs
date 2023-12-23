use std::{
    fmt::{Debug, Display},
    sync::{Arc, Mutex},
};

#[derive(Clone, Copy)]
pub struct Register(pub f64);

/// The smallest representation of a stored value in Whirlwind.
///
/// The smallest value created by the runtime will have
/// a size of 24 bytes, consequentially. `[sad trumpet noise.]`
///
/// Forgiveness is requested. It will be optimized later.
#[derive(Debug, Default, Clone)]
pub enum Value {
    HeapPointer(HeapPointer),
    Number(f64),
    Boolean(bool),
    Constant(usize),
    Function(usize),
    #[default]
    None,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::HeapPointer(_) => String::from("HeapPointer"),
                Value::Number(num) => num.to_string(),
                _ => String::from("None"),
            }
        )
    }
}
impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value::Number(value as f64)
    }
}

#[derive(Debug)]
pub struct HeapPointer(pub Arc<Mutex<Vec<Value>>>);
impl Clone for HeapPointer {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[derive(Default, Debug)]
pub struct RegisterList {
    // 8-bit registers.
    pub r8: i8,
    pub acc8: i8,

    // 16-bit registers.
    pub r16: i16,
    pub acc16: i16,

    // 32-bit registers.
    pub r32: f32,
    pub acc32: f32,

    // 64-bit registers.
    pub r64: f64,
    pub acc64: f64,

    /// Boolean registers.
    pub boola: bool,
    pub boolb: bool,

    // Constant pointer registers.
    pub constptra: usize,
    pub constptrb: usize,

    // value registsers.
    pub vala: Option<Value>,
    pub valb: Option<Value>,

    /// The value of this.
    pub this: Option<Value>,

    // The return value of a function call.
    pub ret: Option<Value>,
}

impl RegisterList {
    pub fn new() -> Self {
        Default::default()
    }
}
