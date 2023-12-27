use std::{collections::HashMap, fmt::Display};

/// A smart list of unique constant values.
pub struct ConstantPool {
    pub list: Vec<Constant>,
    /// The router redirects LiteralIndex requests to the new unique values.
    pub router: HashMap<usize, usize>,
}

impl ConstantPool {
    /// Creates a new constant list.
    pub fn new() -> Self {
        Self {
            list: vec![],
            router: HashMap::new(),
        }
    }
    /// Adds a new constant and return its index.
    // TODO: Interning.
    pub fn add<T: Into<Constant>>(&mut self, constant: T) -> usize {
        let constant = constant.into();
        let index = self.list.len();
        self.list.push(constant);
        return index;
    }
}

pub enum Constant {
    String(String),
    Number(f64),
    Bool(bool),
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::String(s) => write!(f, "{s}"),
            Constant::Number(n) => write!(f, "{n}"),
            Constant::Bool(b) => write!(f, "{b}"),
        }
    }
}

impl From<String> for Constant {
    fn from(value: String) -> Self {
        Constant::String(value)
    }
}
