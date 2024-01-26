use std::cell::RefCell;
use std::rc::Rc;

use analyzer::SymbolIndex;

#[derive(Clone)]
pub enum Value {
    // Primitives.
    Bool(bool),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),

    /// A reference to another value on the stack.
    StackRef(usize),
    /// A layout of a model instance.
    Instance(Rc<RefCell<ModelInstance>>),
    /// An empty value.
    Void,
}

pub struct ModelInstance {
    pub parent_model: SymbolIndex,
    pub properties: Vec<Value>,
}
