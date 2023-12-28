mod opcode;

use crate::RegisterList;
use analyzer::{PathIndex, ScopeId, SymbolIndex};
pub use opcode::{Opcode, PAD};

#[derive(Debug)]
pub struct Instruction {
    opcode: Opcode,
    data: Data,
}

#[derive(Debug)]
pub enum Data {
    None,
    ImmediateNumericValue(Vec<u8>),
    StackFrameAddress([u8; 4]),
    ConstantIndex([u8; 8]),
    FunctionIndex(SymbolIndex),
    BlockScopeId((PathIndex, ScopeId)),
}

#[derive(Debug)]
pub struct Block {
    registers: RegisterList,
    instructions: Vec<Instruction>,
}

impl CallablePtr {
    pub fn main() -> Self {
        Self {
            name: String::from("main"),
            param_count: 0,
            start: 1,
            calls: 0,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CallablePtr {
    /// Name of the callable. For methods it is `Model.methodName`.
    pub name: String,
    /// The number of parameters in the function/method call.
    pub param_count: usize,
    /// The index of the first byte or instruction.
    pub start: usize,
    /// The number of times the function/method has been called.
    pub calls: usize,
}
