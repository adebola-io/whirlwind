mod constant;
mod injunction;
mod opcode;
mod registers;
mod retriever;
#[cfg(test)]
mod tests;

use analyzer::{PathIndex, ScopeId, Standpoint, SymbolIndex};
pub use constant::*;
pub use injunction::*;
pub use opcode::*;
pub use registers::*;
pub use retriever::*;

#[derive(Debug)]
/// A function in the virtual machine.
pub struct Function {
    /// Computed name of the function.
    pub name: String,
    pub blocks: Vec<Block>,
}

#[derive(Debug)]
pub struct Block {
    instructions: Vec<Instruction>,
}

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

pub struct BytecodeObject {
    constants: ConstantPool,
    functions: Vec<Function>,
}

pub enum BytecodeError {
    MainIsAsync,
    MainReturns,
    MainNotFound,
    MainHasParameters,
}

/// Generates a list of instructions and data from a standpoint.
pub fn generate_from(standpoint: &Standpoint) -> Result<BytecodeObject, BytecodeError> {
    todo!()
}

/// Serializes a bytecode object into a stream of bytes.
pub fn serialize_object(object: BytecodeObject) -> Vec<u8> {
    todo!()
}
