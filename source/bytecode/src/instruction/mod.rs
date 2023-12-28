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
