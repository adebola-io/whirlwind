#![allow(unused)]
use std::borrow::Cow;

use crate::{FunctionPtr, Instruction, RegisterList};
use analyzer::{ScopeId, Standpoint, TypedFunctionDeclaration};

pub struct InstructionBlock {
    id: ScopeId,
    registers: RegisterList,
    instructions: Vec<Instruction>,
}

pub struct Program {
    blocks: Vec<InstructionBlock>,
}

pub struct BytecodeBuilder<'a> {
    standpoint: &'a Standpoint,
    functions: Vec<WrlFunction>,
    blocks: Vec<InstructionBlock>,
}

pub struct Location {}

pub struct WrlFunction {
    name: String,
    address: Location,
}

impl<'a> BytecodeBuilder<'a> {
    pub fn new(standpoint: &'a Standpoint) -> Self {
        Self {
            standpoint,
            functions: vec![],
            blocks: vec![],
        }
    }
    pub fn write(&mut self) -> Option<Program> {
        let main = self.standpoint.main()?;
        self.write_function(main);
        // Some(std::mem::take(&mut self.blocks))
        None
    }

    /// Writes a function into bytecode.
    pub fn write_function(&mut self, function: &TypedFunctionDeclaration) {
        self.create_call_frame(function.name);
        // self.load_params_to_stack();
        // self.goto(function.body.scopeid);
        // self.write_block(&function.body);
    }

    fn create_call_frame(&mut self, name: analyzer::SymbolIndex) {}

    fn allocate_or_get(&self, name: analyzer::SymbolIndex) -> crate::FunctionPtr {
        todo!()
    }
}
