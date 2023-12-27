use crate::{Block, ConstantPool, Function, Instruction, RegisterList};
use analyzer::{
    PathIndex, ScopeId, SemanticSymbolKind, Standpoint, SymbolIndex, TypedFunctionDeclaration,
};
use errors::BytecodeError;

pub struct BytecodeObject {
    constants: ConstantPool,
    functions: Vec<Function>,
    blocks: Vec<Block>,
}

pub struct BytecodeGenerator<'a> {
    standpoint: &'a Standpoint,
    main: &'a TypedFunctionDeclaration,
    final_object: BytecodeObject,
}

impl<'a> BytecodeGenerator<'a> {
    pub fn from(main: &'a TypedFunctionDeclaration, standpoint: &'a Standpoint) -> Self {
        Self {
            main,
            standpoint,
            final_object: BytecodeObject {
                constants: ConstantPool::new(),
                blocks: vec![],
                functions: todo!(),
            },
        }
    }

    pub fn generate(self) -> Result<BytecodeObject, BytecodeError> {
        Ok(self.final_object)
    }
}
