#![allow(unused)]
mod constant;
mod generator;
mod injunction;
mod instruction;
mod registers;
mod retriever;
#[cfg(test)]
mod tests;

use analyzer::{
    PathIndex, ScopeId, SemanticSymbolKind, Standpoint, SymbolIndex, TypedFunctionDeclaration,
};
pub use constant::*;
use errors::BytecodeError;
pub use generator::BytecodeGenerator;
use generator::BytecodeObject;
pub use injunction::*;
pub use instruction::*;
pub use registers::*;
pub use retriever::*;

/// Generates a list of instructions and data from a standpoint.
pub fn generate_from(standpoint: &Standpoint) -> Result<BytecodeObject, BytecodeError> {
    let main = standpoint.main();
    let main = match main {
        Some(main) => main,
        None => return Err(BytecodeError::MainNotFound),
    };
    let main_symbol = match standpoint.symbol_library.get(main.name) {
        Some(main) => main,
        None => return Err(BytecodeError::MainNotFound),
    };
    if let SemanticSymbolKind::Function {
        is_public,
        is_async,
        params,
        generic_params,
        return_type,
    } = &main_symbol.kind
    {
        if *is_async {
            return Err(BytecodeError::MainIsAsync);
        }
        if return_type.is_some() {
            return Err(BytecodeError::MainReturns);
        }
        if !params.is_empty() {
            return Err(BytecodeError::MainHasParameters);
        }
    }
    let mut generator = BytecodeGenerator::from(main, standpoint);
    let object = generator.generate()?;
    Ok(object)
}

/// Serializes a bytecode object into a stream of bytes.
pub fn serialize_object(object: BytecodeObject) -> Vec<u8> {
    vec![]
}
