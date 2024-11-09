mod opcode;

use analyzer::{
    SemanticSymbolKind, Standpoint, SymbolLibrary, TypedFunctionDeclaration, TypedStmnt,
};
use errors::BytecodeError;
use opcode::{WasmSectionId, WasmType};

/// Generates a WebAssembly (WASM) binary from a Whirlwind standpoint.
///
/// This function takes a `Standpoint` object, which represents the entry point and
/// dependencies of a Whirlwind program, and generates a WASM binary that can be
/// executed by a WASM runtime.
///
/// # Errors
/// This function may return a `BytecodeError` if there are issues generating the
/// WASM binary, such as the main function not being found or not meeting the
/// requirements for a WASM entry point.
pub fn generate_wasm_from_whirlwind_standpoint(
    standpoint: &Standpoint,
) -> Result<Vec<u8>, BytecodeError> {
    WasmBytecode::from_standpoint(standpoint).map(|bytecode| bytecode.to_bytes())
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct WasmBytecodeTypeSection {
    chunks: Vec<Vec<u8>>,
}

#[derive(Debug, Default, Clone)]
pub struct WasmBytecode {
    pub type_section: WasmBytecodeTypeSection,
}

impl WasmBytecode {
    pub fn from_standpoint(standpoint: &Standpoint) -> Result<Self, BytecodeError> {
        let main_function = match standpoint.main() {
            Some(main_function) => main_function,
            None => return Err(BytecodeError::MainNotFound),
        };
        let main_function_symbol = match standpoint.symbol_library.get(main_function.name) {
            Some(symbol) => symbol,
            None => return Err(BytecodeError::MainNotResolvable),
        };
        if let SemanticSymbolKind::Function {
            is_public,
            is_async,
            params,
            generic_params,
            extern_import_source,
            return_type,
        } = &main_function_symbol.kind
        {
            if *is_async {
                return Err(BytecodeError::MainIsAsync);
            }
            if return_type.is_some() {
                return Err(BytecodeError::MainReturns);
            }
            if !is_public {
                return Err(BytecodeError::MainNotPublic);
            }
            if params.len() > 0 || generic_params.len() > 0 {
                return Err(BytecodeError::MainHasParameters);
            }
            if !extern_import_source.is_none() {
                return Err(BytecodeError::MainIsImported);
            }
        }

        let mut bytecode: WasmBytecode = Default::default();

        // ..

        Ok(bytecode)
    }

    pub fn emit_function(
        &mut self,
        function: &TypedFunctionDeclaration,
        symbollib: &SymbolLibrary,
    ) {
        let mut type_chunk: Vec<u8> = vec![];
        type_chunk.push(WasmType::Function.into());

        let function_symbol = match symbollib.get(function.name) {
            Some(symbol) => symbol,
            None => function_resolve_error(function),
        };

        match &function_symbol.kind {
            SemanticSymbolKind::Function {
                is_public,
                is_async,
                params,
                generic_params,
                extern_import_source,
                return_type,
            } => {
                if *is_async {
                    type_chunk.push(WasmType::I32.into());
                }
                if return_type.is_some() {
                    type_chunk.push(WasmType::I32.into());
                }
                if !is_public {
                    type_chunk.push(WasmType::I32.into());
                }
                for param in params {
                    type_chunk.push(WasmType::I32.into());
                }
                // TODO: Generic params

                self.type_section.chunks.push(type_chunk);
            }
            _ => function_mismatch_error(function_symbol),
        }
    }

    pub fn emit_statement(&mut self, statement: &TypedStmnt, symbollib: &SymbolLibrary) {
        todo!()
    }

    pub fn to_bytes(mut self) -> Vec<u8> {
        let mut bytes = vec![
            0x00, 0x61, 0x73, 0x6D, // Magic
            0x01, 0x00, 0x00, 0x00, // Version
        ];

        bytes
    }
}

const REPORT: &str =
    "This is a compiler bug. Please report this at https://github.com/adebola-io/whirlwind/issues.";
fn function_mismatch_error(function_symbol: &analyzer::SemanticSymbol) {
    unreachable!("A function declaration was matched to a symbol of type {:?} during bytecode generation. {REPORT}", function_symbol.kind)
}

fn function_resolve_error(function_symbol: &TypedFunctionDeclaration) -> ! {
    unreachable!("A function declaration {:?} did not resolve to a symbol during bytecode generation. {REPORT}", function_symbol)
}
