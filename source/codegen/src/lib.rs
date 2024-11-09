mod error_helpers;
mod opcode;

use analyzer::{
    simple_evaluate, SemanticSymbol, SemanticSymbolKind, Standpoint, SymbolLibrary,
    TypedFunctionDeclaration,
};
use error_helpers::{function_mismatch_error, function_resolve_error};
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
pub struct WasmBytecodeSection {
    chunks: Vec<Vec<u8>>,
}

#[derive(Debug, Default, Clone)]
pub struct WasmBytecode {
    pub type_section: WasmBytecodeSection,
    pub export_section: WasmBytecodeSection,
    pub functions: Vec<u8>,
    pub code_section: WasmBytecodeSection,
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

        let symbollib = &standpoint.symbol_library;
        bytecode.emit_function(main_function, symbollib);

        Ok(bytecode)
    }

    pub fn emit_function(
        &mut self,
        function: &TypedFunctionDeclaration,
        symbollib: &SymbolLibrary,
    ) {
        // Generating function type signature.
        let mut type_chunk: Vec<u8> = vec![];
        type_chunk.push(WasmType::Function.into());

        let function_symbol = match symbollib.get(function.name) {
            Some(symbol) => symbol,
            None => function_resolve_error(function),
        };

        let mut function_is_public = false;
        match &function_symbol.kind {
            SemanticSymbolKind::Function {
                is_public,
                params,
                return_type,
                ..
            } => {
                // Encode parameters.
                type_chunk.extend_from_slice(&params.len().to_le_bytes());
                for param in params {
                    type_chunk.push(WasmType::from_parameter(*param, symbollib).into());
                }
                // Encode return type.
                if let Some(return_type) = return_type {
                    let return_type_evaled = simple_evaluate(return_type, symbollib);
                    type_chunk.push(0x01);
                    type_chunk
                        .push(WasmType::from_evaluated_type(&return_type_evaled, symbollib).into());
                }
                function_is_public = *is_public;
            }
            _ => function_mismatch_error(function_symbol),
        }

        self.type_section.chunks.push(type_chunk);
        let type_chunk_index = self.type_section.chunks.len() - 1;

        // Add function type index to function section.
        self.functions
            .extend_from_slice(&type_chunk_index.to_le_bytes());

        self.emit_function_body(function, symbollib);

        if function_is_public {
            self.export_function(function_symbol, type_chunk_index);
        }
    }

    pub fn emit_function_body(
        &mut self,
        _function: &TypedFunctionDeclaration,
        _symbollib: &SymbolLibrary,
    ) {
        let mut code_chunk: Vec<u8> = vec![];
        code_chunk.push(0x00); // size of function placeholder.
        code_chunk.push(0x0b);
        self.code_section.chunks.push(code_chunk);
    }

    pub fn export_function(&mut self, function_symbol: &SemanticSymbol, type_chunk_index: usize) {
        let mut export_chunk: Vec<u8> = vec![];
        // Export name length.
        export_chunk.extend_from_slice(&function_symbol.name.len().to_le_bytes());
        // Export name.
        export_chunk.extend_from_slice(function_symbol.name.as_bytes());
        // Export kind.
        export_chunk.push(0x00); // Function
                                 // Export type index.
        export_chunk.extend_from_slice(&type_chunk_index.to_le_bytes());

        self.export_section.chunks.push(export_chunk);
    }

    pub fn to_bytes(self) -> Vec<u8> {
        let mut bytes = vec![
            0x00, 0x61, 0x73, 0x6D, // Magic
            0x01, 0x00, 0x00, 0x00, // Version
        ];

        // Appending type section.
        bytes.push(WasmSectionId::TypeSectionId.into());
        let mut section_length = 1; // starting with the section id.

        for chunk_length in self.type_section.chunks.iter().map(|chunk| chunk.len()) {
            section_length += chunk_length;
        }

        bytes.extend_from_slice(&section_length.to_le_bytes()); // section length.
        bytes.extend_from_slice(&self.type_section.chunks.len().to_le_bytes()); // number of types.

        for chunk in self.type_section.chunks {
            bytes.extend_from_slice(&chunk); // type chunks.
        }

        // Appending function section.
        bytes.push(WasmSectionId::FunctionSectionId.into());
        section_length = 1 + self.functions.len(); // starting with the section id.

        bytes.extend_from_slice(&section_length.to_le_bytes()); // section length.
        bytes.extend_from_slice(&self.functions.len().to_le_bytes()); // number of functions.
        bytes.extend_from_slice(&self.functions); // function chunks.

        // Appending export section.
        bytes.push(WasmSectionId::ExportSectionId.into());
        section_length = 1; // starting with the section id.

        for chunk_length in self.export_section.chunks.iter().map(|chunk| chunk.len()) {
            section_length += chunk_length;
        }
        bytes.extend_from_slice(&section_length.to_le_bytes()); // section length.
        bytes.extend_from_slice(&self.export_section.chunks.len().to_le_bytes()); // number of exports.
        for chunk in self.export_section.chunks {
            bytes.extend_from_slice(&chunk);
        }

        bytes
    }
}
