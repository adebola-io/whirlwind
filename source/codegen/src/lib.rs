use analyzer::{SemanticSymbolKind, Standpoint};
use errors::BytecodeError;

pub fn generate_wasm_from_whirlwind_standpoint(
    standpoint: &Standpoint,
) -> Result<Vec<u8>, BytecodeError> {
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
    Ok(vec![])
}

pub struct WasmBytecode {}
