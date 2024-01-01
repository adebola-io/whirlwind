use std::path::PathBuf;

use crate::{generate_from, BytecodeObject};
use analyzer::{Module, Standpoint, CORE_LIBRARY_PATH};
use errors::BytecodeError;

/// Generates bytecode directly from text.
/// Only for testing purposes.
pub fn bytecode_from_text(text: &str) -> Result<BytecodeObject, BytecodeError> {
    let corelib_path = Some(PathBuf::from(CORE_LIBRARY_PATH));
    let mut standpoint = Standpoint::new(true, corelib_path);
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://main.wrl"));
    standpoint.entry_module = standpoint.add_module(module).unwrap();
    standpoint.validate();
    if standpoint.diagnostics.len() > 0 {
        panic!(
            "Non-zero diagnostics: \n\n
        {:#?}
        ",
            standpoint.diagnostics
        )
    }
    return generate_from(&standpoint);
}
