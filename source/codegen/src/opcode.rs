use analyzer::{EvaluatedType, SemanticSymbolKind, SymbolIndex, SymbolLibrary};

pub enum WasmSectionId {
    TypeSectionId = 0x01,
    ImportSectionId = 0x02,
    FunctionSectionId = 0x03,
    TableSectionId = 0x04,
    MemorySectionId = 0x05,
    GlobalSectionId = 0x06,
    ExportSectionId = 0x07,
    StartSectionId = 0x08,
    ElementSectionId = 0x09,
    CodeSectionId = 0x10,
    DataSectionId = 0x11,
    DataCountSectionId = 0x12,
}

impl Into<u8> for WasmSectionId {
    fn into(self) -> u8 {
        self as u8
    }
}

pub enum WasmType {
    Function = 0x60,

    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

impl Into<u8> for WasmType {
    fn into(self) -> u8 {
        self as u8
    }
}

impl WasmType {
    pub fn from_parameter(parameter: SymbolIndex, symbollib: &SymbolLibrary) -> Self {
        let parameter_type = symbollib
            .get(parameter)
            .map(|symbol| match &symbol.kind {
                SemanticSymbolKind::Parameter { inferred_type, .. } => Some(inferred_type),
                _ => None,
            })
            .flatten();

        match parameter_type {
            Some(evaled_typed) => WasmType::from_evaluated_type(evaled_typed, symbollib),
            _ => todo!(),
        }
    }

    pub fn from_evaluated_type(evaluated_type: &EvaluatedType, symbollib: &SymbolLibrary) -> Self {
        match evaluated_type {
            EvaluatedType::ModelInstance { model, .. } => {
                let index = *model;
                if symbollib
                    .i32
                    .is_some_and(|symbol_index| index == symbol_index)
                {
                    WasmType::I32
                } else if symbollib
                    .i64
                    .is_some_and(|symbol_index| index == symbol_index)
                {
                    WasmType::I64
                } else if symbollib
                    .f32
                    .is_some_and(|symbol_index| index == symbol_index)
                {
                    WasmType::F32
                } else if symbollib
                    .f64
                    .is_some_and(|symbol_index| index == symbol_index)
                {
                    WasmType::F64
                } else {
                    WasmType::I32
                }
            }
            _ => todo!(),
        }
    }
}
