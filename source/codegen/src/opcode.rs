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
