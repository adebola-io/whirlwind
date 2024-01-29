pub const MAGIC_HEADER: [u8; 4] = [0x00, 0x61, 0x73, 0x6d]; // \0asm.

pub enum WasmOpcode {
    I32Add,
    I64Add,
    F32Add,
}
