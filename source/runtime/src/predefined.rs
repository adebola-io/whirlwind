/// Maximum value for the part of a stack that is allocated in an array slice.
pub const MAX_STACK_SLICE_SIZE: usize = 16384; // 16kb
/// Maximum value for the part of a stack that is allocated on (Rust's) heap.
pub const MAX_STACK_HEAP_SIZE: usize = 2097152; //2mb
/// The start of the instruction stream in the bytecode.
pub const INSTRUCTION_START: usize = 1; // 0 is supposed to be the global return address.
/// Padding for the bytecode.
pub const PAD: u8 = 0;