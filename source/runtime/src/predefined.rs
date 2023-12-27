/// Maximum value for the part of a stack that is allocated on (Rust's) heap.
pub const MAX_STACK_SIZE: usize = 2097152; //2mb
/// The start of the instruction stream in the bytecode.
pub const INSTRUCTION_START: usize = 1; // 0 is supposed to be the global return address.
