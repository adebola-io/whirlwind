/// An instance of the Whirlwind runtime.
pub struct VM {
    sequence_pool: &'static mut [Sequence],
    sequence_queue: Vec<Sequence>,
    heap: Vec<*mut dyn std::any::Any>,
}

pub fn run(blob: Vec<u32>) {}

pub struct Sequence {
    program_counter: u128,
    stack: [u8; 2048],
    accumulator: u64,
    registers: [Register; 8],
}

pub struct Register(u64);
