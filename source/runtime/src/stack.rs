use crate::{predefined::MAX_STACK_SLICE_SIZE, vm::Function};
use analyzer::PathIndex;
use ast::Span;
use bytecode::RegisterList;
use std::{
    ops::Range,
    panic::Location,
    sync::{Arc, Mutex},
};

#[derive(Debug)]
pub enum StackError {
    StackOverflow,
    IllegalMemoryAccess,
}
pub const NO_FRAME_ERR: &'static str = "No frames allocated on stack.";
pub const NO_BLOCK_ERR: &'static str = "No blocks allocated in frame";

/// A value stored on the stack.
/// It can either be a pointer to a heap value or
/// a stream of bytes stored inline on the stack.
#[derive(Clone, Copy)]
pub union StackValue {
    pub heap_pointer: *mut u8,
    pub byte: u8,
}
/// A stack for memory management in a sequence.
pub struct Stack {
    /// A pointer to the top of the stack.
    pointer: usize,
    /// An inner stack of call frames.
    frames: Vec<CallFrame>,
    /// The initial space for stack allocation.
    slice: [StackValue; MAX_STACK_SLICE_SIZE],
    /// Flag indicating whether the slice has been filled.
    filled: bool,
    /// Additional space for the stack if the slice is filled.
    adjunct: Option<Vec<StackValue>>,
}
/// A call frame represents an area within the stack that is
/// peculiar to a function call. It consist of the range in the stack,
/// the return address from the call and the inner stack of expression
/// blocks.
pub struct CallFrame {
    call_address: usize,
    range: Range<usize>,
    return_address: usize,
    blocks: Vec<Block>,
}
/// A block corresponds semantically to a block
/// of expression code in the source.
#[derive(Debug)]
pub struct Block {
    registers: RegisterList,
}

impl Default for StackValue {
    fn default() -> Self {
        StackValue { byte: 0 }
    }
}

impl CallFrame {
    /// Creates a new call frame.
    /// By default, it starts with a single block, corresponding to the
    /// block of the function itself.
    pub fn new(call_address: usize, range: Range<usize>, return_address: usize) -> Self {
        CallFrame {
            call_address,
            range,
            return_address,
            blocks: vec![Block::new()],
        }
    }
    /// Translates an address in the frame to an address in the overall stack.
    #[inline]
    pub fn translate(&self, index: usize) -> usize {
        self.range.start + index
    }
}
impl Block {
    pub fn new() -> Self {
        Self {
            registers: RegisterList::new(),
        }
    }
}
impl Stack {
    /// Returns a new stack.
    pub fn new() -> Self {
        Self {
            pointer: 0,
            frames: vec![],
            slice: [StackValue { byte: 0 }; MAX_STACK_SLICE_SIZE],
            filled: false,
            adjunct: None,
        }
    }

    /// Creates a new call frame for a function call.
    #[inline]
    pub fn allocate_new_frame(
        &mut self,
        function: &Function,
        return_address: usize,
    ) -> Result<(), StackError> {
        let size = function.frame_size;
        // The call address is always the return address - 1 instruction.
        let call_address = return_address - 1;
        // Allocate on the slice
        if self.pointer + size > MAX_STACK_SLICE_SIZE {
            return Err(StackError::StackOverflow);
        }
        let range = self.pointer..(self.pointer + size);
        let new_frame = CallFrame::new(call_address, range, return_address);
        self.frames.push(new_frame);
        self.pointer += size;
        Ok(())
    }

    /// Pops the current frame from the stack and returns its return address.
    #[inline]
    pub fn deallocate_current_frame(&mut self) -> Option<usize> {
        let frame = self.frames.pop()?;
        let size = frame.range.end - frame.range.start;
        self.pointer -= size;
        Some(frame.return_address)
    }

    /// Write an index in the current frame with a value.
    #[inline]
    pub fn write(&mut self, index: usize, value: StackValue) -> Result<(), StackError> {
        let frame = self.frames.last().expect("No frames allocated on stack.");
        let index = frame.translate(index);
        if index > frame.range.end {
            return Err(StackError::IllegalMemoryAccess);
        }
        self.slice[index] = value;
        Ok(())
    }

    /// Writes a range in the current frame with a set of values.
    #[inline]
    pub fn write_from<const T: usize>(
        &mut self,
        index: usize,
        values: [StackValue; T],
    ) -> Result<(), StackError> {
        let frame = self.frames.last().expect("No frames allocated on stack.");
        let mut index = frame.translate(index);
        let end = index + values.len();
        if end > frame.range.end {
            return Err(StackError::IllegalMemoryAccess);
        }
        values.into_iter().for_each(|value| {
            self.slice[index] = value;
            index += 1;
        });
        Ok(())
    }

    /// Reads the value at an address in the stack.
    #[inline]
    pub fn read(&self, index: usize) -> StackValue {
        let current_frame = self.frames.last().expect(NO_FRAME_ERR);
        let index = current_frame.translate(index);
        return self.slice[index];
    }

    /// Creates a new block in the current frame.
    ///
    /// # Panics
    /// It panics if there are no frames in the stack.
    #[inline]
    pub fn allocate_block(&mut self) {
        let current_frame = self.frames.last_mut().expect(NO_FRAME_ERR);
        current_frame.blocks.push(Block::new());
    }

    #[inline]
    pub fn deallocate_block(&mut self) {
        let current_frame = self.frames.last_mut().expect(NO_FRAME_ERR);
        current_frame.blocks.pop().expect(NO_BLOCK_ERR);
    }

    /// Get the registers of the current block.
    #[inline]
    pub fn registers(&self) -> Option<&RegisterList> {
        Some(&self.frames.last()?.blocks.last()?.registers)
    }

    /// Get the registers of the current block.
    #[inline]
    pub fn registers_mut(&mut self) -> Option<&mut RegisterList> {
        Some(&mut self.frames.last_mut()?.blocks.last_mut()?.registers)
    }

    /// Reads a range of values in the stack.
    #[inline]
    pub fn read_from(&self, index: usize, length: usize) -> &[StackValue] {
        let current_frame = self.frames.last().expect("No frames allocated on stack");
        let index = current_frame.translate(index);
        return &self.slice[index..(index + length)];
    }
}

#[test]
fn stack_creation() {}
