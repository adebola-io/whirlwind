use crate::predefined::MAX_STACK_SIZE;
use analyzer::PathIndex;
use ast::Span;
use bytecode::{FunctionPtr, RegisterList, Value};
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

/// A stack for memory management in a sequence.
pub struct Stack {
    /// An inner stack of call frames.
    frames: Vec<CallFrame>,
    /// The space for stack allocation.
    space: Vec<Value>,
}
/// A call frame represents an area within the stack that is
/// peculiar to a function call. It consist of the range in the stack,
/// the return address from the call and the inner stack of expression
/// blocks.
pub struct CallFrame {
    call_address: usize,
    // The start of the frame in the stack.
    start: usize,
    return_address: usize,
    blocks: Vec<Block>,
}
/// A block corresponds semantically to a block
/// of expression code in the source.
#[derive(Debug)]
pub struct Block {
    registers: RegisterList,
}

impl CallFrame {
    /// Creates a new call frame.
    /// By default, it starts with a single block, corresponding to the
    /// block of the function itself.
    pub fn new(call_address: usize, start: usize, return_address: usize) -> Self {
        CallFrame {
            call_address,
            start,
            return_address,
            blocks: vec![Block::new()],
        }
    }
    /// Translates an address in the frame to an address in the overall stack.
    #[inline]
    pub fn translate(&self, index: usize) -> usize {
        self.start + index
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
            frames: vec![],
            space: Vec::with_capacity(MAX_STACK_SIZE),
        }
    }

    /// Creates a new call frame for a function call.
    #[inline]
    pub fn allocate_new_frame(
        &mut self,
        function: &FunctionPtr,
        return_address: usize,
    ) -> Result<(), StackError> {
        // The call address is always the return address - 1 instruction.
        let call_address = return_address - 1;
        let stack_pointer = self.space.len();
        if stack_pointer == MAX_STACK_SIZE {
            return Err(StackError::StackOverflow);
        }
        let new_frame = CallFrame::new(call_address, stack_pointer, return_address);
        self.frames.push(new_frame);
        // It may be possible to create empty frames recursively.
        if self.frames.len() > 8192 {
            return Err(StackError::StackOverflow);
        }
        Ok(())
    }

    /// Pops the current frame from the stack and returns its return address.
    #[inline]
    pub fn deallocate_current_frame(&mut self) -> Option<usize> {
        let frame = self.frames.pop()?;
        let size = self.space.len() - frame.start;
        println!("{size}, {}", self.space.len());
        let shrink = self.space.len() - size;
        while self.space.len() > shrink {
            self.space.pop();
        }
        Some(frame.return_address)
    }

    /// Write an index in the current frame with a value.
    #[inline]
    pub fn write(&mut self, index: usize, value: Value) -> Result<(), StackError> {
        let frame = self.frames.last().expect("No frames allocated on stack.");
        let index = frame.translate(index);
        if index > MAX_STACK_SIZE {
            return Err(StackError::StackOverflow);
        }
        while self.space.len() < index + 1 {
            self.space.push(Value::None);
        }
        self.space[index] = value;
        Ok(())
    }

    /// Reads the value at an address in the stack.
    #[inline]
    pub fn read(&self, index: usize) -> &Value {
        let current_frame = self.frames.last().expect(NO_FRAME_ERR);
        let index = current_frame.translate(index);
        return &self.space[index];
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

    /// Deallocates the current block in the current frame.
    ///
    /// # Panics
    /// It panics if there are no blocks in the frame or frames on the stack.
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
}

#[test]
fn stack_creation() {}
