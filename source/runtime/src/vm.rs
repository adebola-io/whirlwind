use crate::{
    predefined::INSTRUCTION_START,
    sequence::{Sequence, SequenceId, SequenceRequest, SequenceStatus},
};
use analyzer::SymbolIndex;
use bytecode::ConstantPool;
use std::{
    any::Any,
    collections::HashMap,
    fmt::Display,
    io::{stdout, Write},
};

pub struct Layout {
    pub size: usize,
    pub property_offsets: Vec<usize>,
}

/// An instance of the Whirlwind runtime.
pub struct VM {
    queue: Vec<SequenceRequest>,
    pub instructions: Vec<u8>,
    pub layouts: Vec<Layout>,
    pub constant_pool: ConstantPool,
    pub running_sequences: usize,
    pub vtable: Vec<Function>,
    pub exited: bool,
}

#[derive(Debug)]
/// A function in the virtual machine.
pub struct Function {
    /// Computed name of the function.
    pub name: String,
    /// The instruction address of the first instruction in the function.
    pub start: usize,
    /// The number of calls made to this function for debug purposes.
    pub calls: usize,
}
impl Function {
    pub fn main() -> Function {
        Self {
            name: String::from("Main"),
            start: 1,
            calls: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExecutionError {
    MainCrashed,
    MainFunctionNotDefined,
    StackOverflow,
    IllegalMemoryAccess,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM {
            queue: Vec::new(),
            vtable: vec![],
            instructions: vec![],
            exited: false,
            layouts: vec![],
            constant_pool: ConstantPool::new(),
            running_sequences: 0,
        };
        vm.queue.push(SequenceRequest {
            solicitor: None,
            instruction_pointer: INSTRUCTION_START,
        });
        vm
    }

    /// Defines the function with which to start execution.
    pub fn define_main_function(&mut self, function: Function) {
        self.vtable.insert(0, function);
    }

    /// The entry execution function for a virtual machine.
    pub fn run(&mut self) -> Result<usize, ExecutionError> {
        let main_function = self
            .vtable
            .get(0)
            .ok_or(ExecutionError::MainFunctionNotDefined)?;
        let main_sequence = Sequence::new(None, SequenceId(0), main_function, 2);
        let mut sequence_pool = vec![main_sequence];
        let time = std::time::Instant::now();
        'runtime: loop {
            let mut i = 0;
            while i < sequence_pool.len() {
                //         // let raw = &mut sequence_pool as *mut [Sequence];
                let sequence = &mut sequence_pool[i];
                match sequence.status {
                    SequenceStatus::Idle => {
                        if self.queue.len() != 0 {
                            let request = self.queue.pop().unwrap();
                            self.running_sequences += 1;
                            sequence.resume(request.instruction_pointer, self);
                        }
                    }
                    SequenceStatus::Paused => sequence.resume(sequence.pc, self),
                    SequenceStatus::Running => {}
                    SequenceStatus::Waiting => {}
                    SequenceStatus::Aborted => {
                        //                 sequence.clear();
                        //                 self.running_sequences -= 1;
                    }
                    SequenceStatus::Crashed(reason) => {
                        self.running_sequences -= 1;
                        if sequence.id == SequenceId(0) {
                            // todo: gather stack trace.
                            return Err(reason);
                        }
                    }
                    SequenceStatus::Resolved => {
                        //                 self.running_sequences -= 1;
                        //                 sequence.clear();
                    }
                }
                if self.exited {
                    println!("Program exited in {:?}", time.elapsed());
                    utils::terminal::success("Program exited successfully.");
                    break 'runtime;
                } else {
                    i += 1;
                }
            }
        }

        Ok(0)
    }
}
