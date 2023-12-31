use crate::{
    predefined::INSTRUCTION_START,
    sequence::{Sequence, SequenceId, SequenceRequest, SequenceStatus},
};
use analyzer::SymbolIndex;
use bytecode::{BytecodeObject, CallablePtr, ConstantPool, Layout};
use errors::ExecutionError;
use std::{
    any::Any,
    collections::HashMap,
    fmt::Display,
    io::{stdout, Write},
};

/// An instance of the Whirlwind runtime.
#[derive(Debug)]
pub struct VM {
    queue: Vec<SequenceRequest>,
    pub instructions: Vec<u8>,
    pub layouts: Vec<Layout>,
    pub constant_pool: ConstantPool,
    pub running_sequences: usize,
    pub dispatch_table: Vec<CallablePtr>,
    /// For use in scripting contexts where main() is not necessarily defined.
    autocall: bool,
    pub exited: bool,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM {
            queue: Vec::new(),
            dispatch_table: vec![],
            instructions: vec![],
            exited: false,
            layouts: vec![],
            constant_pool: ConstantPool::new(),
            autocall: true,
            running_sequences: 0,
        };
        vm.queue.push(SequenceRequest {
            solicitor: None,
            instruction_pointer: INSTRUCTION_START,
        });
        vm
    }

    pub fn from_object(object: BytecodeObject) -> Self {
        let mut vm = VM {
            queue: Vec::new(),
            instructions: object.instructions,
            layouts: object.layouts,
            constant_pool: object.constants,
            running_sequences: 0,
            dispatch_table: object.functions,
            exited: false,
            autocall: false,
        };
        vm.queue.push(SequenceRequest {
            solicitor: None,
            instruction_pointer: INSTRUCTION_START,
        });
        vm
    }

    /// Defines the function with which to start execution.
    pub fn define_main_function(&mut self, function: CallablePtr) {
        self.dispatch_table.insert(0, function);
    }

    /// The entry execution function for a virtual machine.
    pub fn run(&mut self) -> Result<usize, ExecutionError> {
        let main_sequence = if self.autocall {
            let main_function = self
                .dispatch_table
                .get(0)
                .ok_or(ExecutionError::MainFunctionNotDefined)?;
            Sequence::new(None, SequenceId(0), main_function, 2)
        } else {
            Sequence::main(2)
        };
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
                    println!("Program exited after {:?}", time.elapsed());
                    break 'runtime;
                } else {
                    i += 1;
                }
            }
        }

        Ok(0)
    }
}
