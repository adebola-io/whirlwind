use crate::{
    predefined::INSTRUCTION_START,
    sequence::{Sequence, SequenceRequest, SequenceStatus},
};
use analyzer::SymbolIndex;
use std::{
    alloc::Layout,
    any::Any,
    collections::HashMap,
    fmt::Display,
    io::{stdout, Write},
};

pub struct SizeRegistry {
    map: HashMap<SymbolIndex, Layout>,
}
impl SizeRegistry {
    pub fn empty() -> SizeRegistry {
        Self {
            map: HashMap::new(),
        }
    }
}

/// An instance of the Whirlwind runtime.
pub struct VM {
    queue: Vec<SequenceRequest>,
    pub size_registry: SizeRegistry,
    pub constants: Vec<Constant>,
    pub running_sequences: usize,
    pub exited: bool,
}

pub enum Constant {
    String(String),
    Number(f64),
    Bool(bool),
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::String(s) => write!(f, "{s}"),
            Constant::Number(n) => write!(f, "{n}"),
            Constant::Bool(b) => write!(f, "{b}"),
        }
    }
}

impl VM {
    pub fn new(size_registry: SizeRegistry, constants: Vec<Constant>) -> Self {
        let mut vm = VM {
            queue: Vec::new(),
            exited: false,
            size_registry,
            constants,
            running_sequences: 0,
        };
        vm.queue.push(SequenceRequest {
            solicitor: None,
            instruction_pointer: INSTRUCTION_START,
        });
        vm
    }

    /// The entry execution function for a virtual machine.
    pub fn run(&mut self, blob: &[u8]) {
        let mut sequence_pool = vec![Sequence::main()];
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
                            sequence.step(blob, request.instruction_pointer, self);
                        }
                    }
                    SequenceStatus::Paused => sequence.step(blob, sequence.pc, self),
                    SequenceStatus::Running => {}
                    SequenceStatus::Waiting => {}
                    SequenceStatus::Aborted => {
                        //                 sequence.clear();
                        //                 self.running_sequences -= 1;
                    }
                    SequenceStatus::Crashed => {
                        //                 self.running_sequences -= 1;
                        //                 if sequence.id == SequenceId(0) {
                        //                     // todo: gather stack trace.
                        //                     break 'runtime;
                        //                 }
                        //                 sequence.clear();
                    }
                    SequenceStatus::Resolved => {
                        //                 self.running_sequences -= 1;
                        //                 sequence.clear();
                    }
                }
                if self.exited {
                    break 'runtime;
                } else {
                    i += 1;
                }
            }
        }

        utils::success("Program exited successfully.")
    }
}
