use crate::{
    predefined::INSTRUCTION_START,
    sequence::{Sequence, SequenceId, SequenceRequest, SequenceStatus},
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
    pub instructions: Vec<u8>,
    pub size_registry: SizeRegistry,
    pub constants: Vec<Constant>,
    pub running_sequences: usize,
    pub functions: Vec<Function>,
    pub exited: bool,
}

#[derive(Debug)]
/// A function in the virtual machine.
pub struct Function {
    /// Computed name of the function.
    pub name: String,
    /// The instruction address of the first instruction in the function.
    pub start: usize,
    /// The size of the call frame.
    pub frame_size: usize,
    /// The number of calls made to this function for debug purposes.
    pub calls: usize,
}
impl Function {
    pub fn main() -> Function {
        Self {
            name: String::from("Main"),
            start: 1,
            frame_size: 1024,
            calls: 0,
        }
    }
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

impl From<String> for Constant {
    fn from(value: String) -> Self {
        Constant::String(value)
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
            functions: vec![],
            instructions: vec![],
            exited: false,
            size_registry: SizeRegistry::empty(),
            constants: vec![],
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
        self.functions.insert(0, function);
    }

    /// Adds a new constant and return its index.
    // TODO: Interning.
    pub fn add_constant<T: Into<Constant>>(&mut self, constant: T) -> usize {
        let constant = constant.into();
        let index = self.constants.len();
        self.constants.push(constant);
        return index;
    }

    /// The entry execution function for a virtual machine.
    pub fn run(&mut self) -> Result<usize, ExecutionError> {
        let main_function = self
            .functions
            .get(0)
            .ok_or(ExecutionError::MainFunctionNotDefined)?;
        let main_sequence = Sequence::new(None, SequenceId(0), main_function, 2);
        let mut sequence_pool = vec![main_sequence];
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
                    utils::success("Program exited successfully.");
                    break 'runtime;
                } else {
                    i += 1;
                }
            }
        }

        Ok(0)
    }
}
