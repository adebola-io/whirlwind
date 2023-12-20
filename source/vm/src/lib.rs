#![allow(unused)]
mod sequence;

use sequence::{Sequence, SequenceRequest, SequenceStatus};
use std::{
    any::Any,
    fmt::Display,
    io::{stdout, Write},
};

pub struct SizeRegistry {}

/// An instance of the Whirlwind runtime.
pub struct VM {
    queue: Vec<SequenceRequest>,
    heap: Vec<*mut dyn Any>,
    size_registry: SizeRegistry,
    constants: Vec<Constant>,
    running_sequences: usize,
    exited: bool,
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
            heap: vec![],
            exited: false,
            size_registry,
            constants,
            running_sequences: 0,
        };
        vm.queue.push(SequenceRequest {
            solicitor: None,
            instruction_pointer: 0,
        });
        vm
    }

    /// The entry execution function for a virtual machine.
    pub unsafe fn run(&mut self, blob: &[u8]) {
        let mut sequence_pool = vec![Sequence::main()];
        let time = std::time::Instant::now();

        'runtime: loop {
            let mut i = 0;
            while i < sequence_pool.len() {
                //         // let raw = &mut sequence_pool as *mut [Sequence];
                let sequence = sequence_pool.get_unchecked_mut(i);
                match sequence.status {
                    SequenceStatus::Idle => {
                        if self.queue.len() != 0 {
                            let request = self.queue.pop().unwrap();
                            self.running_sequences += 1;
                            sequence.step(blob, request.instruction_pointer, self);
                        }
                    }
                    SequenceStatus::Paused => sequence.step(blob, sequence.ip, self),
                    SequenceStatus::Running => {}
                    SequenceStatus::Waiting => {
                        //                 todo!()
                    }
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

        println!("Program exited in {:?}", time.elapsed());
    }
}

#[test]
fn test_runtime_exit() {
    let instructions = [0];
    let mut vm = VM::new(SizeRegistry {}, vec![]);
    unsafe { vm.run(&instructions) };
}

#[test]
fn test_runtime_hello_world() {
    let time = std::time::Instant::now();
    let lock = &mut stdout().lock();
    lock.write("hello, world!".as_bytes()).unwrap();
    println!("Completed in {:?}", time.elapsed());
    // let v = 0usize.to_be_bytes();
    // let instructions = [
    //     0x11, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7],
    //     0, // Print "Hello, world." and exit.
    // ];
    // let mut vm = VM::new(
    //     SizeRegistry {},
    //     vec![
    //         Constant::String(String::from("Hello, world.")),
    //         Constant::String(String::from("\n")),
    //     ],
    // );
    // unsafe { vm.run(&instructions) };
}

#[test]
fn test_runtime_add_numbers() {
    // let instructions = [1, 4, 0];
    // Print 4 + 5;
    let mut vm = VM::new(SizeRegistry {}, vec![]);
    // unsafe { vm.run(&instructions) };
}
