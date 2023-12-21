use crate::{
    predefined::{INSTRUCTION_START, MAX_STACK_SLICE_SIZE},
    stack::{Block, Stack, StackError, StackValue},
    vm::{Constant, ExecutionError, Function, VM},
};
use bytecode::{AccValue, Opcode, RegisterList};
use std::{
    io::{stdout, Write},
    ops::ControlFlow,
    time::Duration,
};

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct SequenceId(pub usize);

/// A request to the virtual machine to allocate and run another sequence.
pub struct SequenceRequest {
    /// The sequence making this request.
    pub solicitor: Option<SequenceId>,
    /// The start of the new execution.
    pub instruction_pointer: usize,
}

#[derive(Clone, Copy)]
pub enum SequenceStatus {
    Idle,
    Running,
    Waiting,
    Aborted,
    Crashed(ExecutionError),
    Resolved,
    Paused,
}

/// A sequence is a threadlike execution flow within the overall program.
pub struct Sequence {
    parent: Option<SequenceId>,
    pub id: SequenceId,
    pub pc: usize,
    stack: Stack,
    time: Option<std::time::Instant>,
    blob: Option<*const [u8]>,
    pub status: SequenceStatus,
}

impl Sequence {
    pub fn new(
        parent: Option<SequenceId>,
        id: SequenceId,
        function: &Function,
        return_address: usize,
    ) -> Self {
        let mut sequence = Self {
            parent,
            id,
            pc: function.start,
            blob: None,
            time: None,
            stack: Stack::new(),
            status: SequenceStatus::Idle,
        };
        sequence
            .stack
            .allocate_new_frame(function, return_address)
            .unwrap();
        sequence
    }

    /// Advances the execution in the sequence.
    pub fn resume(&mut self, pc: usize, vm: &mut VM) {
        self.setup(pc, vm);
        'instruction_cycle: loop {
            // Yield after 10us, but only if there are other sequences to run.
            // It is a crude way of implementing concurrency. It will be improved.
            if vm.running_sequences > 1
                && self
                    .time
                    .is_some_and(|time| time.elapsed() >= Duration::from_micros(10))
            {
                self.status = SequenceStatus::Paused;
                return;
            }
            let opcode = self.next_byte().into();
            match opcode {
                Opcode::Exit => {
                    vm.exited = true;
                    break 'instruction_cycle;
                }
                Opcode::LoadIr8 => self.loadir8(),
                Opcode::LoadIr16 => self.load_ir16(),
                Opcode::LoadIr32 => self.load_ir32(),
                Opcode::LoadIr64 => self.load_ir64(),
                Opcode::LoadIframe => self.load_iframe(),
                Opcode::LoadIconstptra => self.load_iconstptra(),

                Opcode::LoadIacc8 => self.loadiacc8(),

                Opcode::Printacc8 => self.printacc8(),
                Opcode::Printacc64 => self.printacc64(),

                Opcode::Printconstptra => self.printconstptra(vm),
                Opcode::Addacc => self.addacc(),
                Opcode::Sqrtacc => self.sqrtacc(),
                Opcode::Call => {
                    let function_idx = u32::from_be_bytes(self.next_four_bytes()) as usize;
                    let function = &mut vm.functions[function_idx];
                    function.calls += 1;
                    if let Err(error) = self.stack.allocate_new_frame(&function, self.pc) {
                        self.status = match error {
                            StackError::StackOverflow => {
                                SequenceStatus::Crashed(ExecutionError::StackOverflow)
                            }
                            StackError::IllegalMemoryAccess => {
                                SequenceStatus::Crashed(ExecutionError::IllegalMemoryAccess)
                            }
                        };
                        return;
                    }
                    self.pc = function.start;
                }
                Opcode::Return => {
                    // TODO: Where to store return values?
                    let return_address = self
                        .stack
                        .deallocate_current_frame()
                        .expect("No valid return address!");
                    self.pc = return_address;
                }
                _ => unimplemented!("Runtime does not support instruction {opcode:?}!!"),
            }
        }
        self.status = SequenceStatus::Resolved;
    }

    /// Sets up the sequence to begin execution.
    #[inline]
    fn setup(&mut self, pc: usize, vm: &mut VM) {
        self.status = SequenceStatus::Running;
        self.pc = pc;
        self.blob = Some(vm.instructions.as_slice() as *const [u8]);
        self.time = Some(std::time::Instant::now());
    }

    #[inline]
    fn loadiacc8(&mut self) {
        self.registers_mut().acc8 = self.next_byte() as i8;
    }

    #[inline]
    fn loadir8(&mut self) {
        self.registers_mut().r8 = self.next_byte() as i8;
    }

    /// Executes the [`Opcode::Sqrtacc`] instruction.
    ///
    fn sqrtacc(&mut self) {
        println!("Squaring...");
        let byte = self.next_byte();
        match byte {
            0 => match self.next_byte() {
                0 => {
                    self.registers_mut().acc64 = (self.registers().acc8 as f64).sqrt();
                }
                1 => {
                    let address = u32::from_be_bytes(self.next_four_bytes()) as usize;
                    // todo: handle failure.
                    self.stack.write(address, StackValue { byte });
                }
                _ => unreachable!("Invalid instruction format."),
            },
            1 => {}
            _ => todo!(),
        }
    }

    /// Executes the [`Opcode::Addacc`] instruction.
    fn addacc(&mut self) {
        println!("Adding...");
        let byte = self.next_byte();
        match byte {
            0 => match self.next_byte() {
                0 => self.registers_mut().acc8 += self.registers_mut().r8,
                1 => {
                    let address = u32::from_be_bytes(self.next_four_bytes()) as usize;
                    self.stack.write(address, StackValue { byte });
                    // todo: handle failure.
                }
                _ => unreachable!("Invalid instruction format."),
            },
            1 => {}
            _ => todo!(),
        }
    }

    #[inline]
    fn printconstptra(&mut self, vm: &mut VM) {
        let constant = &vm.constants[self.registers().constptra];
        let lock = &mut stdout().lock();
        match constant {
            Constant::String(c) => lock.write(c.as_bytes()),
            Constant::Number(n) => lock.write(&n.to_ne_bytes()),
            Constant::Bool(b) => lock.write(if *b { b"true" } else { b"false" }),
        }
        .unwrap();
    }

    /// Prints the value in the 8-bit accumulator.
    #[inline]
    fn printacc8(&mut self) {
        write!(&mut stdout().lock(), "{}\n", self.registers().acc8).unwrap();
    }

    /// Prints the value in the 16-bit accumulator.
    #[inline]
    fn printacc16(&mut self) {
        write!(&mut stdout().lock(), "{}\n", self.registers().acc16).unwrap();
    }

    /// Prints the value in the 32-bit accumulator.
    #[inline]
    fn printacc32(&mut self) {
        write!(&mut stdout().lock(), "{}\n", self.registers().acc32).unwrap();
    }

    /// Prints the value in the 64-bit accumulator.
    #[inline]
    fn printacc64(&mut self) {
        write!(&mut stdout().lock(), "{}\n", self.registers().acc64).unwrap();
    }

    #[inline]
    fn load_iconstptra(&mut self) {
        let bytes = self.next_eight_bytes();
        let value = usize::from_be_bytes(bytes);
        self.registers_mut().constptra = value;
        println!("Loaded {:?} into constptra", self.registers().constptra);
    }

    /// Load a range of bytes into the frameptr.
    #[inline]
    fn load_iframe(&mut self) {
        let blob = self.instructions();
        let bytes = self.next_four_bytes();
        let start = u32::from_be_bytes(bytes) as usize;
        let load_type = self.next_byte();
    }

    #[inline]
    fn load_ir16(&mut self) {
        let bytes = self.next_two_bytes();
        let from_be_bytes = i16::from_be_bytes(bytes);
        self.registers_mut().r16 = from_be_bytes
    }

    #[inline]
    fn load_ir32(&mut self) {
        let bytes = self.next_four_bytes();
        self.registers_mut().r32 = f32::from_be_bytes(bytes);
    }

    #[inline]
    fn load_ir64(&mut self) {
        self.registers_mut().r64 = f64::from_be_bytes(self.next_eight_bytes());
    }

    /// Returns the list of registers for the current block.
    #[inline]
    fn registers(&self) -> &RegisterList {
        self.stack.registers().unwrap()
    }

    /// Returns the list of registers for the current block.
    #[inline]
    fn registers_mut(&mut self) -> &mut RegisterList {
        self.stack.registers_mut().unwrap()
    }

    /// Returns the stream of instructions.
    #[inline]
    fn instructions(&self) -> &[u8] {
        unsafe { (&*self.blob.unwrap()) }
    }
}

// ADVANCERS
impl Sequence {
    /// Next eight bytes.
    #[inline]
    fn next_eight_bytes(&mut self) -> [u8; 8] {
        let blob = self.instructions();
        let bytes = unsafe { *(blob[((self.pc)..(self.pc + 8))].as_ptr() as *const [u8; 8]) };
        self.pc += 8;
        return bytes;
    }
    /// Next four bytes.
    #[inline]
    fn next_four_bytes(&mut self) -> [u8; 4] {
        let blob = self.instructions();
        let bytes = unsafe { *(blob[((self.pc)..(self.pc + 4))].as_ptr() as *const [u8; 4]) };
        self.pc += 4;
        return bytes;
    }
    /// Next four bytes.
    #[inline]
    fn next_two_bytes(&mut self) -> [u8; 2] {
        let blob = self.instructions();
        let bytes = unsafe { *(blob[((self.pc)..(self.pc + 2))].as_ptr() as *const [u8; 2]) };
        self.pc += 2;
        return bytes;
    }
    /// Advances and returns the next four bytes in the stream.
    #[inline]
    fn next_byte(&mut self) -> u8 {
        let blob = self.instructions();
        let next_byte = blob[self.pc];
        self.pc += 1;
        return next_byte;
    }
}
