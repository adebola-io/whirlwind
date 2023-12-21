use crate::{
    predefined::{INSTRUCTION_START, MAX_STACK_SLICE_SIZE},
    stack::{Block, Stack, StackValue},
    vm::{Constant, VM},
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
    Crashed,
    Resolved,
    Paused,
}

/// A sequence is a threadlike execution flow within the overall program.
pub struct Sequence {
    parent: Option<SequenceId>,
    pub id: SequenceId,
    pub pc: usize,
    stack: Stack,
    blob: Option<*const [u8]>,
    pub status: SequenceStatus,
}

impl Sequence {
    /// Creates the main sequence.
    pub fn main() -> Self {
        let mut main_seq = Self {
            parent: None,
            id: SequenceId(0),
            pc: INSTRUCTION_START,
            stack: Stack::new(),
            blob: None,
            status: SequenceStatus::Idle,
        };
        main_seq.stack.allocate_new_frame(1, 0).unwrap();
        main_seq
    }

    pub fn new(parent: Option<SequenceId>, id: SequenceId) -> Self {
        Self {
            parent,
            id,
            pc: 0,
            blob: None,
            stack: Stack::new(),
            status: SequenceStatus::Idle,
        }
    }

    pub fn clear(&mut self) {
        *self = Sequence::new(self.parent, self.id);
    }

    /// Advances the execution in the sequence.
    pub fn step(&mut self, blob: &[u8], pc: usize, vm: &mut VM) {
        self.status = SequenceStatus::Running;
        self.pc = pc;
        self.blob = Some(blob as *const [u8]);
        let time = std::time::Instant::now();
        loop {
            // Yield after 10microsecs.
            if time.elapsed() >= Duration::from_micros(10) && vm.running_sequences > 1 {
                self.status = SequenceStatus::Paused;
                return;
            }
            let opcode = self.next_byte().into();
            if let ControlFlow::Break(_) = self.decode(opcode, vm) {
                break;
            }
        }
        self.status = SequenceStatus::Resolved;
    }

    #[inline]
    fn decode(&mut self, opcode: Opcode, vm: &mut VM) -> ControlFlow<()> {
        match opcode {
            Opcode::Exit => {
                vm.exited = true;
                return ControlFlow::Break(());
            }
            Opcode::LoadIr8 => {
                self.registers_mut().r8 = self.next_byte() as i8;
                println!("Loaded {:?} into r8", self.registers().r8);
            }
            Opcode::LoadIr16 => self.load_ir16(),
            Opcode::LoadIr32 => self.load_ir32(),
            Opcode::LoadIr64 => self.load_ir64(),
            Opcode::LoadIframe => self.load_iframe(),
            Opcode::LoadIconstptra => self.load_iconstptra(),
            // // Opcode::LoadImToFramePtr
            // 0x05 => self.load_immediate_frameptr(),
            // // Opcode::LoadImToConstPtr
            // 0x06 => self.load_immediate_constptr(),
            Opcode::LoadIacc8 => {
                self.registers_mut().acc8 = self.next_byte() as i8;
                println!("Loaded {:?} into acc8", self.registers().acc8);
            }
            // // Opcode::LoadAcc16
            // 0x09 => self.block().acc16 = self.block().r16,
            // // Opcode::LoadAcc32
            // 0x10 => self.block().acc32 = self.block().r32,
            // // Opcode::LoadAcc64
            // 0x11 => self.block().acc64 = self.block().r64,
            Opcode::Printacc8 => self.printacc8(),
            // // Opcode::PrintAcc16
            // 0x12 => self.printacc16(),
            // // Opcode::PrintAcc32
            // 0x12 => self.printacc32(),
            // // Opcode::PrintAcc64
            // 0x10 => self.printacc64(),
            // // Opcode::PrintConstPtr
            Opcode::Printconstptra => self.printconstptra(vm),
            Opcode::Addacc => {
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
            // // Opcode::MoveAcc8To16
            // 0x12 => {
            //     self.block().acc16 = self.block().acc8 as i16;
            // }
            // // Opcode::MoveAcc8To32
            // 0x13 => {
            //     self.block().acc32 = self.block().acc8 as f32;
            // }
            // // Opcode::MoveAcc8To64
            // 0x14 => {
            //     self.block().acc64 = self.block().acc8 as f64;
            // }
            // // Opcode::MoveAcc16To32
            // 0x15 => {
            //     self.block().acc32 = self.block().acc16 as f32;
            // }
            // // Opcode::MoveAcc16To64
            // 0x16 => {
            //     self.block().acc64 = self.block().acc16 as f64;
            // }
            // // Opcode::MoveAcc32To64
            // 0x17 => {
            //     self.block().acc64 = self.block().acc32 as f64;
            // }
            // Opcode::Call
            // 0x22 =>
            _ => unimplemented!("Unsupported instruction {opcode:?}!!"),
        }
        ControlFlow::Continue(())
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
