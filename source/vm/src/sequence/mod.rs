use crate::VM;
use bytecode::{AccValue, RegisterList};
use std::{
    io::{stdout, Write},
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

#[derive(Debug)]
pub struct Block {
    registers: RegisterList,
}

impl Block {
    pub fn new() -> Self {
        Self {
            registers: RegisterList::new(),
        }
    }
}

pub struct Sequence {
    parent: Option<SequenceId>,
    pub id: SequenceId,
    pub ip: usize,
    stack: [u8; 16384],
    blocks: Vec<Block>,
    current_block: *mut Block,
    blob: *const [u8],
    pub status: SequenceStatus,
}

impl Sequence {
    /// Creates the main sequence.
    pub fn main() -> Self {
        Self {
            parent: None,
            id: SequenceId(0),
            ip: 0,
            stack: [0; 16384],
            blocks: vec![Block::new()],
            current_block: unsafe { std::mem::zeroed() },
            blob: unsafe { std::mem::zeroed() },
            status: SequenceStatus::Idle,
        }
    }

    pub fn new(parent: Option<SequenceId>, id: SequenceId) -> Self {
        Self {
            parent,
            id,
            ip: 0,
            stack: [0; 16384],
            blocks: vec![Block::new()],
            current_block: unsafe { std::mem::zeroed() },
            blob: unsafe { std::mem::zeroed() },
            status: SequenceStatus::Idle,
        }
    }

    pub fn clear(&mut self) {
        *self = Sequence::new(self.parent, self.id);
    }

    #[inline]
    pub unsafe fn step(&mut self, blob: &[u8], ip: usize, vm: &mut VM) {
        self.status = SequenceStatus::Running;
        self.ip = ip;
        let time = std::time::Instant::now();
        let len = self.blocks.len();
        self.current_block = self.blocks.get_unchecked_mut(len - 1) as *mut Block;
        self.blob = blob as *const [u8];
        while self.ip < blob.len() {
            // Yield after 10microsecs.
            if time.elapsed() >= Duration::from_micros(10) && vm.running_sequences > 1 {
                self.status = SequenceStatus::Paused;
                return;
            }

            match blob.get_unchecked(self.ip) {
                // Opcode::Exit.
                0x00 => {
                    vm.exited = true;
                    break;
                }
                // Opcode::LoadImToReg8
                0x01 => self.load_immediate_8(),
                // Opcode::LoadImToReg16
                0x02 => self.load_immediate_16(),
                // Opcode::LoadImToReg32
                0x03 => self.load_immediate_32(),
                // Opcode::LoadImToReg64
                0x04 => self.load_immediate_64(),
                // Opcode::LoadImToStackPtr
                0x05 => self.load_immediate_stackptr(),
                // Opcode::LoadImToConstPtr
                0x06 => self.load_immediate_constptr(),
                // Opcode::PrintAcc8
                0x07 => self.printacc8(),
                // Opcode::PrintAcc16
                0x08 => self.printacc16(),
                // Opcode::PrintAcc32
                0x09 => self.printacc32(),
                // Opcode::PrintAcc64
                0x10 => self.printacc64(),
                // Opcode::PrintConstPtr
                0x11 => self.printconstptr(vm),
                // Opcode::MoveAcc8To16
                0x12 => {
                    self.block().registers.acc16 = self.block().registers.acc8 as i16;
                }
                // Opcode::MoveAcc8To32
                0x13 => {
                    self.block().registers.acc32 = self.block().registers.acc8 as f32;
                }
                // Opcode::MoveAcc8To64
                0x14 => {
                    self.block().registers.acc64 = self.block().registers.acc8 as f64;
                }
                // Opcode::MoveAcc16To32
                0x15 => {
                    self.block().registers.acc32 = self.block().registers.acc16 as f32;
                }
                // Opcode::MoveAcc16To64
                0x16 => {
                    self.block().registers.acc64 = self.block().registers.acc16 as f64;
                }
                // Opcode::MoveAcc32To64
                0x17 => {
                    self.block().registers.acc64 = self.block().registers.acc32 as f64;
                }
                // Opcode::LoadAcc8
                0x18 => self.block().registers.acc8 = self.block().registers.r8,
                // Opcode::LoadAcc16
                0x19 => self.block().registers.acc16 = self.block().registers.r16,
                // Opcode::LoadAcc32
                0x20 => self.block().registers.acc32 = self.block().registers.r32,
                // Opcode::LoadAcc64
                0x21 => self.block().registers.acc64 = self.block().registers.r64,
                _ => unimplemented!("Unsupported instruction!!"),
            }
            self.ip += 1;
        }
        self.status = SequenceStatus::Resolved;
    }

    #[inline]
    fn printconstptr(&self, vm: &mut VM) {
        let constant = &vm.constants[self.block().registers.constptr];
        let lock = &mut stdout().lock();
        match constant {
            crate::Constant::String(c) => lock.write(c.as_bytes()),
            crate::Constant::Number(n) => lock.write(&n.to_ne_bytes()),
            crate::Constant::Bool(b) => lock.write(if *b { b"true" } else { b"false" }),
        }
        .unwrap();
        // lock.write(b"\n");
    }

    /// Prints the value in the 8-bit accumulator.
    #[inline]
    fn printacc8(&self) {
        let value = self.block().registers.acc8;
        let lock = &mut stdout().lock();
        lock.write(&value.to_ne_bytes()).unwrap();
    }

    /// Prints the value in the 16-bit accumulator.
    #[inline]
    fn printacc16(&self) {
        let value = self.block().registers.acc16;
        let lock = &mut stdout().lock();
        lock.write(&value.to_ne_bytes()).unwrap();
    }

    /// Prints the value in the 32-bit accumulator.
    #[inline]
    fn printacc32(&self) {
        let value = self.block().registers.acc32;
        let lock = &mut stdout().lock();
        lock.write(&value.to_ne_bytes()).unwrap();
    }

    /// Prints the value in the 64-bit accumulator.
    #[inline]
    fn printacc64(&self) {
        let value = self.block().registers.acc64;
        let lock = &mut stdout().lock();
        lock.write(&value.to_ne_bytes()).unwrap();
    }

    #[inline]
    unsafe fn load_immediate_constptr(&mut self) {
        let range = (self.ip + 1)..(self.ip + 8);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 8]);
        let value = usize::from_be_bytes(bytes);
        self.block().registers.constptr = value;
        self.ip += 8;
    }

    #[inline]
    unsafe fn load_immediate_stackptr(&mut self) {
        let range = (self.ip + 1)..(self.ip + 4);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 4]);
        let value = u32::from_be_bytes(bytes) as usize;
        self.block().registers.frameptr = value;
        self.ip += 4;
    }

    #[inline]
    unsafe fn load_immediate_8(&mut self) {
        self.ip += 1;
        let blob = (&*self.blob);
        self.block().registers.r8 = blob[self.ip] as i8;
    }

    #[inline]
    unsafe fn load_immediate_16(&mut self) {
        self.block().registers.r16 = self.read_i16_from_stream();
    }

    #[inline]
    unsafe fn read_i16_from_stream(&mut self) -> i16 {
        let range = (self.ip + 1)..(self.ip + 2);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 2]);
        self.ip += 2;
        let from_be_bytes = i16::from_be_bytes(bytes);
        from_be_bytes
    }

    #[inline]
    unsafe fn load_immediate_32(&mut self) {
        let range = (self.ip + 1)..(self.ip + 4);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 4]);
        self.block().registers.r32 = f32::from_be_bytes(bytes);
        self.ip += 4;
    }

    #[inline]
    unsafe fn load_immediate_64(&mut self) {
        let range = (self.ip + 1)..(self.ip + 8);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 8]);
        self.block().registers.r64 = f64::from_be_bytes(bytes);
        self.ip += 8;
    }

    #[inline]
    fn block(&self) -> &mut Block {
        unsafe { &mut *self.current_block }
    }
}
