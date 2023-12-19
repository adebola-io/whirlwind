#![allow(unused)]
use std::{
    io::{stdout, Write},
    time::Duration,
};

use crate::VM;
use bytecode::{AccValue, RegisterList};

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
    stack: [u8; 2048],
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
            stack: [0; 2048],
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
            stack: [0; 2048],
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
                // Opcode::PrintConstPtr
                0x07 => self.printconstptr(vm),
                _ => unimplemented!("Unsupported instruction!!"),
            }
            self.ip += 1;
        }
        self.status = SequenceStatus::Resolved;
    }

    #[inline]
    fn printconstptr(&mut self, vm: &mut VM) {
        let constant = &vm.constants[self.current_block().registers.constptr];
        let lock = &mut stdout().lock();
        match constant {
            crate::Constant::String(c) => lock.write(c.as_bytes()),
            crate::Constant::Number(n) => lock.write(&n.to_ne_bytes()),
            crate::Constant::Bool(b) => lock.write(if *b { b"true" } else { b"false" }),
        }
        .unwrap();
        // lock.write(b"\n");
    }

    #[inline]
    unsafe fn load_immediate_constptr(&mut self) {
        let range = (self.ip + 1)..(self.ip + 8);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 8]);
        let value = usize::from_be_bytes(bytes);
        self.current_block().registers.constptr = value;
        self.ip += 8;
    }

    #[inline]
    unsafe fn load_immediate_stackptr(&mut self) {
        let range = (self.ip + 1)..(self.ip + 4);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 4]);
        let value = u32::from_be_bytes(bytes) as usize;
        self.current_block().registers.stackptr = value;
        self.ip += 4;
    }

    #[inline]
    unsafe fn load_immediate_8(&mut self) {
        self.ip += 1;
        let blob = (&*self.blob);
        self.current_block().registers.r8 = blob[self.ip] as i8;
    }

    #[inline]
    unsafe fn load_immediate_16(&mut self) {
        let range = (self.ip + 1)..(self.ip + 2);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 2]);
        self.current_block().registers.r16 = i16::from_be_bytes(bytes);
        self.ip += 2;
    }

    #[inline]
    unsafe fn load_immediate_32(&mut self) {
        let range = (self.ip + 1)..(self.ip + 4);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 4]);
        self.current_block().registers.r32 = f32::from_be_bytes(bytes);
        self.ip += 4;
    }

    #[inline]
    unsafe fn load_immediate_64(&mut self) {
        let range = (self.ip + 1)..(self.ip + 8);
        let blob = (&*self.blob);
        let bytes = *(blob[range].as_ptr() as *const [u8; 8]);
        self.current_block().registers.r64 = f64::from_be_bytes(bytes);
        self.ip += 8;
    }

    #[inline]
    fn current_block(&self) -> &mut Block {
        unsafe { &mut *self.current_block }
    }
}
