use std::any::Any;

type ValueAddress = *mut dyn Any;

pub enum Register {
    /// The accumulator.
    Acc(u64),
    /// A register for storing small unsigned integer values.
    Suintx(u8),
    /// A register for storing 16-bit unsigned integer values.
    Muintx(u16),
    /// A register for storing 32-bit unsigned integer values.
    Luintx(u32),
    /// A register for storing 64-bit unsigned integer values.
    Xluintx(u64),
    /// A register for storing signed integer values.
    Aintx(isize),
    /// Register for temporary boolean values.
    Boolx(bool),
    /// A register for storing addresses.
    Addrx(ValueAddress),
    /// Another register for storing addresses.
    Eaddrx(ValueAddress),
    /// Index to a position on the stack.
    Sp(usize),
    /// Address of the next executable instruction.
    Ip(usize),
    /// The counter value for loops.
    Cntx(usize),
}
