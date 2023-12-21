/// Bytecode Operations in Whirlwind.
#[derive(Debug, PartialEq)]
pub enum Opcode {
    // LOAD/MOVE OPCODES
    // --
    /// Loads the immediate next byte value into the 8-bit register.
    ///
    /// Format: `[LoadIr8] [value]`
    LoadIr8,
    /// Loads the next two bytes as a u16 value into the 16-bit register.
    ///
    /// Format: `[LoadIr8] [value: [a, b]]`
    LoadIr16,
    /// Loads the next four bytes as a u32 value into the 32-bit register.
    ///
    /// Format: `[LoadIr32] [value: [a, b, c, d]]`
    LoadIr32,
    /// Loads the next eight bytes as a u64 value into the 64-bit register.
    ///
    /// Format: `[LoadIr64] [value: [a, b, c, d, e, f, g, h]]`
    LoadIr64,
    /// Loads the immediate next byte value into the 8-bit accumulator.
    /// It is useful for unary expressions, to circumvent having to move from the
    /// stream to the register to the accumulator.
    /// Format: `[LoadIacc8] [value]`
    LoadIacc8,
    /// Loads the next two bytes as a u16 value into the 16-bit accumulator.
    ///
    /// Format: `[LoadIacc16] [value: [a, b]]`
    LoadIacc16,
    /// Loads the next four bytes as a u32 value into the 32-bit accumulator.
    ///
    /// Format: `[LoadIacc32] [value: [a, b, c, d]]`
    LoadIacc32,
    /// Loads the next eight bytes as a u64 value into the 64-bit accumulator.
    ///
    /// Format: `[LoadIacc64] [value: [a, b, c, d, e, f, g, h]]`
    LoadIacc64,

    /// Loads an immediate value into an address in the current frame.
    /// The following byte is the index to start writing from.
    /// if the next byte is n, it loads the next 2^n bytes into the frame.
    /// Format: `[0x05] [start: [a, b, c, d]] [len] [...data]`
    LoadIframe,
    /// Loads the next eight bytes as a usize into the constptra register.
    ///
    /// Format: `[LoadIconstptra] [index: [a, b, c, d, e, f, g, h]]`
    LoadIconstptra,
    /// Loads the next eight bytes as a usize into the constptrb register.
    ///
    /// Format: `[LoadIconstptrb] [index: [a, b, c, d, e, f, g, h]]`
    LoadIconstptrb,
    /// Loads the value in an 8-bit register into the 8-bit accumulator.
    ///
    /// Format: `[Upaccr8]`
    Upaccr8,
    /// Loads the value in the 16-bit register into the 16-bit accumulator.
    ///
    /// Format: `[Upaccr16]`
    Uppaccr16,
    /// Loads the value in the 32-bit register into the 32-bit accumulator.
    ///
    /// Format: `[Upaccr32]`
    Upaccr32,
    /// Loads the value in the 64-bit register into the 64-bit accumulator.
    ///
    /// Format: `[Upaccr64]`
    Upaccr64,
    /// Moves the value in the 8-bit accumulator into the 16-bit accumulator.
    ///
    /// Format: `[Moveacc8To16]`
    Moveacc8To16,
    /// Moves the value in the 8-bit accumulator into the 32-bit accumulator.
    ///
    /// Format: `[Moveacc8To32]`
    Moveacc8To32,
    /// Moves the value in the 8-bit accumulator into the 64-bit accumulator.
    ///
    /// Format: `[Moveacc8To64]`
    Moveacc8To64,
    /// Moves the value in the 16-bit accumulator into the 32-bit accumulator.
    ///
    /// Format: `[Moveacc8To32]`
    Moveacc16To32,
    /// Moves the value in the 16-bit accumulator into the 64-bit accumulator.
    ///
    /// Format: `[Moveacc16To64]`
    Moveacc16To64,
    /// Moves the value in the 32-bit accumulator into the 64-bit accumulator.
    ///
    /// Format: `[Moveacc32To64]`
    Moveacc32To64,
    /// Moves the value in the 8-bit accumulator to the frame.
    /// The next 4 bytes correspond to the index in the frame to write into.
    ///
    /// Format: `[Moveacc32To64] [destination: [a, b, c, d]]`
    Moveacc8ToFrame,
    /// Moves the value in the 16-bit accumulator to the frame in the frame.
    /// The next 4 bytes correspond to the index in the frame to start writing into.
    /// It will write into the first and overflow into the next.
    ///
    /// Format: `[Moveacc16ToFrame] [destination: [a, b, c, d]]`
    Moveacc16ToFrame,
    /// Moves the value in the 32-bit accumulator to the frame.
    /// The next 4 bytes correspond to the index in the frame to start writing into.
    /// It will write into the first and overflow into the next three.
    ///
    /// Format: `[Moveacc32ToFrame] [destination: [a, b, c, d]]`
    Moveacc32ToFrame,
    /// Moves the value in the 64-bit accumulator to the frame.
    /// The next 4 bytes correspond to the index in the frame to start writing into.
    /// It will write into the first and overflow into the next seven.
    ///
    /// Format: `[Moveacc64ToFrame] [destination: [a, b, c, d]]`
    Moveacc64ToFrame,

    // PRINT OPCODES
    // --
    /// Prints the value in the 8-bit accumulator to the console.
    ///
    /// Format: `[Printacc8]`
    Printacc8,
    /// Prints the value in the 16-bit accumulator to the console.
    ///
    /// Format: `[Printacc16]`
    Printacc16,
    /// Prints the value in the 32-bit accumulator to the console.
    ///
    /// Format: `[Printacc32]`
    Printacc32,
    /// Prints the value in the 64-bit accumulator to the console.
    ///
    /// Format: `[Printacc64]`
    Printacc64,
    /// Prints the value in the constptra register.
    ///
    /// Format: `[Printconstptra]`
    Printconstptra,
    /// Prints the value in the constptrb register.
    ///
    /// Format: `[Printconstptrb]`
    Printconstptrb,

    // ARITHMETIC OPCODES.
    // ---
    /// Adds the value in a register to its accumulator.
    /// - If the next byte is 0, it adds 8 bits.
    /// - If the next byte is 1, it adds 16 bits.
    /// - If the next byte is 2, it adds 32 bits.
    /// - If the next byte is 3, its adds 64 bits.
    ///
    /// - If the following byte is 0, it stores the result back in the accumulator.
    /// - If the byte is 1, it reads the value of a frame address and stores it
    /// there.
    ///
    /// Formats:
    /// - `[Addacc] [0x00 | 0x01 | 0x02 | 0x03] [[0] | [1] [address: [a, b, c, d]]]`
    Addacc,
    /// Add the value in an accumulator directly to a frame address.
    /// - If the next byte is 0, it adds 8 bits.
    /// - If the next byte is 1, it adds 16 bits.
    /// - If the next byte is 2, it adds 32 bits.
    /// - If the next byte is 3, its adds 64 bits.
    AddaccToframe,
    /// Subtracts the value in a register from the value in its accumulator.
    /// The schema & format is the same as AddAcc.
    Subacc,
    /// Multiplies the value in the register by the value in the accumulator.
    /// The schema & format is the same as AddAcc.
    MulAcc,
    /// Takes the square root of the value in the accumulator.
    /// The schema & format is the same as AddAcc,
    /// WITH THE EXCEPTION THAT THE IMPLICIT RESULT (0) IS ALWAYS STORED IN ACC64.
    Sqrtacc,
    /// Divides the value in an accumulator by the value in a register.
    /// and stores the result in the accumulator.
    /// WITH THE EXCEPTION THAT THE IMPLICIT RESULT (0) IS ALWAYS STORED IN ACC64.
    Divacc,
    /// Performs a modulus operation on the value in the accumulator using the
    /// value in a register.
    /// The schema & format is the same as AddAcc.
    Modacc,
    /// Decrements the value in an accumulator by the value in a register,
    /// and stores the result in the accumulator.
    /// The schema & format is the same as AddAcc.
    Decacc,
    /// Increments the value in an accumulator by the value in a register,
    /// and stores the result in the accumulator.
    /// The schema & format is the same as AddAcc.
    Incacc,
    /// Right shifts the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    /// The schema & format is the same as AddAcc.
    RShacc,
    /// Left shifts the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    /// The schema & format is the same as AddAcc.
    LShacc,
    /// Checks that the value in the accumulator equals the value in a register,
    /// and stores the result in the accumulator.
    /// The schema & format is the same as AddAcc.
    Eqacc,
    /// Checks that the value in the accumulator does not equal the value in the register,
    /// and stores the result in the accumulator.
    /// The schema & format is the same as AddAcc.
    Neacc,
    /// Stores the value in an accumulator and stores the address into a register.
    /// The schema & format is the same as AddAcc.
    Storeacc,

    // CONTROL FLOW OPCODES
    // --
    /// Jumps to an instruction index if the value stored in boolx register is true.
    /// The next 8 bytes correspond to the instruction index.
    ///
    /// Format: `[JumpIfTrue] [index: [a, b, c, d, e, f, g, h]]`
    JumpIfTrue,
    /// Jumps to an instruction index based on the value in the boolx register.
    /// The following 8 bytes correspond to the instruction index for truth.
    /// The next 8 bytes correspond to the instruction index for falsehood.
    ///
    /// Format: `[JumpConditional] [true: [a, b, c, d, e, f, g, h]] [false: [i, j, k, l, m, n, o, p]]`
    JumpConditional,
    /// Predefines a loop.
    /// The first 8 bytes correspond to the starting instruction index.
    /// The second set of 8 bytes corresponds to the ending instruction index.
    /// The third set of 8 bytes correspond to the number of iterations in the loop.
    ///
    /// Format: `[LoopFor] [start: [a..h]] [end: [i..p]] [count: [q..x]]`
    LoopFor,
    /// An instruction that literally does nothing.
    /// It is useful for predefined loops over empty blocks.
    ///
    /// Format: `[Stall]`
    Stall,
    /// Breaks out of a predefined loop by setting the loopcounter register to the last
    /// value in the loop.
    ///
    /// Format: `[BreakLoop]`
    BreakLoop,
    /// Moves the instruction pointer to a new index.
    /// The next 8 bytes correspond to the instruction index.
    ///
    /// Format: `[Goto] [destination: [a, b, c, d, e, f, g, h]]`
    Goto,
    /// Calls a function.
    /// The next 4 bytes correspond to the index of the function in the function registry.
    ///
    /// Format: `[Call]`
    Call,
    /// Returns to the caller block of the current function and continues execution.
    ///
    /// Format: `[Return]`
    Return,
    /// Signals to the virtual machine to move heap marked objects.
    ///
    /// Format: `[EndBlock]`
    EndBlock,

    // MEMORY ALLOCATION OPCODES.
    // --
    /// Marks a value slice in the frame to be moved to the heap as an `Object` when the block ends.
    /// The next 4 bytes correspond to the start of the slice.
    /// The 4 bytes after correspond to the end of the slice.
    ///
    /// Format: `[MarkForHeap] [start: [a, b, c, d]] [end: [e, f, g, h]]`
    MarkForHeap,
    /// Given an opaque value id, creates an instance of the value on the stack and stores a reference to it.
    NewOpaque,
    /// Creates a new empty array on the heap and stores a reference to it in a register.
    NewArrLiteral,

    // SEQUENCE OPCODES.
    // --
    /// Spawns and starts running a new sequence of instructions.
    /// The next 8 bytes correspond to the instruction index to start from.
    SpawnSeq,
    /// Forces the current sequence to wait for another to end.
    ///
    /// It will change the status of the current sequence to SequenceStatus::Waiting,
    /// until it is changed by any running child sequence.
    SyncSeq,
    /// Halts the sequence execution.
    HaltSeq,
    /// Invokes an intrinsic injunction.
    Invoke,
    /// Exits the program.
    Exit,
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        match value {
            0 => Opcode::Exit,
            1 => Opcode::LoadIr8,
            2 => Opcode::LoadIr16,
            3 => Opcode::LoadIr32,
            4 => Opcode::LoadIr64,
            5 => Opcode::LoadIacc8,
            6 => Opcode::LoadIacc16,
            7 => Opcode::LoadIacc32,
            8 => Opcode::LoadIacc64,
            9 => Opcode::LoadIframe,
            10 => Opcode::LoadIconstptra,
            11 => Opcode::LoadIconstptrb,

            25 => Opcode::Printacc8,
            28 => Opcode::Printacc64,
            29 => Opcode::Printconstptra,
            31 => Opcode::Addacc,

            35 => Opcode::Sqrtacc,
            42 => Opcode::Eqacc,
            45 => Opcode::JumpIfTrue,
            47 => Opcode::LoopFor,
            48 => Opcode::Stall,
            51 => Opcode::Call,
            52 => Opcode::Return,
            _ => unimplemented!("{value}"),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        match value {
            Opcode::LoadIr8 => 1,
            Opcode::LoadIr16 => 2,
            Opcode::LoadIr32 => 3,
            Opcode::LoadIr64 => 4,
            Opcode::LoadIacc8 => 5,
            Opcode::LoadIacc16 => 6,
            Opcode::LoadIacc32 => 7,
            Opcode::LoadIacc64 => 8,
            Opcode::LoadIframe => 9,
            Opcode::LoadIconstptra => 10,
            Opcode::LoadIconstptrb => 11,
            Opcode::Upaccr8 => todo!(),
            Opcode::Uppaccr16 => todo!(),
            Opcode::Upaccr32 => todo!(),
            Opcode::Upaccr64 => todo!(),
            Opcode::Moveacc8To16 => todo!(),
            Opcode::Moveacc8To32 => todo!(),
            Opcode::Moveacc8To64 => todo!(),
            Opcode::Moveacc16To32 => todo!(),
            Opcode::Moveacc16To64 => todo!(),
            Opcode::Moveacc32To64 => todo!(),
            Opcode::Moveacc8ToFrame => todo!(),
            Opcode::Moveacc16ToFrame => todo!(),
            Opcode::Moveacc32ToFrame => todo!(),
            Opcode::Moveacc64ToFrame => todo!(),
            Opcode::Printacc8 => 25,
            Opcode::Printacc16 => todo!(),
            Opcode::Printacc32 => todo!(),
            Opcode::Printacc64 => 28,
            Opcode::Printconstptra => 29,
            Opcode::Printconstptrb => todo!(),
            Opcode::Addacc => 31,
            Opcode::AddaccToframe => todo!(),
            Opcode::Subacc => todo!(),
            Opcode::MulAcc => todo!(),
            Opcode::Sqrtacc => 35,
            Opcode::Divacc => todo!(),
            Opcode::Modacc => todo!(),
            Opcode::Decacc => todo!(),
            Opcode::Incacc => todo!(),
            Opcode::RShacc => todo!(),
            Opcode::LShacc => todo!(),
            Opcode::Eqacc => 42,
            Opcode::Neacc => todo!(),
            Opcode::Storeacc => todo!(),
            Opcode::JumpIfTrue => 45,
            Opcode::JumpConditional => todo!(),
            Opcode::LoopFor => 47,
            Opcode::Stall => 48,
            Opcode::BreakLoop => todo!(),
            Opcode::Goto => todo!(),
            Opcode::Call => 51,
            Opcode::Return => 52,
            Opcode::EndBlock => todo!(),
            Opcode::MarkForHeap => todo!(),
            Opcode::NewOpaque => todo!(),
            Opcode::NewArrLiteral => todo!(),
            Opcode::SpawnSeq => todo!(),
            Opcode::SyncSeq => todo!(),
            Opcode::HaltSeq => todo!(),
            Opcode::Invoke => todo!(),
            Opcode::Exit => 0,
        }
    }
}
