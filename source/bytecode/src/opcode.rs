/// Bytecode Operations in Whirlwind.
#[derive(Debug)]
pub enum Opcode {
    // PRINT OPCODES
    // --
    /// Prints the value in the 8-bit accumulator to the console.
    PrintAcc8,
    /// Prints the value in the 16-bit accumulator to the console.
    PrintAcc16,
    /// Prints the value in the 32-bit accumulator to the console.
    PrintAcc32,
    /// Prints the value in the 64-bit accumulator to the console.
    PrintAcc64,
    /// Prints the value in the constant pointer register.
    PrintConstPtr,

    // LOAD/MOVE OPCODES
    // --
    /// Loads the next byte value into the 8-bit register.
    LoadImToReg8,
    /// Loads the next two bytes as a u16 value into the 16-bit register.
    LoadImToReg16,
    /// Loads the next four bytes as a u32 value into the 32-bit register.
    LoadImToReg32,
    /// Loads the next eight bytes as a u64 value into the 64-bit register.
    LoadImToReg64,
    /// Loads the next four bytes as a usize into the stackptr register.
    /// The value decoded is offset from the start of the function's frame,
    /// not the overall stack.
    LoadImToStackPtr,
    /// Loads the next eight bytes as a usize into the constptr register.
    LoadImToConstPtr,
    /// Loads the value in an 8-bit register into the 8-bit accumulator.
    LoadAcc8,
    /// Loads the value in the 16-bit register into the 16-bit accumulator.
    LoadAcc16,
    /// Loads the value in the 32-bit register into the 32-bit accumulator.
    LoadAcc32,
    /// Loads the value in the 64-bit register into the 64-bit accumulator.
    LoadAcc64,
    /// Moves the value in the 8-bit accumulator into the 64-bit accumulator.
    MoveAcc8To64,
    /// Moves the value in the 16-bit accumulator into the 64-bit accumulator.
    MoveAcc16To64,
    /// Moves the value in the 32-bit accumulator into the 64-bit accumulator.
    MoveAcc32To64,

    // ARITHMETIC OPCODES.
    // ---
    /// Adds the value in the a register to its accumulator.
    /// If the next byte is 0, it adds 8 bits.
    /// If the next byte is 1, it adds 16 bits.
    /// If the next byte is 2, it adds 32 bits.
    /// If the next byte is 3, its adds 64 bits.
    AddAcc,
    /// Subtracts the value in a register from the value in its accumulator,
    /// and stores the result in the accumulator.
    /// The schema is the same as AddAcc.
    SubAcc,
    /// Multiplies the value in the register by the value in the accumulator,
    /// and stores the result in the accumulator.
    /// The schema is the same as AddAcc.
    MulAcc,
    /// Takes the square root of the value in the accumulator and stores the
    /// result in the accumulator.
    /// The schema is the same as AddAcc.
    SqrtAcc,
    /// Divides the value in an accumulator by the value in a register.
    /// and stores the result in the accumulator.
    /// The schema is the same as AddAcc.
    DivAcc,
    /// Performs a modulus operation on the value in the accumulator using the
    /// value in a register.
    /// The schema is the same as AddAcc.
    ModAcc,
    /// Decrements the value in an accumulator by the value in a register,
    /// and stores the result in the accumulator.
    /// The schema is the same as AddAcc.
    DecAcc,
    /// Increments the value in an accumulator by the value in a register,
    /// and stores the result in the accumulator.
    /// The schema is the same as AddAcc.
    IncrAcc,
    /// Right shifts the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    /// The schema is the same as AddAcc.
    RShiftAcc,
    /// Left shifts the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    /// The schema is the same as AddAcc.
    LShiftAcc,
    /// Checks that the value in the accumulator equals the value in a register,
    /// and stores the result in the accumulator.
    /// The schema is the same as AddAcc.
    EqAcc,
    /// Checks that the value in the accumulator does not equal the value in the register,
    /// and stores the result in the accumulator.
    /// The schema is the same as AddAcc.
    NeAcc,
    /// Checks that the address in the accumulator is equal to the address in a register
    /// and stores the result in the boolx register.
    /// The schema is the same as AddAcc.
    IsAcc,
    /// Stores the value in an accumulator and stores the address into a register.
    /// The schema is the same as AddAcc.
    StoreAcc,

    // CONTROL FLOW OPCODES
    // --
    /// Jumps to an instruction index if the value stored in boolx register is true.
    /// The next 8 bytes correspond to the instruction index.
    JumpIfTrue,
    /// Jumps to an instruction index based on the value in the boolx register.
    /// The following 8 bytes correspond to the instruction index for truth.
    /// The next 8 bytes correspond to the instruction index for falsehood.
    JumpConditional,
    /// Reads the next 24 bytes in the stream for a defined loop.
    /// The first 8 bytes correspond to the starting instruction index.
    /// The second set of 8 bytes corresponds to the ending instruction index.
    /// The third set of 8 bytes correspond to the number of iterations in the loop.
    LoopFor,
    /// An instruction that literally does nothing.
    /// It is useful for predefined loops over empty blocks.
    Stall,
    /// Breaks out of a predefined loop by setting the loopcounter register to the last
    /// value in the loop.
    BreakLoop,
    /// Moves the instruction pointer to a new index.
    Goto,
    /// Calls a function.
    Call,
    /// Returns to the caller block of the current function and continues execution.
    Return,

    // MEMORY ALLOCATION OPCODES.
    // --
    /// Marks a slice in the stack to be moved to the heap as an `Object` when the block ends.
    /// The next 4 bytes correspond to the start of the slice.
    /// The 4 bytes after correspond to the end of the slice.
    MarkForHeap,
    /// Given a model id, creates an instance of a model on the stack and stores its reference in
    /// the stackptr register.
    NewModel,
    /// Given an opaque value id, creates an instance of the value on the stack and stores a reference to it.
    NewOpaque,
    /// Creates a new empty array on the heap and stores a reference to it in a register.
    NewArr,

    // SEQUENCE OPCODES.
    // --
    /// Spawns and starts running a new sequence of instructions.
    /// The next 8 bytes correspond to the instruction index to start from.
    SpawnSeq,
    /// Forces the current sequence to wait for another to end.
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
            0x00 => Self::Exit,
            0x01 => Self::LoadImToReg8,
            _ => todo!(),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        match value {
            Opcode::Exit => 0,
            _ => todo!(),
        }
    }
}
