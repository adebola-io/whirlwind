/// Bytecode Operations in Whirlwind.
pub enum Opcode {
    /// Loads a value into a register.
    LoadReg,
    /// Loads a constant value from the constant pool, using the index stored in
    /// a register, and stores the value in the accumulator.
    LoadConst,
    /// Loads the value in a register into the accumulator.
    LoadAcc,
    /// Empties the value in the accumulator into a register.
    MovAcc,
    /// Adds the value in a register to the accumulator.
    AddAcc,
    /// Subtracts the value in a register from the value in the accumulator,
    /// and stores the result in the accumulator.
    SubAcc,
    /// Multiplies the value in the register by the value in the accumulator,
    /// and stores the result in the accumulator.
    MulAcc,
    /// Divides the value in the accumulator by the value in a register.
    /// and stores the result in the accumulator.
    DivAcc,
    /// Performs a modulus operation on the value in the accumulator using the
    /// value in a register.
    ModAcc,
    /// Decrements the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    DecAcc,
    /// Increments the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    IncrAcc,
    /// Right shifts the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    RShiftAcc,
    /// Left shifts the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    LShiftAcc,
    /// Checks that the value in the accumulator equals the value in a register,
    /// and stores the result in the accumulator.
    EqAcc,
    /// Checks that the value in the accumulator does not equal the value in the register,
    /// and stores the result in the accumulator.
    NeAcc,
    /// Looks up the index of a variable from an outer scope.
    Lookup,
    /// Checks that the address in the accumulator is equal to the address in a register
    /// and stores the value in the accumulator.
    Is,
    /// Jumps to an instruction index stored in the register if the value in the accumulator is true.
    JumpIfTrue,
    /// Jumps to the instruction index stored in the register if the value in the accumulator is false.
    JumpIfFalse,
    /// Moves the instruction pointer to a new index.
    Goto,
    /// Calls a function.
    Call,
    /// Creates a copy of the value in the register and stores it in the accumulator.
    Copy,
    /// Returns to the caller block of the current function and continues execution.
    Return,
    /// Given a model id, creates an instance of a model on the stack and stores its reference in a register.
    NewModel,
    /// Given an opaque value id, creates an instance of the value on the stack and stores a reference to it.
    NewOpaque,
    /// Creates a new empty array on the heap and stores a reference to it in a register.
    NewArr,
    /// Stores the value in the accumulator on the stack and stores the address
    /// in a register.
    StoreAcc,
    /// Spawns and starts running a new sequence of instructions.
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
