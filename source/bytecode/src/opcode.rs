pub enum Opcode {
    /// Adds the value in a register to the accumulator.
    AddAcc, // r1
    /// Subtracts the value in a register from the value in the accumulator,
    /// and stores the result in the accumulator.
    SubAcc, // r1
    /// Multiplies the value in the register by the value in the accumulator,
    /// and stores the result in the accumulator.
    MulAcc, // r1
    /// Divides the value in the accumulator by the value in a register.
    /// and stores the result in the accumulator.
    DivAcc, // r1
    /// Performs a modulus operator on the value in the accumulator using the
    /// value in a register, and stores the value in the accumulator.
    ModAcc,
    /// Right shifts the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    RightShiftAcc, // r1
    /// Left shifts the value in the accumulator by the value in a register,
    /// and stores the result in the accumulator.
    LeftShiftAcc, // r1
    /// Loads a value into the accumulator.
    Load,
    /// Loads a constant value from the constant pool, using the index stored in
    /// a register, and stores the value in the accumulator.
    LoadConst, // r1
    // Stores the value in the accumulator at a memory address and stores the address
    /// in a register.
    StoreAcc, // r1
    /// Creates a new sequence of instructions.
    SpawnSeq, // r1
    /// Forces the current sequence to wait for another sequence to end.
    SyncSeq, // r1
    /// Invokes an intrinsic injunction.
    InvokeInjunct, // r1
    /// Exits the program.
    Exit,
}
