/// Padding for the bytecode.
pub const PAD: u8 = 0;
/// The maximum number of assignable registers for each data type.
///
/// It is set to `u8::MAX` so that each register id can be represented
/// by a single byte, allowing for compactness over simplicity.
///
/// Registers are function scoped, so unless there is a single function with
/// a ridiculous amount of nested blocks, this should not be a real issue.
pub const MAX_REGISTER_COUNT: u8 = 255;

/// Bytecode Operation codes in Whirlwind.
///
/// Each opcode is one-byte long but has a variable length of data that
/// may follow it in the stream.
#[derive(Debug, PartialEq)]
pub enum Opcode {
    // =============
    // DATA OPCODES
    // =============

    // --------------
    // Immediate Loads.
    // --------------
    /// Loads a byte value into an int8 register.
    ///
    /// Format: `[LoadInt8] [register] [value]`
    LoadInt8,
    /// Loads two bytes as a u16 value into an int16 register.
    ///
    /// Format: `[LoadInt8] [register] [value: [a, b]]`
    LoadInt16,
    /// Loads four bytes as a u32 value into an f32 register.
    ///
    /// Format: `[LoadFloat32] [register] [value: [a, b, c, d]]`
    LoadFloat32,
    /// Loads eight bytes as a u64 value into an r64 register.
    ///
    /// Format: `[LoadFloat64] [register] [value: [a, b, c, d, e, f, g, h]]`
    LoadFloat64,
    /// Loads a boolean byte value (0 or 1) into a bool register.
    ///
    /// Format: `[LoadBool] [register] [00 | 01]`
    LoadBool,
    /// Loads the 4-byte callable pointer id into the function ptr.
    ///
    /// Format: `[LoadFunctionPtr] [register] [id: [a, b, c, d]]`
    LoadFunctionPtr,

    // ---------------
    // Register-to-Register.
    // ---------------
    /// Moves a value from one int8 register to another.
    ///
    /// Format: `[MoveInt8] [destination] [source]`
    MoveInt8,
    /// Moves a value from one int16 register to another.
    ///
    /// Format: `[MoveInt16] [destination] [source]`
    MoveInt16,
    /// Moves a value from one f32 register to another.
    ///
    /// Format: `[MoveFloat32] [destination] [source]`
    MoveFloat32,
    /// Moves a value from one f64 register to another.
    ///
    /// Format: `[MoveFloat64] [destination] [source]`
    MoveFloat64,
    /// Moves a value from one boolean register to another.
    ///
    /// Format: `[MoveBool] [destination] [source]`
    MoveBool,
    /// Moves an address from one addr register to another.
    ///
    /// Format: `[MoveAddr] [destination] [source]`
    MoveAddr,
    /// Moves a value from the ether register to a register of another
    /// data type.
    ///
    /// If the next byte is 0, it moves to an int8 register.
    /// If the next byte is 1, it moves to an int16 register.
    /// If the next byte is 2, it moves to a float32 register.
    /// If the next byte is 3, it moves to a float64 register.
    /// If the next byte is 4, it moves to a bool register.
    /// If the next byte is 5, it moves to an addr register.
    ///
    /// Format: `[MoveEther] ] [destination-group] [destination-register]`
    MoveEther,

    // ---------------
    // Register-to-Stack
    // ---------------
    /// Stores a value from an int8 register to an address on the
    /// current stack frame.
    ///
    /// Format: `[StoreInt8] [destination-address] [source-register]`
    StoreInt8,
    /// Stores a value from an int16 register to an address on the
    /// current stack frame.
    ///
    /// Format: `[StoreInt16] [destination-address] [source-register]`
    StoreInt16,
    /// Stores a value from a float32 register to an address on the
    /// current stack frame.
    ///
    /// Format: `[StoreFloat32] [destination-address] [source-register]`
    StoreFloat32,
    /// Stores a value from an float64 register to an address on the
    /// current stack frame.
    ///
    /// Format: `[StoreFloat64] [destination-address] [source-register]`
    StoreFloat64,
    /// Stores a value from a bool register to an address on the
    /// current stack frame.
    ///
    /// Format: `[StoreBool] [destination-address] [source-register]`
    StoreBool,
    /// Stores a value from a functionptr register to an address on the
    /// current stack frame.
    ///
    /// Format: `[StoreFunctionPtr] [destination-address] [source-register]`
    StoreFunctionPtr,
    /// Stores a heap pointer from an addr register to an address on the
    /// current stack frame.
    ///
    /// Format: `[StoreAddr] [destination-address] [source-register]`
    StoreAddr,

    // ---------------
    // Stack-to-Register
    // ---------------
    /// Retrieves a value from an address in the current stack frame to
    /// an int8 register.
    ///
    /// Format: `[RetrieveInt8] [destination-register] [source-address]`
    RetrieveInt8,
    /// Retrieves a value from an address in the current stack frame to an
    /// int16 register.
    ///
    /// Format: `[RetrieveInt16] [destination-register] [source-address]`
    RetrieveInt16,
    /// Retrieves a value from an address in the current stack frame to a
    /// float32 register.
    ///
    /// Format: `[RetrieveFloat32] [destination-register] [source-address]`
    RetrieveFloat32,
    /// Retrieves a value from an address in the current stack frame to a
    /// float64 register.
    ///
    /// Format: `[RetrieveFloat64] [destination-register] [source-address]`
    RetrieveFloat64,
    /// Retrieves a value from an address in the current stack frame to a
    /// bool register.
    ///
    /// Format: `[RetrieveBool] [destination-register] [source-address]`
    RetrieveBool,
    /// Retrieves a value from an address in the current stack frame to a
    /// functionptr register.
    ///
    /// Format: `[RetrieveFunctionPtr] [destination-register] [source-address]`
    RetrieveFunctionPtr,
    /// Retrieves a value from an address in the current stack frame to a
    /// addr register.
    ///
    /// Format: `[RetrieveAddr] [destination-register] [source-address]`
    RetrieveAddr,

    // ------------------
    // Return-to-Register
    // ------------------
    /// Moves the value in the global return register to an int8 register.
    ///
    /// Format: `[ReturnInt8] [register]`
    ReturnInt8,
    /// Moves the value in the global return register to an int16 register.
    ///
    /// Format: `[ReturnInt16] [register]`
    ReturnInt16,
    /// Moves the value in the global return register to a float32 register.
    ///
    /// Format: `[ReturnFloat32] [register]`
    ReturnFloat32,
    /// Moves the value in the global return register to a float64 register.
    ///
    /// Format: `[ReturnFloat64] [register]`
    ReturnFloat64,
    /// Moves the value in the global return register to a bool register.
    ///
    /// Format: `[ReturnBool] [register]`
    ReturnBool,
    /// Moves the value in the global return register to a functionptr register.
    ///
    /// Format: `[ReturnFunctionPtr] [register]`
    ReturnFunctionPtr,
    /// Moves the value in the global return register to an addr register.
    ///
    /// Format: `[ReturnAddr] [register]`
    ReturnAddr,
    /// Moves the value in the global return register to the ether register.
    ///
    /// Format: `[ReturnFunctionPtr] [register]`
    ReturnEther,

    // ===================
    // ARITHMETIC OPCODES
    // ===================
    /// Compound opcode that adds the value in two registers
    /// and stores the result in a third.
    ///
    /// - If the next byte is 0, it adds 8 bits.
    /// - If the next byte is 1, it adds 16 bits.
    /// - If the next byte is 2, it adds 32 bits.
    /// - If the next byte is 3, its adds 64 bits.
    ///
    /// Format: `[Add] [register-group] [result-r3] [left-r1] [left-r2]`
    Add,
    /// Subtracts the value in a register from the value in another register
    /// and stores the result in a third.
    ///
    /// The schema is the same as [`Opcode::Add`].
    ///
    /// Format: `[Sub] [register-group] [result-r3] [minuend-r1] [subtrahend-r2]`
    Sub,
    /// Multiplies the value in a register by the value in another register and
    /// stores the result in a third.
    ///
    /// The schema is the same as [`Opcode::Add`].
    ///
    /// Format: `[Mul] [register-group] [result-r3] [left-r1] [right-r2]`
    Mul,
    /// Divides the value in a register by the value in another
    /// and stores the result in a third.
    ///
    /// The schema is the same as [`Opcode::Add`].
    ///
    /// Format: `[Div] [register-group] [result-r3] [numerator-r1] [denominator-r2]`
    Div,
    /// Performs a modulus operation on the value in a register using the
    /// value in another register and stores the result in a third.
    ///
    /// The schema is the same as [`Opcode::Add`].
    ///
    /// Format: `[Mod] [register-group] [result-r3] [numerator-r1] [denominator-r2]`
    Mod,
    /// Right shifts the value in a register by the value in another,
    /// and stores the result in a third.
    ///
    /// The schema is the same as [`Opcode::Add`].
    ///
    /// Format: `[RightShift] [register-group] [result-r3] [numerator-r1] [denominator-r2]`
    RightShift,
    /// Left shifts the value in a register by the value in another,
    /// and stores the result in a third.
    ///
    /// The schema is the same as [`Opcode::Add`].
    ///
    /// Format: `[LeftShift] [register-group] [result-r3] [numerator-r1] [denominator-r2]`
    LeftShift,

    // ================
    // EQUALITY OPCODES
    // ================
    /// Checks that the value in two int8 registers are equal and stores the result in
    /// a bool register.
    ///
    /// Format: `[EqInt8] [bool-destination] [first] [second]`
    EqInt8,
    /// Checks that the value in two int16 registers are equal and stores the result in
    /// a bool register.
    ///
    /// Format: `[EqInt16] [bool-destination] [first] [second]`
    EqInt16,
    /// Checks that the value in two float32 registers are equal and stores the result in
    /// a bool register.
    ///
    /// Format: `[EqFloat32] [bool-destination] [first] [second]`
    EqFloat32,
    /// Checks that the value in two float64 registers are equal and stores the result in
    /// a bool register.
    ///
    /// Format: `[EqFloat64] [bool-destination] [first] [second]`
    EqFloat64,
    /// Checks that the value in two boolean registers are equal and stores the result in
    /// another boolean register.
    ///
    /// Format: `[EqBool] [bool-destination] [first] [second]`
    EqBool,
    /// Checks that the value in two functionptr registers are equal and stores the result
    /// in a boolean register.
    ///
    /// Format: `[EqFunctionPtr] [bool-destination] [first] [second]`
    EqFunctionPtr,
    /// Checks that the value in two addr registers are equal and stores the result
    /// in a boolean register.
    ///
    /// Format: `[EqAddr] [bool-destination] [first] [second]`
    EqAddr,
    /// Negates the value in a boolean register.
    ///
    /// Format: `[Negate] [register]`
    Negate,

    // ===============
    // CONTROL OPCODES
    // ===============
    /// Jumps to an instruction index if the value stored in a bool register is true.
    /// The next 8 bytes correspond to the instruction index.
    ///
    /// Format: `[JumpIfTrue] [register] [index: [a, b, c, d, e, f, g, h]]`
    JumpIfTrue,
    /// Jumps to an instruction index based on the value in a bool register.
    /// The following 8 bytes correspond to the instruction index for truth.
    /// The next 8 bytes correspond to the instruction index for falsehood.
    ///
    /// Format: `[JumpConditional] [register] [true: [a, b, c, d, e, f, g, h]] [false: [i, j, k, l, m, n, o, p]]`
    JumpConditional,
    /// Predefines a loop.
    /// The first 8 bytes correspond to the starting instruction index.
    /// The second set of 8 bytes corresponds to the ending instruction index.
    /// The third set of 8 bytes correspond to the number of iterations in the loop.
    ///
    /// Format: `[LoopFor] [start: [a..h]] [end: [i..p]] [count: [q..x]]`
    LoopFor,
    /// Do nothing.
    /// It is useful for predefined loops over empty blocks.
    ///
    /// Format: `[Stall]`
    Stall,
    /// Breaks out of a predefined loop by setting the loopcounter to the last
    /// value in the loop.
    ///
    /// Format: `[BreakLoop]`
    BreakLoop,
    /// Moves the instruction pointer to a new index.
    /// The next 8 bytes correspond to the instruction index.
    ///
    /// Format: `[Goto] [destination: [a, b, c, d, e, f, g, h]]`
    Goto,
    /// Calls a function or a method using its id in a functionptr register.
    ///
    /// Format: `[Call] [register]`
    Call,
    /// Returns to the caller block of the current function and continues execution.
    ///
    /// Format: `[Return]`
    Return,

    // ===================
    // ALLOCATION OPCODES
    // ===================
    /// Creates a new instance on the heap, and stores a pointer to it in
    /// the _vala_ register.
    /// The next eight bytes correspond to the index of the layout to allocate in the table.
    ///
    /// Format: `[NewInstanceValueA] [type: [a, b, c, d, e, f, g, h]]`
    NewInstanceValueA,
    /// Loads the value in the addra register to the stack.
    /// The next four bytes correspond to the index in the stack frame.
    ///
    /// Format: `[LoadAddrAToFrame] [dest: [a, b, c, d]]`
    StoreValueAToFrame,
    /// Creates a new instance of a model on the heap, and stores a pointer to it in
    /// the _valb_ register.
    /// The next eight bytes correspond to the index of the layout to allocate in the table.
    ///
    /// Format: `[NewInstanceValueB] [type: [a, b, c, d, e, f, g, h]]`
    NewInstanceValueB,
    /// Creates a new array on the heap and stores a pointer to it in the addra register.
    ///
    /// Format: `[NewArrayAddr] [type: [a, b, c, d, e, f, g, h]]`
    NewArrayAddrA,
    /// Creates a new array on the heap and stores a pointer to it in the addrb register.
    ///
    /// Format: `[NewArrayEAddr] [type: [a, b, c, d, e, f, g, h]]`
    NewArrayAddrE,
    /// Gets the property beginning at an offset in an instance on the heap and loads it
    /// into the _vala_ register.
    ///
    /// The next four bytes correspond to the start of address of the heap pointer on the stack.
    /// The four bytes that follow correspond to the offset into the instance.
    ///
    /// Format: `[GetPropertyOffset] [stackidx: [a, b, c, d]] [offset: [e, f, g, h]]`
    GetPropertyOffset,
    /// Moves the value in the vala register to the ret register in preparation of a return.
    /// It moves the empty value if the vala register is empty.
    ///
    /// Format: `[MoveValtoRet]`
    MoveValtoRet,

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
    /// Invokes an intrinsic injunction. It uses the last five values on the stack to
    /// determine what to do.
    ///
    /// Format: `[Invoke]`
    Invoke,
    /// Exits the program.
    Exit,
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        match value {
            // 0 => Opcode::Exit,

            // 1 => Opcode::LoadInt8,
            // 2 => Opcode::LoadInt16,
            // 3 => Opcode::LoadFloat32,
            // 4 => Opcode::LoadFloat64,
            // // 5 => Opcode::LoadIacc8,
            // // 6 => Opcode::LoadIacc16,
            // // 7 => Opcode::LoadIacc32,
            // // 8 => Opcode::LoadIacc64,
            // // 9 => Opcode::LoadIframe,
            // // 10 => Opcode::LoadIret,
            // // 11 => Opcode::LoadIconstptra,
            // // 12 => Opcode::LoadIconstptrb,
            // // 25 => Opcode::Printacc8,
            // // 28 => Opcode::Printacc64,
            // // 29 => Opcode::Printframe,
            // // 30 => Opcode::Printconstptra,
            // // 31 => Opcode::Addacc,

            // // 35 => Opcode::Sqrtacc64,
            // // 42 => Opcode::Eqacc,
            // 45 => Opcode::JumpIfTrue,
            // 47 => Opcode::LoopFor,
            // 48 => Opcode::Stall,
            // // 51 => Opcode::CallNamedFunction,
            // 52 => Opcode::Return,
            // 55 => Opcode::NewInstanceValueA,
            // 56 => Opcode::StoreValueAToFrame,
            // // 57 => Opcode::StartBlock,
            // // 58 => Opcode::EndBlock,
            // 65 => Opcode::MoveValtoRet,
            _ => unimplemented!("{value}"),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        match value {
            Opcode::LoadInt8 => todo!(),
            Opcode::LoadInt16 => todo!(),
            Opcode::LoadFloat32 => todo!(),
            Opcode::LoadFloat64 => todo!(),
            Opcode::LoadBool => todo!(),
            Opcode::LoadFunctionPtr => 5,
            Opcode::MoveInt8 => todo!(),
            Opcode::MoveInt16 => todo!(),
            Opcode::MoveFloat32 => todo!(),
            Opcode::MoveFloat64 => todo!(),
            Opcode::MoveBool => todo!(),
            Opcode::MoveAddr => todo!(),
            Opcode::MoveEther => todo!(),
            Opcode::StoreInt8 => todo!(),
            Opcode::StoreInt16 => todo!(),
            Opcode::StoreFloat32 => todo!(),
            Opcode::StoreFloat64 => todo!(),
            Opcode::StoreBool => todo!(),
            Opcode::StoreFunctionPtr => todo!(),
            Opcode::StoreAddr => todo!(),
            Opcode::RetrieveInt8 => todo!(),
            Opcode::RetrieveInt16 => todo!(),
            Opcode::RetrieveFloat32 => todo!(),
            Opcode::RetrieveFloat64 => todo!(),
            Opcode::RetrieveBool => todo!(),
            Opcode::RetrieveFunctionPtr => todo!(),
            Opcode::RetrieveAddr => todo!(),
            Opcode::ReturnInt8 => todo!(),
            Opcode::ReturnInt16 => todo!(),
            Opcode::ReturnFloat32 => todo!(),
            Opcode::ReturnFloat64 => todo!(),
            Opcode::ReturnBool => todo!(),
            Opcode::ReturnFunctionPtr => todo!(),
            Opcode::ReturnAddr => todo!(),
            Opcode::ReturnEther => todo!(),
            Opcode::Add => todo!(),
            Opcode::Sub => todo!(),
            Opcode::Mul => todo!(),
            Opcode::Div => todo!(),
            Opcode::Mod => todo!(),
            Opcode::RightShift => todo!(),
            Opcode::LeftShift => todo!(),
            Opcode::EqInt8 => todo!(),
            Opcode::EqInt16 => todo!(),
            Opcode::EqFloat32 => todo!(),
            Opcode::EqFloat64 => todo!(),
            Opcode::EqBool => todo!(),
            Opcode::EqFunctionPtr => todo!(),
            Opcode::EqAddr => todo!(),
            Opcode::Negate => todo!(),
            Opcode::JumpIfTrue => todo!(),
            Opcode::JumpConditional => todo!(),
            Opcode::LoopFor => todo!(),
            Opcode::Stall => todo!(),
            Opcode::BreakLoop => todo!(),
            Opcode::Goto => todo!(),
            Opcode::Call => todo!(),
            Opcode::Return => todo!(),
            Opcode::NewInstanceValueA => todo!(),
            Opcode::StoreValueAToFrame => todo!(),
            Opcode::NewInstanceValueB => todo!(),
            Opcode::NewArrayAddrA => todo!(),
            Opcode::NewArrayAddrE => todo!(),
            Opcode::GetPropertyOffset => todo!(),
            Opcode::MoveValtoRet => todo!(),
            Opcode::SpawnSeq => todo!(),
            Opcode::SyncSeq => todo!(),
            Opcode::HaltSeq => todo!(),
            Opcode::Invoke => todo!(),
            Opcode::Exit => todo!(),
        }
    }
}
