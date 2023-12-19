pub enum Value {
    StackPointer(usize),
    Numeric(u64),
}

pub struct RegisterIdx(pub u8);
pub struct FunctionPtr(pub usize);

pub enum Instruction {
    PrintAcc,
    SqrtAcc,
    LoadReg { to: RegisterIdx, value: Value },
    LoadConst { const_idx: usize },
    LoadAcc { from: RegisterIdx },
    MovAcc { to: RegisterIdx },
    AddAcc { rg: RegisterIdx },
    SubAcc { rg: RegisterIdx },
    MulAcc { rg: RegisterIdx },
    DivAcc { rg: RegisterIdx },
    ModAcc { rg: RegisterIdx },
    DecAcc { rg: RegisterIdx },
    IncrAcc { rg: RegisterIdx },
    RShiftAcc { rg: RegisterIdx },
    LShiftAcc { rg: RegisterIdx },
    EqAcc { rg: RegisterIdx },
    NeAcc { rg: RegisterIdx },
    IsAcc { rg: RegisterIdx },
    JumpIfTrue { idx: usize },
    JumpConditional { consequent: usize, alternate: usize },
    Goto { instruction_idx: usize },
    Call { funcidx: FunctionPtr },
    LoadArg,
    Return { instruction_idx: usize },
    NewModel { model_type: usize },
    NewArr,
    StoreAcc { stack_idx: usize },
}
