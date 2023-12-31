use std::collections::{HashMap, VecDeque};

use analyzer::{EvaluatedType, SymbolIndex, SymbolLibrary};
use ast::BinOperator;

use crate::{CallablePtrId, Opcode, MAX_REGISTER_COUNT, PAD};

/// Responsible for aportioning and managing registers and memory during code generation,
/// and storing the generated code.
/// It allows me to operate on the expression nodes with the lower level details of register
/// allocation and reuse abstracted out.
pub struct BytecodeMemoryManager {
    /// The generated bytecode.
    pub code: Vec<u8>,
    /// A shadow stack of frames for storing and retrieving variables.
    frame_stack: Vec<BytecodeCallFrame>,
    /// The attributor stores property offsets in linear form before they can be moved to
    /// the Addr registers.
    /// For example, for a model:
    /// ```wrl
    /// model Data {
    ///     var age: UInt8;
    ///     var name: String;
    /// }
    /// model Person {
    ///     var data: Data;
    /// }
    /// ```
    /// person.data would have an attribution value of (stackAddress, [0]), The first value
    /// in the `person` instance, then person.data.age would have the attribution value of
    /// (stackAddress, [0, 0]), the first value in the first value of the person instance.
    attributor: Option<(StackAddress, Vec<u32>)>,
}

#[derive(Debug)]
/// A representation of the registers in the virtual machine for easy memory management.
pub struct Register {
    id: RegisterId,
    state: RegisterState,
}

/// Identifier marking a register.
#[derive(Debug, Clone, Copy)]
pub struct RegisterId(pub u8, pub RegisterGroup);

/// The index of a variable on the current stack frame.
#[derive(Debug, Clone, Copy)]
pub struct StackAddress(pub u32);

pub struct BytecodeCallFrame {
    /// Each group corresponds to a data type in the runtime.
    /// The possible data types are:
    /// - Int8
    /// - Int16
    /// - Float32
    /// - Float64
    /// - Address of a value on the heap.
    /// - Index of a constant.
    /// Each group has a maximum of 255 registers to track
    /// a sufficient number of live expressions in each function.
    register_groups: HashMap<RegisterGroup, Vec<Register>>,
    /// The address of stack values in the program.
    addresses: HashMap<SymbolIndex, StackAddress>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum RegisterGroup {
    /// For storing UInt8 values.
    Int8,
    /// For storing UInt64 values.
    Int16,
    /// For storing Float32 values.
    F32,
    /// For storing Float64 values.
    F64,
    /// For storing boolean values.
    Bool,
    /// For storing callable pointers.
    FunctionPtr,
    /// For storing references to instances of models, enum and opaque values.
    Addr,
    /// The set of registers storing "never" values.
    Ether,
}
impl RegisterGroup {
    /// Returns the register group that is distinct to an inferred type,
    /// according to the symbol library.
    pub fn of(inferred_type: &EvaluatedType, symbollib: &SymbolLibrary) -> RegisterGroup {
        todo!()
    }
}

#[derive(Debug, Default)]
/// Current state of the register in relation to the flow of control in the program.
pub enum RegisterState {
    #[default]
    Empty,
    Occupied,
}

impl BytecodeMemoryManager {
    /// Creates a new bytecode memory manager.
    pub fn new() -> Self {
        Self {
            code: vec![PAD],
            frame_stack: vec![BytecodeCallFrame::new()],
            attributor: None,
        }
    }
    /// Returns the address of a value currently stored on the frame.
    pub fn get_address_of(&self, value: SymbolIndex) -> StackAddress {
        todo!()
    }
    /// Stores a value in the stack frame.
    pub fn store_variable(&self, value: SymbolIndex) -> StackAddress {
        todo!()
    }
    /// Load an immediate numeric value into a register.
    /// It returns the id of whatever register was loaded.
    pub fn load_immediate_number(
        &mut self,
        group: RegisterGroup,
        value: &ast::Number,
    ) -> RegisterId {
        let mut register = self.get_free_register(group);
        if register.is_none() {
            register = Some(self.create_register(group));
        }
        let register = register.unwrap();
        register.state = RegisterState::Occupied;
        let id = register.id;
        let number = match value {
            ast::Number::Binary(data)
            | ast::Number::Octal(data)
            | ast::Number::Hexadecimal(data)
            | ast::Number::Decimal(data) => data
                .parse::<f64>()
                .expect("Bytecode gen failed due to numeric conversion error."),
            ast::Number::None => unreachable!(),
        };
        // code gen.
        let base_opcode = match group {
            RegisterGroup::Int8 => Opcode::LoadInt8,
            RegisterGroup::Int16 => Opcode::LoadInt16,
            RegisterGroup::F32 => Opcode::LoadFloat32,
            RegisterGroup::F64 => Opcode::LoadFloat64,
            _ => unreachable!(
                "Bytecode generation failed. Cannot load number value into invalid register."
            ),
        };
        self.code.push(base_opcode.into());
        match group {
            RegisterGroup::Int8 => self.code.push(number as u8),
            RegisterGroup::Int16 => (number as u16)
                .to_be_bytes()
                .into_iter()
                .for_each(|byte| self.code.push(byte)),
            RegisterGroup::F32 => (number as f32)
                .to_be_bytes()
                .into_iter()
                .for_each(|byte| self.code.push(byte)),
            RegisterGroup::F64 => (number as f64)
                .to_be_bytes()
                .into_iter()
                .for_each(|byte| self.code.push(byte)),
            _ => {}
        };
        return id;
    }

    /// Creates a new register in a group.
    ///
    /// It panics if the group is already full.
    pub fn create_register(&mut self, group: RegisterGroup) -> &mut Register {
        let registers = self
            .frame_stack
            .last_mut()
            .expect("No frame in memory.")
            .register_groups
            .get_mut(&group)
            .unwrap();
        let len = registers.len();
        if len == MAX_REGISTER_COUNT as usize {
            panic!("Register allocation failed. Maximum register count for free values exceeded.");
        }
        let id = RegisterId(len as u8, group);
        println!("created register %r{len} in group {group:?}");
        let new_register = Register {
            id,
            state: RegisterState::Empty,
        };
        registers.push(new_register);
        registers.last_mut().unwrap()
    }

    /// Returns a mutable reference to a free register from the register pool, if it exists.
    pub fn get_free_register(&mut self, group: RegisterGroup) -> Option<&mut Register> {
        let registers = self
            .frame_stack
            .last_mut()
            .expect("No frame in memory.")
            .register_groups
            .get_mut(&group)
            .unwrap();
        registers.iter_mut().find(|register| {
            if register.state.is_empty() {
                println!(
                    "reusing existing register %r{} in group {:?}",
                    register.id.0, register.id.1
                );
                return true;
            }
            return false;
        })
    }

    /// Load an immediate boolean value into a register.
    /// Returns the id of the loaded register.
    pub fn load_immediate_boolean(&mut self, value: bool) -> RegisterId {
        let mut register = self.get_free_register(RegisterGroup::Bool);
        if register.is_none() {
            register = Some(self.create_register(RegisterGroup::Bool));
        }
        let register = register.unwrap();
        register.state = RegisterState::Occupied;
        let id = register.id;

        // code gen.
        self.code.push(Opcode::LoadBool.into());
        self.code.push(if value { 1 } else { 0 });
        return id;
    }

    /// Loads a value into the functionptr register.
    /// Returns the id of the loaded register.
    pub fn load_immediate_function_ptr(&mut self, value: CallablePtrId) -> RegisterId {
        let mut register = self.get_free_register(RegisterGroup::FunctionPtr);
        if register.is_none() {
            register = Some(self.create_register(RegisterGroup::FunctionPtr));
        }
        let register = register.unwrap();
        register.state = RegisterState::Occupied;
        let id = register.id;

        // code gen.
        self.code.push(Opcode::LoadFunctionPtr.into());
        value
            .0
            .to_be_bytes()
            .into_iter()
            .for_each(|byte| self.code.push(byte));
        return id;
    }
    /// Generates the code to call a function in a given function register id.
    pub fn call_function_in(&mut self, register_id: RegisterId) {
        let register = self.get_register_mut(&(RegisterGroup::FunctionPtr, register_id));
        register.state.clear();

        // code gen.
        self.code.push(Opcode::Call.into());
        self.code.push(register_id.0);
    }

    /// Generate bytecode to move the value in a register of a
    /// register group to a stack address.
    pub fn store(&mut self, from: (RegisterGroup, RegisterId), to: StackAddress) {
        let register = self.get_register_mut(&from);
        register.state.clear();
        // code gen.
        let base_opcode = match from.0 {
            RegisterGroup::Int8 => Opcode::StoreInt8,
            RegisterGroup::Int16 => Opcode::StoreInt16,
            RegisterGroup::F32 => Opcode::StoreFloat32,
            RegisterGroup::F64 => Opcode::StoreFloat64,
            RegisterGroup::Bool => Opcode::StoreBool,
            RegisterGroup::FunctionPtr => Opcode::StoreFunctionPtr,
            RegisterGroup::Addr => Opcode::StoreAddr,
            RegisterGroup::Ether => panic!("Ether value must be moved to addr register first."),
        };
        let stack_address = to.0.to_be_bytes();
        self.code.push(base_opcode.into());
        stack_address
            .into_iter()
            .for_each(|byte| self.code.push(byte));
        self.code.push(from.1 .0);
    }
    /// Generates bytecode to retrieve a value in a stack address to a register.
    ///
    /// It returns the id of the register where the value was stored.
    pub fn retrieve(&mut self, from: StackAddress, group: RegisterGroup) -> RegisterId {
        let mut register = self.get_free_register(group);
        if register.is_none() {
            register = Some(self.create_register(group));
        }
        let register = register.unwrap();
        register.state = RegisterState::Occupied;
        let id = register.id;

        // code gen.
        let base_opcode = match group {
            RegisterGroup::Int8 => Opcode::RetrieveInt8,
            RegisterGroup::Int16 => Opcode::RetrieveInt16,
            RegisterGroup::F32 => Opcode::RetrieveFloat32,
            RegisterGroup::F64 => Opcode::RetrieveFloat64,
            RegisterGroup::Bool => Opcode::RetrieveBool,
            RegisterGroup::FunctionPtr => Opcode::RetrieveFunctionPtr,
            RegisterGroup::Addr => Opcode::RetrieveAddr,
            _ => unreachable!("Cannot retrieve into an abstract register."),
        };
        self.code.push(id.0); // destination register.
        let stack_address = from.0.to_be_bytes();
        stack_address
            .into_iter()
            .for_each(|byte| self.code.push(byte)); // source address.
        return id;
    }

    pub fn get_register_mut(&mut self, from: &(RegisterGroup, RegisterId)) -> &mut Register {
        &mut self
            .frame_stack
            .last_mut()
            .unwrap()
            .register_groups
            .get_mut(&from.0)
            .expect("Could not find group for data type")[from.1 .0 as usize]
    }

    pub fn clear_register(&mut self, register: (RegisterGroup, RegisterId)) {
        let register = self.get_register_mut(&register);
        register.state = RegisterState::Empty;
    }

    /// Opens a new frame on the stack.
    pub fn create_new_frame(&mut self) {
        self.frame_stack.push(BytecodeCallFrame::new());
    }

    /// Clears the current frame on the stack.
    pub fn clear_frame(&mut self) {
        self.frame_stack.pop();
    }

    /// Generates the code to move the value in the global return register
    /// into the register of a given type and returns the register id of the value.
    pub fn get_return_value(&mut self, group: RegisterGroup) -> RegisterId {
        let mut new_register = self.get_free_register(group);
        if new_register.is_none() {
            new_register = Some(self.create_register(group));
        }
        let new_register = new_register.unwrap();
        new_register.state = RegisterState::Occupied;
        let id = new_register.id;

        // code gen.
        let base_opcode = match group {
            RegisterGroup::Int8 => Opcode::ReturnInt8,
            RegisterGroup::Int16 => Opcode::ReturnInt16,
            RegisterGroup::F32 => Opcode::ReturnFloat32,
            RegisterGroup::F64 => Opcode::ReturnFloat64,
            RegisterGroup::Bool => Opcode::ReturnBool,
            RegisterGroup::FunctionPtr => Opcode::ReturnFunctionPtr,
            RegisterGroup::Addr => Opcode::ReturnAddr,
            RegisterGroup::Ether => Opcode::ReturnEther,
        };
        self.code.push(id.0);

        return id;
    }
}

impl BytecodeCallFrame {
    pub fn new() -> Self {
        let mut register_groups = HashMap::new();
        for group in [
            RegisterGroup::Addr,
            RegisterGroup::Bool,
            RegisterGroup::Ether,
            RegisterGroup::F32,
            RegisterGroup::F64,
            RegisterGroup::F64,
            RegisterGroup::FunctionPtr,
            RegisterGroup::Int16,
            RegisterGroup::Int8,
        ] {
            register_groups.insert(group, Vec::with_capacity(255));
        }
        Self {
            register_groups,
            addresses: HashMap::new(),
        }
    }
}

impl RegisterState {
    /// Returns `true` if the bytecode register state is [`Empty`].
    ///
    /// [`Empty`]: BytecodeRegisterState::Empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    /// Returns `true` if the bytecode register state is [`Occupied`].
    ///
    /// [`Occupied`]: BytecodeRegisterState::Occupied
    #[must_use]
    pub fn is_occupied(&self) -> bool {
        matches!(self, Self::Occupied)
    }
    /// Empties the register state.
    fn clear(&mut self) {
        *self = Self::Empty
    }
}
