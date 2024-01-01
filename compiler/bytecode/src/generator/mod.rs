use crate::{ConstantPool, FunctionRetriever, Opcode, PAD};
use analyzer::{
    utils::distill_as_function_type, EvaluatedType, Literal, PathIndex, ScopeId,
    SemanticSymbolKind, Standpoint, SymbolIndex, SymbolLibrary, TypedExpression,
    TypedFunctionDeclaration, TypedInterfaceProperty, TypedInterfacePropertyType,
    TypedModelProperty, TypedModelPropertyType, TypedStmnt,
};
use errors::BytecodeError;
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
};

mod callable;
mod expression;
mod memory;

pub use callable::*;
pub use memory::*;

/// The final object to be produced by the bytecode generator.
pub struct BytecodeObject {
    pub constants: ConstantPool,
    pub functions: Vec<CallablePtr>,
    pub layouts: Vec<Layout>,
    pub instructions: Vec<u8>,
}

/// A blueprint of how an instance of a model is layed out in (virtual) memory.
#[derive(Debug)]
pub struct Layout {
    pub property_offsets: Vec<u32>,
}

/// The bytecode generator is the penultimate pass in the Whirlwind compiler.
/// It transforms the tree-like structures from the standpoint
/// into a linear sequence of bytecode instructions to be run by the virtual
/// machine.
pub struct BytecodeGenerator<'standpoint> {
    /// The standpoint to traverse.
    standpoint: &'standpoint Standpoint,
    memory: BytecodeMemoryManager,
    /// Functions in the code.
    callables: Callables<'standpoint>,
    /// Constants in the code.
    constants: ConstantPool,
    /// Mutable queue of callable entities to generate bytecode for.
    queue: VecDeque<Callable<'standpoint>>,
}

pub enum FunctionType {
    NamedFunction,
    AnonymousFunction,
}

impl From<BytecodeGenerator<'_>> for BytecodeObject {
    fn from(value: BytecodeGenerator<'_>) -> Self {
        BytecodeObject {
            constants: value.constants,
            functions: value.callables.table.take(),
            instructions: value.memory.code,
            layouts: vec![],
        }
    }
}

impl<'a> BytecodeGenerator<'a> {
    pub fn from(standpoint: &'a Standpoint) -> Self {
        Self {
            standpoint,
            constants: ConstantPool::new(),
            callables: Callables {
                standpoint,
                lookup: RefCell::new(HashMap::new()),
                table: RefCell::new(vec![]),
            },
            memory: BytecodeMemoryManager::new(),
            queue: VecDeque::new(),
        }
    }

    /// Walks the execution context and generates bytecode.
    pub fn generate(mut self) -> Result<BytecodeObject, BytecodeError> {
        let main_function = self.standpoint.main().unwrap();

        let main_ptr_id = self.callables.get_or_create(main_function.name);
        let function_register = self.memory.load_immediate_function_ptr(main_ptr_id);
        self.memory.call_function_in(function_register);
        self.exit(); // call exit immediately main() ends.

        self.queue.push_back(Callable::NamedFunction(main_function));

        // Build the blocks pertaining to each function in the program.
        while !self.queue.is_empty() {
            let next_function = self.queue.pop_front().unwrap();
            self.emit_callable(next_function);
        }
        Ok(BytecodeObject::from(self))
    }

    /// Emits a callable as bytecode.
    fn emit_callable(&mut self, callable: Callable<'a>) {
        // The starting position of this callable is the current length of the bytecode.
        let start = self.memory.code.len();
        // get the callable index and the block of statements.
        let (symbol_idx, body) = match callable {
            Callable::NamedFunction(function) => (function.name, &function.body),
            Callable::NamedModelMethod(method) => match &method._type {
                TypedModelPropertyType::TypedMethod { body }
                | TypedModelPropertyType::InterfaceImpl { body, .. } => {
                    (method.name, body)
                }
                _ => unreachable!("Attempted to call an attribute while generating bytecode."),
            },
            Callable::NamedInterfaceMethod(interface) => match &interface._type {
                TypedInterfacePropertyType::Signature => unreachable!("Attempted to call an umimplemented interface method while generating bytecode."),
                TypedInterfacePropertyType::Method { body } => (interface.name, body),
            },
        };
        self.callables.set_start(symbol_idx, start);
        self.memory.create_new_frame();
        self.emit_block(body, true);
        // return if a return is not already specified.
        if !body
            .statements
            .last()
            .is_some_and(|stmnt| stmnt.is_return())
        {
            self.memory.code.push(Opcode::Return.into());
        }
        let end = self.memory.code.len() - 1;
        self.memory.clear_frame();
        self.callables.set_end(symbol_idx, end);
    }

    /// Emits a statement.
    fn emit_statement(&mut self, statement: &'a TypedStmnt) {
        match statement {
            TypedStmnt::RecordDeclaration => todo!(),
            TypedStmnt::TestDeclaration(_) => todo!(),
            TypedStmnt::EnumDeclaration(_) => todo!(),
            TypedStmnt::UseDeclaration(_) => todo!(),
            TypedStmnt::VariableDeclaration(_) => todo!(),
            TypedStmnt::ShorthandVariableDeclaration(_) => todo!(),
            TypedStmnt::ConstantDeclaration(_) => todo!(),
            TypedStmnt::TypeDeclaration(_) => todo!(),
            TypedStmnt::ModelDeclaration(_) => todo!(),
            TypedStmnt::ModuleDeclaration(_) => todo!(),
            TypedStmnt::FunctionDeclaration(function) => {
                self.emit_callable(Callable::NamedFunction(function))
            }
            TypedStmnt::InterfaceDeclaration(_) => todo!(),
            TypedStmnt::ExpressionStatement(expression)
            | TypedStmnt::FreeExpression(expression) => {
                if let Some(register_id) = self.emit_expression(expression) {
                    self.memory.clear_register(register_id);
                }
            }
            TypedStmnt::ReturnStatement(retty) => self.emit_return(retty),
            TypedStmnt::BreakStatement(_) => todo!(),
            TypedStmnt::ForStatement(_) => todo!(),
            TypedStmnt::WhileStatement(_) => todo!(),
            TypedStmnt::ContinueStatement(_) => todo!(),
        }
    }

    /// Exits the program.
    fn exit(&mut self) {
        self.memory.code.push(Opcode::Exit.into())
    }

    /// Emits a block of code.
    fn emit_block(
        &mut self,
        body: &'a analyzer::TypedBlock,
        is_function_block: bool,
    ) -> Option<RegisterId> {
        for (i, statement) in body.statements.iter().enumerate() {
            // Handle returns.
            match (i + 1 == body.statements.len(), statement, is_function_block) {
                (true, TypedStmnt::FreeExpression(free_expr), false) => {
                    return self.emit_expression(&free_expr);
                }
                _ => self.emit_statement(&statement),
            }
        }
        return None;
    }

    /// Emits a return statement.
    ///
    /// It evaluates the return value and moves the value into the return register.
    fn emit_return(&mut self, retty: &'a analyzer::TypedReturnStatement) {
        if let Some(expression) = &retty.value {
            self.emit_expression(expression);
            self.memory.code.push(Opcode::MoveValtoRet.into())
        }
        self.memory.code.push(Opcode::Return.into())
    }

    /// Returns the nature of the caller in the call expression.
    fn get_caller_nature(&mut self, caller: &TypedExpression) -> CallerNature {
        if caller.is_identifier() {
            let literals = &self.standpoint.literals;
            let library = &self.standpoint.symbol_library;
            let type_of_callable = library.get_expression_type(caller, literals).unwrap();
            match type_of_callable {
                EvaluatedType::FunctionInstance { function, .. } => {
                    return CallerNature::NamedFunction(function)
                }
                EvaluatedType::MethodInstance { method, .. } => {
                    return CallerNature::Method(method)
                }
                EvaluatedType::FunctionExpressionInstance { .. } => todo!(),
                _ => unreachable!("Could not construct nature on non-callable value."),
            }
        }
        todo!()
    }
}

pub fn print_instructions(instructions: &[u8]) {
    for (index, byte) in instructions.iter().enumerate() {
        if index % 10 == 0 {
            print!("\n")
        }
        print!("{:02X} ", byte);
    }
    print!("\n\n");
}
