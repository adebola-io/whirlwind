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
        self.memory.clear_frame();
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
                let register = self.emit_expression(expression);
                let (literals, symbollib, _) = self.standpoint.data();
                let inferred_type = symbollib.get_expression_type(expression, literals).unwrap();
                let registertype = RegisterGroup::of(&inferred_type, symbollib);
                self.memory.clear_register((registertype, register));
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
    ) -> RegisterId {
        for (i, statement) in body.statements.iter().enumerate() {
            // Handle returns.
            match (i + 1 == body.statements.len(), statement, is_function_block) {
                (true, TypedStmnt::FreeExpression(free_expr), false) => {
                    return self.emit_expression(&free_expr);
                }
                _ => self.emit_statement(&statement),
            }
        }
        todo!()
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

    /// Emits an expression as bytecode.
    /// It returns the id of the register that contains the result
    /// of the expression evaluation.
    fn emit_expression(&mut self, expression: &'a TypedExpression) -> RegisterId {
        match expression {
            TypedExpression::Identifier(id) => todo!(),
            TypedExpression::Literal(literal) => {
                let literalmap = &self.standpoint.literals;
                let literal_value = literalmap.get(*literal).unwrap();
                match literal_value {
                    Literal::StringLiteral { module, value } => todo!(),
                    Literal::NumericLiteral {
                        value,
                        inferred_type,
                        ..
                    } => {
                        let symbollib = &self.standpoint.symbol_library;
                        let register_group = RegisterGroup::of(inferred_type, symbollib);
                        self.memory
                            .load_immediate_number(register_group, &value.value)
                    }
                    Literal::BooleanLiteral { value, .. } => {
                        self.memory.load_immediate_boolean(*value)
                    }
                }
            }
            TypedExpression::NewExpr(_) => todo!(),
            TypedExpression::ThisExpr(_) => todo!(),
            TypedExpression::CallExpr(call) => self.emit_call_expression(call),
            TypedExpression::FnExpr(_) => todo!(),
            TypedExpression::Block(block) => self.emit_block(block, false),
            TypedExpression::IfExpr(_) => todo!(),
            TypedExpression::AccessExpr(_) => todo!(),
            TypedExpression::ArrayExpr(_) => todo!(),
            TypedExpression::IndexExpr(_) => todo!(),
            TypedExpression::BinaryExpr(binexp) => self.emit_binary_expression(binexp),
            TypedExpression::AssignmentExpr(_) => todo!(),
            TypedExpression::UnaryExpr(_) => todo!(),
            TypedExpression::LogicExpr(_) => todo!(),
            TypedExpression::UpdateExpr(_) => todo!(),
        }
    }

    /// Emits a call expression.
    ///
    /// It loads the parameters to the stack from left to right,
    /// then calls the function.
    fn emit_call_expression(&mut self, call: &'a analyzer::TypedCallExpr) -> RegisterId {
        // todo: load parameters.
        // todo: load optional parameters.
        let register_id = self.emit_expression(&call.caller);
        assert_eq!(register_id.1, RegisterGroup::FunctionPtr);
        // Adds the call instruction to the bytecode.
        self.memory.call_function_in(register_id);
        let (literals, symbollib, _) = self.standpoint.data();
        let caller_type = symbollib
            .get_expression_type(&call.caller, literals)
            .unwrap();
        let return_type = distill_as_function_type(&caller_type, symbollib)
            .unwrap()
            .return_type;
        let group = RegisterGroup::of(&return_type, symbollib);
        self.memory.get_return_value(group)
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

    /// Emits a binary expression.
    fn emit_binary_expression(&mut self, binexp: &'a analyzer::TypedBinExpr) -> RegisterId {
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
