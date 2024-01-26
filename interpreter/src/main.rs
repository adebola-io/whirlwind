mod statement;
mod value;

use std::collections::HashMap;
use value::Value;

use analyzer::{SemanticSymbolKind, Standpoint, SymbolIndex};
use statement::InterpeterStatement;

pub struct InterpreterStack {
    pub variables: Vec<HashMap<SymbolIndex, Value>>,
}

pub struct InterpreterHeap {}

impl InterpreterHeap {}

pub struct Interpreter {
    builtin_functions: Vec<fn(value: Value) -> Value>,
    stack: InterpreterStack,
    heap: InterpreterHeap,
}

pub enum InterpreterError {
    /// A function has overflown the stack.
    StackOverflow,
    /// No main() function is defined in the standpoint.
    NoMain,
    /// The main() function defined in the standpoint has parameters.
    ParameteredMain,
    /// The main() function defined in the standpoint is asynchronous.
    AsyncMain,
    /// The main() function defined in the standpoint has a return value.
    ReturnFromMain,
}

impl Interpreter {
    /// Executes a standpoint, beginning from the main() function.
    pub fn run(&mut self, standpoint: Standpoint) -> Result<i32, InterpreterError> {
        let main = standpoint.main().ok_or(InterpreterError::NoMain)?;
        let symbollib = &standpoint.symbol_library;
        let main_function_symbol = symbollib.get(main.name).ok_or(InterpreterError::NoMain)?;
        if let SemanticSymbolKind::Function {
            is_public,
            is_async,
            params,
            generic_params,
            return_type,
        } = &main_function_symbol.kind
        {
            if *is_async {
                return Err(InterpreterError::AsyncMain);
            }
            if return_type.is_some() {
                return Err(InterpreterError::ReturnFromMain);
            }
            if params.len() > 0 {
                return Err(InterpreterError::ParameteredMain);
            }
        }
        Ok(0)
    }
}

impl Interpreter {
    /// Executes an interpreter statement.
    pub fn run_statement(&mut self, statement: InterpeterStatement) {
        todo!()
    }
}

fn main() {
    println!("Hello, world!");
}
