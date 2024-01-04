use std::{cell::RefCell, collections::HashMap};

use analyzer::{
    SemanticSymbolKind, Standpoint, SymbolIndex, TypedFunctionDeclaration, TypedInterfaceProperty,
    TypedModelProperty,
};

use crate::StackAddress;

/// The index of a variable in the vtable.
pub struct VTableId(pub usize);

/// The type of the caller in a call expression.
pub enum CallerNature {
    /// A function created normally.
    NamedFunction(SymbolIndex),
    /// A function created from a function expression.
    /// The stack address is the address of the caller variable.
    AnonymousFunction(StackAddress),
    /// A method on an opaque type, which will be resolved at runtime.
    VirtualMethod {
        owner_opaque_value: StackAddress,
        method_id: VTableId,
    },
    /// A method on an interface or model.
    Method(SymbolIndex),
}
/// An entity that can be called in the bytecode.
pub enum Callable<'standpoint> {
    NamedFunction(&'standpoint TypedFunctionDeclaration),
    NamedModelMethod(&'standpoint TypedModelProperty),
    NamedInterfaceMethod(&'standpoint TypedInterfaceProperty),
}

impl CallablePtr {
    pub fn main() -> Self {
        Self {
            name: String::from("main"),
            param_count: 0,
            start: 1,
            end: 0,
            calls: 0,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CallablePtr {
    /// Name of the callable. For methods it is `Model.methodName`.
    pub name: String,
    /// The number of parameters in the function/method call.
    pub param_count: usize,
    /// The index of the first byte or instruction.
    /// It is determined duting emission.
    pub start: usize,
    /// The index of the last instruction in the program.
    /// It is determined during emission.
    pub end: usize,
    /// The number of times the function/method has been called.
    pub calls: usize,
}

/// The index of a callable in the function dispatch table.
#[derive(Debug, Clone, Copy)]
pub struct CallablePtrId(pub u32);

pub struct Callables<'standpoint> {
    /// The standpoint to traverse.
    pub standpoint: &'standpoint Standpoint,
    /// Maps a function's symbol index to its index in the table.
    pub lookup: RefCell<HashMap<SymbolIndex, CallablePtrId>>,
    /// Function dispatch table.
    pub table: RefCell<Vec<CallablePtr>>,
}

impl<'standpoint> Callables<'standpoint> {
    /// Returns the identifier index of a symbol in the function registry,
    /// or adds it if it doesn't exist.
    pub fn get_or_create(&self, symbol: SymbolIndex) -> CallablePtrId {
        let mut lookup = self.lookup.borrow_mut();
        match lookup.get(&symbol) {
            Some(index) => return *index,
            None => {
                let mut function_table = self.table.borrow_mut();
                let functionptr = self.construct_callableptr(symbol);
                let index = function_table.len();
                function_table.push(functionptr);
                let id = CallablePtrId(index as u32);
                lookup.insert(symbol, id);
                id
            }
        }
    }
    /// Returns true if a function has been added to the dispatch table.
    pub fn has(&self, function_symbol: SymbolIndex) -> bool {
        self.lookup.borrow().get(&function_symbol).is_some()
    }
    /// Constructs a callable pointer to store in the dispatch table,
    /// based on the symbol signature
    pub fn construct_callableptr(&self, symbol: SymbolIndex) -> CallablePtr {
        let standpoint = self.standpoint;
        let symbol = standpoint.symbol_library.get_forwarded(symbol).unwrap();
        let (name, param_count) = match &symbol.kind {
            SemanticSymbolKind::Method {
                owner_model_or_interface,
                params,
                ..
            } => {
                let owner = standpoint
                    .symbol_library
                    .get(*owner_model_or_interface)
                    .unwrap();
                (format!("{}.{}", owner.name, symbol.name), params.len())
            }
            SemanticSymbolKind::Function { params, .. } => {
                (format!("{}", symbol.name), params.len())
            }
            _ => unreachable!(),
        };
        CallablePtr {
            name,
            param_count,
            start: 0, // tbd
            end: 0,   // tbd
            calls: 0,
        }
    }

    /// Creates the main() function.
    pub fn main(&mut self, main_symbol: SymbolIndex) -> CallablePtrId {
        let mut function_map = self.lookup.borrow_mut();
        let mut list = self.table.borrow_mut();

        assert!(list.is_empty());

        function_map.insert(main_symbol, CallablePtrId(0));
        list.push(CallablePtr::main());
        CallablePtrId(0)
    }

    /// Sets its starting instruction idx (index of the first instruction) in a callable.
    pub fn set_start(&mut self, name: SymbolIndex, start: usize) {
        let idx_in_function_list = self.get_or_create(name);
        let mut function_list = self.table.borrow_mut();
        function_list[idx_in_function_list.0 as usize].start = start;
    }

    /// Sets the end instruction idx (index of the last instruction, usually return) in a callable.
    pub fn set_end(&mut self, name: SymbolIndex, end: usize) {
        let idx_in_function_list = self.get_or_create(name);
        let mut function_list = self.table.borrow_mut();
        function_list[idx_in_function_list.0 as usize].end = end;
    }
}
