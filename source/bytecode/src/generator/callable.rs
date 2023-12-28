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
    pub start: usize,
    /// The number of times the function/method has been called.
    pub calls: usize,
}

pub struct Callables<'standpoint> {
    /// The standpoint to traverse.
    pub standpoint: &'standpoint Standpoint,
    /// Maps a function's symbol index to its index in the table.
    pub lookup: RefCell<HashMap<SymbolIndex, usize>>,
    /// Function dispatch table.
    pub table: RefCell<Vec<CallablePtr>>,
}

impl<'standpoint> Callables<'standpoint> {
    /// Returns the index of a symbol in the function registry,
    /// or adds it if it doesn't exist.
    pub fn get_or_create(&self, symbol: SymbolIndex) -> usize {
        let mut lookup = self.lookup.borrow_mut();
        match lookup.get(&symbol) {
            Some(index) => return *index,
            None => {
                let mut function_table = self.table.borrow_mut();
                let functionptr = self.construct_callableptr(symbol);
                let index = function_table.len();
                function_table.push(functionptr);
                lookup.insert(symbol, index);
                index
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
        let symbol = standpoint.symbol_library.get(symbol).unwrap();
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
            start: 0, // to be determined when the function is actually reached.
            calls: 0,
        }
    }

    /// Creates the main() function.
    pub fn main(&mut self, main_symbol: SymbolIndex) -> usize {
        let mut function_map = self.lookup.borrow_mut();
        let mut list = self.table.borrow_mut();

        assert!(list.is_empty());

        function_map.insert(main_symbol, 0);
        list.push(CallablePtr::main());
        0
    }

    /// Creates or gets a function index while setting its starting instruction idx.
    pub fn set_start(&mut self, name: SymbolIndex, start: usize) -> usize {
        let idx_in_function_list = self.get_or_create(name);
        let mut function_list = self.table.borrow_mut();
        function_list[idx_in_function_list].start = start;
        return idx_in_function_list;
    }
}
