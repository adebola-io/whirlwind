use std::path::Path;
mod expr;
use crate::{Span, WhirlNumber, WhirlString};

/// An index into the paths stored.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct PathIndex(pub usize);

/// An index into the list of literals in the program.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct LiteralIndex(pub usize);

/// An index into the symbol table.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct SymbolIndex(pub usize);

/// An identifier, basically. Contains an index to the symbol name, and a reference number.
#[derive(Debug, PartialEq)]
pub struct SymbolLocator {
    pub symbol_index: SymbolIndex,
    pub reference_number: usize,
}

#[derive(Debug, Default)]
pub enum SymbolEntry {
    #[default]
    Removed,
    Symbol(SemanticSymbol),
}

/// A symbol in the context of a fully resolved program.
#[derive(Debug)]
pub struct SemanticSymbol {
    pub name: String,
    pub symbol_type: SemanticSymbolKind,
    pub references: Vec<SemanticSymbolReferenceList>,
    pub doc_info_range: Option<Span>,
    pub declaration_range: Span,
}

impl SemanticSymbol {
    /// Add a reference to this symbol.
    pub fn add_reference(&mut self, module_path: PathIndex, span: Span) {
        match self
            .references
            .iter_mut()
            .find(|list| list.module_path == module_path)
        {
            Some(list) => {
                list.starts.push(span.start);
            }
            None => self.references.push(SemanticSymbolReferenceList {
                module_path,
                starts: vec![span.start],
            }),
        }
    }
}

/// A collection of all instances of a symbol inside a file.
#[derive(Debug)]
pub struct SemanticSymbolReferenceList {
    /// Path to file where the references exist.
    pub module_path: PathIndex,
    /// List of the starting position for each reference.
    pub starts: Vec<[u32; 2]>,
}

#[derive(Debug)]
/// a reference to a symbol.
pub struct SemanticSymbolReference<'a> {
    pub module_path: &'a Path,
    pub start_position: [u32; 2],
}

/// The container declaration of a symbol.
/// It corresponds with the first reference to a symbol.
/// It contains the path to the module where it is declared,
/// and the range of the entire declaration.
pub struct SemanticSymbolDeclaration<'a> {
    pub module_path: &'a Path,
    pub span: &'a Span,
}

#[derive(Debug)]
pub enum SemanticSymbolKind {
    Module {
        imports: Vec<SymbolIndex>,
        exports: Vec<SymbolIndex>,
    },
    Trait {
        implementations: Vec<SymbolIndex>,
        methods: Vec<SymbolIndex>,
    },
    Model {
        implementations: Vec<SymbolIndex>,
        methods: Vec<SymbolIndex>,
        attributes: Vec<SymbolIndex>,
    },
    Enum {
        is_public: bool,
        variants: Vec<SymbolIndex>,
    },
    /// Variant of an enum declaration.
    Variant {
        owner_enum: SymbolIndex,
        variant_index: usize,
        tagged_type: Option<IntermediateType>,
    },
    /// An instance created with `new A()`.
    Variable {
        is_public: bool,
        declared_type: Option<IntermediateType>,
    },
    /// A method of a trait or model.
    MethodOf {
        is_public: bool,
        is_static: bool,
        is_async: bool,
        owner_model_or_trait: SymbolIndex,
        property_index: usize,
        params: Vec<SymbolIndex>,
        generic_arguments: Vec<SymbolIndex>,
    },
    /// Parameter of a function.
    Parameter {
        is_optional: bool,
        owner_function: SymbolIndex,
        param_type: IntermediateType,
    },
    GenericParameter {
        owner_signature: SymbolIndex,
        traits: Vec<SymbolIndex>,
        default_value: SymbolIndex,
    },
    Function {
        is_public: bool,
        is_async: bool,
        params: Vec<SymbolIndex>,
        generic_arguments: Vec<SymbolIndex>,
        return_type: Vec<IntermediateType>,
    },
    TypeName {
        is_public: bool,
        generic_arguments: Vec<SymbolIndex>,
        value: Vec<IntermediateType>,
    },
    This {
        refers_to: SymbolIndex,
    },
    UndeclaredValue,
}

#[derive(Debug)]
pub enum IntermediateType {
    FunctionType {
        params: Vec<ParameterType>,
        return_type: Box<IntermediateType>,
        span: Span,
    },
    SimpleType {
        value: SymbolIndex,
        generic_arguments: Vec<SymbolIndex>,
        span: Span,
    },
    UnionType {
        types: Vec<SymbolIndex>,
        span: Span,
    },
}

#[derive(Debug)]
pub struct ParameterType {
    pub name: String,
    pub is_public: bool,
    pub type_label: Option<SymbolIndex>,
}

#[derive(Debug)]
pub enum Literal {
    StringLiteral {
        module: PathIndex,
        value: WhirlString,
    },
    NumericLiteral {
        module: PathIndex,
        value: WhirlNumber,
    },
    BooleanLiteral {
        value: bool,
        start_line: u32,
        start_character: u32,
    },
}

/// The symbol table for the whole project.
#[derive(Debug)]
pub struct SymbolTable {
    holes: Vec<usize>,
    symbols: Vec<SymbolEntry>,
}

impl SymbolTable {
    /// Add a symbol to the table and return its index number.
    pub fn add_symbol(&mut self, symbol: SemanticSymbol) -> usize {
        // Fill any holes by removed symbols.
        let index = match self.holes.pop() {
            Some(void_idx) => {
                self.symbols[void_idx] = SymbolEntry::Symbol(symbol);
                void_idx
            }
            None => {
                let id = self.symbols.len();
                self.symbols.push(SymbolEntry::Symbol(symbol));
                id
            }
        };
        return index;
    }
    /// Get a symbol using its index.
    pub fn get_symbol(&self, index: usize) -> Option<&SemanticSymbol> {
        match self.symbols.get(index)? {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
    /// Get a symbol mutably using its index.
    pub fn get_symbol_mut(&mut self, index: usize) -> Option<&mut SemanticSymbol> {
        match self.symbols.get_mut(index)? {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
    /// Remove a symbol using its index.
    pub fn remove_symbol(&mut self, index: usize) -> Option<SemanticSymbol> {
        let symbolentry = std::mem::take(self.symbols.get_mut(index)?);
        self.holes.push(index);
        match symbolentry {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
}

// Bind all symbols in the graph to their original declaration.
// pub fn bind_symbols(mut graph: ModuleGraph) -> BoundModuleGraph {}
