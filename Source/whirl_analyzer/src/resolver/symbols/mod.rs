use std::path::Path;
mod typed_expr;
mod typed_module;
mod typed_statement;

use whirl_ast::{ConstantSignature, Span, TypeSignature, WhirlNumber, WhirlString};

pub use typed_expr::*;
pub use typed_module::*;
pub use typed_statement::*;

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
    pub symbol_idx: SymbolIndex,
    pub ref_number: usize,
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
    pub symbol_kind: SemanticSymbolKind,
    pub references: Vec<SemanticSymbolReferenceList>,
    pub doc_info_range: Option<Span>,
    pub origin_span: Span,
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
        parent_modules: Vec<SymbolIndex>,
        imports: Vec<SymbolIndex>,
        exports: Vec<SymbolIndex>,
    },
    Trait {
        implementations: Vec<SymbolIndex>,
        methods: Vec<SymbolIndex>,
    },
    Model {
        is_public: bool,
        is_constructable: bool,
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
    Variable {
        is_public: bool,
        declared_type: Option<IntermediateType>,
        inferred_type: EvaluatedType,
    },
    Constant {
        is_public: bool,
        declared_type: IntermediateType,
        inferred_type: EvaluatedType,
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

#[derive(Debug, PartialEq)]
pub enum IntermediateType {
    FunctionType {
        params: Vec<ParameterType>,
        return_type: Box<IntermediateType>,
        span: Span,
    },
    SimpleType {
        value: SymbolIndex,
        generic_args: Vec<IntermediateType>,
        span: Span,
    },
    UnionType {
        types: Vec<IntermediateType>,
        span: Span,
    },
    Placeholder,
}

/// A type expression, as is.
#[derive(Debug, PartialEq)]
pub enum EvaluatedType {
    /// An instance created with `new A()`.
    ModelInstance {
        model: SymbolIndex,
        generic_arguments: Vec<EvaluatedType>,
    },
    Constant(SymbolIndex),
    Model(SymbolIndex),
    Trait(SymbolIndex),
    Enum(SymbolIndex),
    Module(SymbolIndex),
    OpaqueType {
        methods: Vec<SymbolIndex>,
        properties: Vec<SymbolIndex>,
        implementations: Vec<SymbolIndex>,
        collaborators: Vec<SymbolIndex>,
    },
    Void,
    Never,
    Unknown {
        candidates: Vec<EvaluatedType>,
    },
}

impl EvaluatedType {
    pub fn unknown() -> Self {
        Self::Unknown { candidates: vec![] }
    }
}

#[derive(Debug, PartialEq)]
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
        module: PathIndex,
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
    /// Create a new semantic symbol from a variable.
    pub fn from_variable(
        variable: &whirl_ast::VariableSignature,
        origin_span: Span,
    ) -> SemanticSymbol {
        Self {
            name: variable.name.name.to_owned(),
            symbol_kind: SemanticSymbolKind::Variable {
                is_public: variable.is_public,
                declared_type: None,
                inferred_type: EvaluatedType::unknown(),
            },
            references: vec![],
            doc_info_range: None, // todo.
            origin_span,
        }
    }
    /// Create a new symbol from a constant.
    pub fn from_constant(constant: &ConstantSignature, origin_span: Span) -> Self {
        Self {
            name: constant.name.name.to_owned(),
            symbol_kind: SemanticSymbolKind::Constant {
                is_public: constant.is_public,
                declared_type: IntermediateType::Placeholder,
                inferred_type: EvaluatedType::unknown(),
            },
            references: vec![],
            doc_info_range: None, // todo.
            origin_span,
        }
    }
    /// Create a new symbol from a type.
    pub fn from_type(_type: &TypeSignature, origin_span: Span) -> Self {
        Self {
            name: _type.name.name.to_owned(),
            symbol_kind: SemanticSymbolKind::TypeName {
                is_public: _type.is_public,
                generic_arguments: vec![],
                value: vec![],
            },
            references: vec![],
            doc_info_range: None,
            origin_span,
        }
    }
}

impl SymbolTable {
    /// Create a new symbol table.
    pub fn new() -> Self {
        Self {
            holes: vec![],
            symbols: vec![],
        }
    }
    /// Add a symbol to the table and return its index number.
    pub fn add(&mut self, symbol: SemanticSymbol) -> SymbolIndex {
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
        SymbolIndex(index)
    }
    /// Get a symbol using its index.
    pub fn get(&self, index: SymbolIndex) -> Option<&SemanticSymbol> {
        match self.symbols.get(index.0)? {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
    /// Returns a list of the symbols in a module.
    pub fn in_module(&self, path_index: PathIndex) -> impl Iterator<Item = &SemanticSymbol> {
        self.symbols
            .iter()
            .filter(|symbolentry| matches!(symbolentry, SymbolEntry::Symbol(_)))
            .map(|symbolentry| match symbolentry {
                SymbolEntry::Symbol(symbol) => symbol,
                _ => unreachable!(),
            })
            .filter(move |symbol| symbol.references.first().unwrap().module_path == path_index)
    }
    /// Get a symbol mutably using its index.
    pub fn get_mut(&mut self, index: SymbolIndex) -> Option<&mut SemanticSymbol> {
        match self.symbols.get_mut(index.0)? {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
    /// Remove a symbol using its index.
    pub fn remove_symbol(&mut self, index: SymbolIndex) -> Option<SemanticSymbol> {
        let symbolentry = std::mem::take(self.symbols.get_mut(index.0)?);
        self.holes.push(index.0);
        match symbolentry {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
    /// Get a list of at most five related symbols.
    pub fn get_relations_for_symbol_at_index(
        &self,
        _index: SymbolIndex,
    ) -> Option<Vec<&SemanticSymbol>> {
        todo!()
    }
    /// Returns the number of symbols in the table.
    pub fn len(&self) -> usize {
        self.symbols.len() - self.holes.len()
    }
}

impl EvaluatedType {
    /// Returns `true` if the evaluated type is [`Constant`].
    ///
    /// [`Constant`]: EvaluatedType::Constant
    #[must_use]
    pub fn is_constant(&self) -> bool {
        matches!(self, Self::Constant(..))
    }

    /// Returns `true` if the evaluated type is [`Model`].
    ///
    /// [`Model`]: EvaluatedType::Model
    #[must_use]
    pub fn is_model(&self) -> bool {
        matches!(self, Self::Model(..))
    }

    /// Returns `true` if the evaluated type is [`Trait`].
    ///
    /// [`Trait`]: EvaluatedType::Trait
    #[must_use]
    pub fn is_trait(&self) -> bool {
        matches!(self, Self::Trait(..))
    }

    /// Returns `true` if the evaluated type is [`Enum`].
    ///
    /// [`Enum`]: EvaluatedType::Enum
    #[must_use]
    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum(..))
    }

    /// Returns `true` if the evaluated type is [`Module`].
    ///
    /// [`Module`]: EvaluatedType::Module
    #[must_use]
    pub fn is_module(&self) -> bool {
        matches!(self, Self::Module(..))
    }

    /// Returns `true` if the evaluated type is [`ModelInstance`].
    ///
    /// [`ModelInstance`]: EvaluatedType::ModelInstance
    #[must_use]
    pub fn is_model_instance(&self) -> bool {
        matches!(self, Self::ModelInstance { .. })
    }
}

#[cfg(test)]
mod tests {
    use whirl_ast::Span;

    use crate::{SemanticSymbol, SemanticSymbolKind, SymbolTable};

    #[test]
    fn test_symbol_adding() {
        let mut symboltable = SymbolTable::new();
        let symbol_index = symboltable.add(SemanticSymbol {
            name: format!("newVariable"),
            symbol_kind: SemanticSymbolKind::TypeName {
                is_public: false,
                generic_arguments: vec![],
                value: vec![],
            },
            references: vec![],
            doc_info_range: None,
            origin_span: Span::default(),
        });
        assert_eq!(symboltable.get(symbol_index).unwrap().name, "newVariable")
    }

    #[test]
    fn test_symbol_removal() {
        let mut symboltable = SymbolTable::new();
        let symbol_index = symboltable.add(SemanticSymbol {
            name: format!("newVariable"),
            symbol_kind: SemanticSymbolKind::TypeName {
                is_public: false,
                generic_arguments: vec![],
                value: vec![],
            },
            references: vec![],
            doc_info_range: None,
            origin_span: Span::default(),
        });

        assert_eq!(symboltable.len(), 1);

        let removed = symboltable.remove_symbol(symbol_index).unwrap();

        assert_eq!(removed.name, "newVariable");

        assert_eq!(symboltable.len(), 0);

        assert!(symboltable.get(symbol_index).is_none(),)
    }
}
