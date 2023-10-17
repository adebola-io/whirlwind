use crate::PathIndex;
use ast::{
    ConstantSignature, EnumSignature, ShorthandVariableSignature, Span, TypeSignature, WhirlNumber,
    WhirlString,
};
use std::path::Path;

impl From<usize> for PathIndex {
    fn from(value: usize) -> Self {
        PathIndex(value as u32)
    }
}

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
    pub kind: SemanticSymbolKind,
    pub references: Vec<SymbolReferenceList>,
    pub doc_info: Option<Vec<String>>,
    pub origin_span: Span,
}

/// A collection of all instances of a symbol inside a file.
#[derive(Debug, Clone)]
pub struct SymbolReferenceList {
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
        symbols: Vec<SymbolIndex>,
    },
    Trait {
        is_public: bool,
        implementations: Vec<SymbolIndex>,
        methods: Vec<SymbolIndex>,
    },
    Model {
        is_public: bool,
        is_constructable: bool,
        generic_params: Vec<SymbolIndex>,
        implementations: Vec<IntermediateType>,
        methods: Vec<SymbolIndex>,
        attributes: Vec<SymbolIndex>,
    },
    // An enum.
    Enum {
        is_public: bool,
        generic_params: Vec<SymbolIndex>,
        variants: Vec<SymbolIndex>,
    },
    /// Variant of an enum declaration.
    Variant {
        owner_enum: SymbolIndex,
        variant_index: usize,
        tagged_types: Vec<IntermediateType>,
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
    /// An attribute in a model.
    Attribute {
        owner_model: SymbolIndex,
        is_public: bool,
        property_index: usize,
        declared_type: IntermediateType,
        inferred_type: EvaluatedType,
    },
    /// A method of a trait or model.
    Method {
        is_public: bool,
        is_static: bool,
        is_async: bool,
        owner_model_or_trait: SymbolIndex,
        property_index: usize,
        params: Vec<SymbolIndex>,
        generic_params: Vec<SymbolIndex>,
        return_type: Option<IntermediateType>,
    },
    /// Parameter of a function.
    Parameter {
        is_optional: bool,
        param_type: Option<IntermediateType>,
    },
    GenericParameter {
        traits: Vec<IntermediateType>,
        default_value: Option<IntermediateType>,
        solutions: Vec<EvaluatedType>,
    },
    Function {
        is_public: bool,
        is_async: bool,
        params: Vec<SymbolIndex>,
        generic_params: Vec<SymbolIndex>,
        return_type: Option<IntermediateType>,
    },
    FnExpr {
        is_async: bool,
        params: Vec<SymbolIndex>,
        generic_params: Vec<SymbolIndex>,
        return_type: Option<IntermediateType>,
    },
    TypeName {
        is_public: bool,
        generic_params: Vec<SymbolIndex>,
        value: IntermediateType,
    },
    UndeclaredValue,
    Import {
        is_public: bool,
        // The index of the symbol being imported.
        source: Option<SymbolIndex>,
    },
    /// An accessed property on another symbol that cannot be resolved yet.
    Property {
        resolved: Option<SymbolIndex>,
    },
}

impl SemanticSymbolKind {
    pub fn is_public(&self) -> bool {
        match self {
            SemanticSymbolKind::Trait { is_public, .. }
            | SemanticSymbolKind::Model { is_public, .. }
            | SemanticSymbolKind::Enum { is_public, .. }
            | SemanticSymbolKind::Variable { is_public, .. }
            | SemanticSymbolKind::Constant { is_public, .. }
            | SemanticSymbolKind::Function { is_public, .. }
            | SemanticSymbolKind::TypeName { is_public, .. }
            | SemanticSymbolKind::Import { is_public, .. }
            | SemanticSymbolKind::Method { is_public, .. }
            | SemanticSymbolKind::Attribute { is_public, .. } => *is_public,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum IntermediateType {
    FunctionType {
        params: Vec<ParameterType>,
        return_type: Option<Box<IntermediateType>>,
        span: Span,
    },
    MemberType {
        object: Box<IntermediateType>,
        property: Box<IntermediateType>,
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
    This {
        meaning: Option<SymbolIndex>,
        span: Span,
    },
    BorrowedType {
        value: Box<IntermediateType>,
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
    pub is_optional: bool,
    pub type_label: Option<IntermediateType>,
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
            None => self.references.push(SymbolReferenceList {
                module_path,
                starts: vec![span.start],
            }),
        }
    }
    /// Create a new semantic symbol from a variable.
    pub fn from_shorthand_variable(
        variable: &ShorthandVariableSignature,
        path_to_module: PathIndex,
        origin_span: Span,
    ) -> SemanticSymbol {
        Self {
            // taking the name makes it un-lookup-able.
            name: variable.name.name.to_owned(),
            kind: SemanticSymbolKind::Variable {
                is_public: false,
                declared_type: None,
                inferred_type: EvaluatedType::unknown(),
            },
            references: vec![SymbolReferenceList {
                module_path: path_to_module,
                starts: vec![variable.name.span.start],
            }],
            doc_info: variable.info.clone(), // todo.
            origin_span,
        }
    }
    /// Create a new symbol from a constant.
    pub fn from_constant(
        constant: &ConstantSignature,
        path_to_module: PathIndex,
        origin_span: Span,
    ) -> Self {
        Self {
            // taking the name makes it un-lookup-able.
            name: constant.name.name.to_owned(),
            kind: SemanticSymbolKind::Constant {
                is_public: constant.is_public,
                declared_type: IntermediateType::Placeholder,
                inferred_type: EvaluatedType::unknown(),
            },
            references: vec![SymbolReferenceList {
                module_path: path_to_module,
                starts: vec![constant.name.span.start],
            }],
            doc_info: constant.info.clone(), // todo.
            origin_span,
        }
    }
    /// Create a new symbol from a type.
    pub fn from_type(_type: &TypeSignature, path_to_module: PathIndex, origin_span: Span) -> Self {
        Self {
            name: _type.name.name.to_owned(),
            kind: SemanticSymbolKind::TypeName {
                is_public: _type.is_public,
                generic_params: vec![],
                value: IntermediateType::Placeholder,
            },
            references: vec![SymbolReferenceList {
                module_path: path_to_module,
                starts: vec![_type.name.span.start],
            }],
            doc_info: _type.info.clone(),
            origin_span,
        }
    }
    /// Create a new symbol from an enum signature.
    pub fn from_enum(_enum: &EnumSignature, path_to_module: PathIndex, origin_span: Span) -> Self {
        Self {
            name: _enum.name.name.to_owned(),
            kind: SemanticSymbolKind::Enum {
                is_public: _enum.is_public,
                generic_params: vec![],
                variants: vec![],
            },
            references: vec![SymbolReferenceList {
                module_path: path_to_module,
                starts: vec![_enum.name.span.start],
            }],
            doc_info: _enum.info.clone(),
            origin_span,
        }
    }
    /// Create a new symbol from a function signature.
    pub fn from_function(
        function: &ast::FunctionSignature,
        path_idx: PathIndex,
        origin_span: Span,
    ) -> SemanticSymbol {
        Self {
            name: function.name.name.to_owned(),
            kind: SemanticSymbolKind::Function {
                is_public: function.is_public,
                is_async: function.is_async,
                params: vec![],
                generic_params: vec![],
                return_type: None,
            },
            references: vec![SymbolReferenceList {
                module_path: path_idx,
                starts: vec![function.name.span.start],
            }],
            doc_info: function.info.clone(), // todo.
            origin_span,
        }
    }

    /// Create a new symbol from a use import.
    pub fn from_use_import(
        u: &ast::UseTargetSignature,
        path_idx: PathIndex,
        origin_span: Span,
    ) -> SemanticSymbol {
        Self {
            name: u.name.name.to_owned(),
            kind: SemanticSymbolKind::Import {
                is_public: u.is_public,
                source: None,
            },
            references: vec![SymbolReferenceList {
                module_path: path_idx,
                starts: vec![u.name.span.start],
            }],
            doc_info: None,
            origin_span,
        }
    }
    /// Reconstruct the identifier span for the original declaration.
    /// Panics if there is not at least one reference.
    pub fn ident_span(&self) -> Span {
        let start = self.references[0].starts[0];
        Span {
            start,
            end: [start[0], (start[1] as usize + self.name.len()) as u32],
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
    /// Returns an iterator over all the symbols in the table.
    pub fn symbols(&self) -> impl Iterator<Item = (SymbolIndex, &SemanticSymbol)> {
        self.symbols
            .iter()
            .enumerate()
            .filter_map(|symbolentry| match symbolentry {
                (idx, SymbolEntry::Symbol(symbol)) => Some((SymbolIndex(idx), symbol)),
                _ => None,
            })
    }
    /// Get a symbol using its index.
    pub fn get(&self, index: SymbolIndex) -> Option<&SemanticSymbol> {
        match self.symbols.get(index.0)? {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
    /// Returns an iterator of the undeclared values in the table.
    pub fn undeclared_values(&self) -> impl Iterator<Item = &SemanticSymbol> {
        self.symbols()
            .map(|(_, symbol)| symbol)
            .filter(|symbol| matches!(symbol.kind, SemanticSymbolKind::UndeclaredValue))
    }
    /// Returns a list of the symbols in a module.
    pub fn in_module(&self, module_path: PathIndex) -> impl Iterator<Item = &SemanticSymbol> {
        self.symbols()
            .map(|(_, symbol)| symbol)
            .filter(move |symbol| {
                symbol
                    .references
                    .first()
                    .is_some_and(|reference| reference.module_path == module_path)
            })
    }
    /// Returns the first symbol in the table that adheres to a predicate.
    pub fn find<F: FnMut(&&SemanticSymbol) -> bool>(
        &self,
        predicate: F,
    ) -> Option<&SemanticSymbol> {
        self.symbols().map(|(_, symbol)| symbol).find(predicate)
    }
    /// Get a symbol mutably using its index.
    pub fn get_mut(&mut self, idx: SymbolIndex) -> Option<&mut SemanticSymbol> {
        match self.symbols.get_mut(idx.0)? {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
    /// Remove a symbol using its index.
    pub fn remove(&mut self, index: SymbolIndex) -> Option<SemanticSymbol> {
        let symbolentry = std::mem::take(self.symbols.get_mut(index.0)?);
        self.holes.push(index.0);
        match symbolentry {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
    /// Get a list of at most five related symbols for a symbol at an index.
    pub fn get_relations(&self, _index: SymbolIndex) -> Option<Vec<&SemanticSymbol>> {
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
    use ast::Span;

    use crate::{SemanticSymbol, SemanticSymbolKind, SymbolTable};

    #[test]
    fn test_symbol_adding() {
        let mut symboltable = SymbolTable::new();
        let symbol_index = symboltable.add(SemanticSymbol {
            name: format!("newVariable"),
            kind: SemanticSymbolKind::TypeName {
                is_public: false,
                generic_params: vec![],
                value: crate::IntermediateType::Placeholder,
            },
            references: vec![],
            doc_info: None,
            origin_span: Span::default(),
        });
        assert_eq!(symboltable.get(symbol_index).unwrap().name, "newVariable")
    }

    #[test]
    fn test_symbol_removal() {
        let mut symboltable = SymbolTable::new();
        let symbol_index = symboltable.add(SemanticSymbol {
            name: format!("newVariable"),
            kind: SemanticSymbolKind::TypeName {
                is_public: false,
                generic_params: vec![],
                value: crate::IntermediateType::Placeholder,
            },
            references: vec![],
            doc_info: None,
            origin_span: Span::default(),
        });

        assert_eq!(symboltable.len(), 1);

        let removed = symboltable.remove(symbol_index).unwrap();

        assert_eq!(removed.name, "newVariable");

        assert_eq!(symboltable.len(), 0);

        assert!(symboltable.get(symbol_index).is_none(),)
    }
}
