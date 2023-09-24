use crate::{
    EnumSignature, FunctionSignature, ModelSignature, Parameter, TraitSignature, TypeSignature,
    UseTargetSignature, VariableSignature,
};

/// An entry to the symbol table of a scope.
#[derive(Debug)]
pub enum ScopeEntry {
    Function(FunctionSignature),
    Type(TypeSignature),
    Model(ModelSignature),
    Enum(EnumSignature),
    Variable(VariableSignature),
    Trait(TraitSignature),
    Parameter(Parameter),
    UseImport(UseTargetSignature),
    // Entry reserved for a not yet parsed atom.
    ReservedSpace,
}

#[derive(Debug, Default)]
pub enum ScopeType {
    Local,
    Test,
    Functional,
    ModelConstructorOf {
        model: SymbolAddress,
    },
    ModelMethodOf {
        model: SymbolAddress,
    },
    TraitMethodOf {
        _trait: SymbolAddress,
    },
    Global,
    /// A placeholder scope.
    #[default]
    VoidScope,
}

/// The location of a symbol in the whole project.
#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct SymbolAddress {
    pub module_id: usize,
    /// The entry in which it is declared.
    pub scope_id: usize,
    /// The entry number.
    pub entry_no: usize,
}

#[derive(Debug, Default)]
pub struct Scope {
    pub id: usize,
    pub(crate) _type: ScopeType,
    /// The index of the parent scope in the module ambience.
    pub(crate) parent_index: Option<usize>,
    /// The index of the children scopes in the module ambience.
    pub(crate) children_index: Vec<usize>,
    pub(crate) entries: Vec<ScopeEntry>,
}

/// The result of a search through the module ambience.
#[derive(Debug)]
pub struct ScopeSearch<'a> {
    pub index: usize,
    pub entry: &'a ScopeEntry,
    pub scope: &'a Scope,
}

impl From<[usize; 3]> for SymbolAddress {
    fn from(value: [usize; 3]) -> Self {
        SymbolAddress {
            module_id: value[0],
            scope_id: value[1],
            entry_no: value[2],
        }
    }
}

impl ScopeEntry {
    /// Returns the name of an entry.
    pub fn name(&self) -> &str {
        match self {
            ScopeEntry::Function(FunctionSignature { name, .. })
            | ScopeEntry::Type(TypeSignature { name, .. })
            | ScopeEntry::Model(ModelSignature { name, .. })
            | ScopeEntry::Enum(EnumSignature { name, .. })
            | ScopeEntry::Variable(VariableSignature { name, .. })
            | ScopeEntry::Trait(TraitSignature { name, .. })
            | ScopeEntry::Parameter(Parameter { name, .. })
            | ScopeEntry::UseImport(UseTargetSignature { name, .. }) => &name.name,
            ScopeEntry::ReservedSpace => "",
        }
    }

    /// Returns the identifier of an entry.
    pub fn ident(&self) -> Option<&crate::Identifier> {
        match self {
            ScopeEntry::Function(FunctionSignature { name, .. })
            | ScopeEntry::Type(TypeSignature { name, .. })
            | ScopeEntry::Model(ModelSignature { name, .. })
            | ScopeEntry::Enum(EnumSignature { name, .. })
            | ScopeEntry::Variable(VariableSignature { name, .. })
            | ScopeEntry::Trait(TraitSignature { name, .. })
            | ScopeEntry::Parameter(Parameter { name, .. })
            | ScopeEntry::UseImport(UseTargetSignature { name, .. }) => Some(&name),
            ScopeEntry::ReservedSpace => None,
        }
    }

    /// Returns an entry as a variable signature. Panics if the entry is not a variable variant.
    pub fn var(&self) -> &VariableSignature {
        match self {
            ScopeEntry::Variable(v) => v,
            _ => panic!("{} is not a variable!", self.name()),
        }
    }

    /// Returns an entry as a mutable variable signature. Panics if the entry is not a variable variant.
    pub fn var_mut(&mut self) -> &mut VariableSignature {
        match self {
            ScopeEntry::Variable(v) => v,
            _ => panic!("{} is not a variable!", self.name()),
        }
    }

    /// Returns an entry as a function signature. Panics if the entry is not a function variant.
    pub fn func(&self) -> &FunctionSignature {
        match self {
            ScopeEntry::Function(f) => f,
            _ => panic!("{} is not a function!", self.name()),
        }
    }
    /// Returns an entry as a mutable function signature. Panics if the entry is not a function variant.
    pub fn func_mut(&mut self) -> &mut FunctionSignature {
        match self {
            ScopeEntry::Function(f) => f,
            _ => panic!("{} is not a function!", self.name()),
        }
    }
    /// Returns an entry as a model signature. Panics if the entry is not a model variant.
    pub fn model(&self) -> &ModelSignature {
        match self {
            ScopeEntry::Model(m) => m,
            _ => panic!("{} is not a model!", self.name()),
        }
    }
    /// Returns an entry as a mutable model signature. Panics if the entry is not a model variant.
    pub fn model_mut(&mut self) -> &mut ModelSignature {
        match self {
            ScopeEntry::Model(m) => m,
            _ => panic!("{} is not a model!", self.name()),
        }
    }
    /// Returns true of the entry is reserved.
    pub fn is_reserved(&self) -> bool {
        matches!(self, ScopeEntry::ReservedSpace)
    }
}

impl Scope {
    /// Creates a global scope.
    pub fn global() -> Self {
        Scope {
            id: 0,
            _type: ScopeType::Global,
            parent_index: None,
            children_index: vec![],
            entries: vec![],
        }
    }
    /// Creates a child of another scope.
    pub fn local(id: usize, parent: usize, _type: ScopeType) -> Self {
        Scope {
            id,
            _type,
            parent_index: Some(parent),
            children_index: vec![],
            entries: vec![],
        }
    }
    /// Find an item inside the current scope.
    pub fn find(&self, name: &str) -> Option<(usize, &ScopeEntry)> {
        for (index, entry) in self.entries.iter().enumerate().rev() {
            if entry.name() == name {
                return Some((index, entry));
            }
        }
        None
    }
    /// Add an entry to the scope and returns its index.
    pub fn add(&mut self, entry: ScopeEntry) -> usize {
        self.entries.push(entry);
        self.entries.len() - 1
    }
    /// Remove an entry with an index.
    pub fn remove(&mut self, entry_no: usize) -> ScopeEntry {
        self.entries.remove(entry_no)
    }
    /// Get a model entry by its index.
    pub fn get_model(&self, entry_no: usize) -> Option<&ModelSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Model(c) => Some(c),
                _ => None,
            })
            .flatten()
    }
    /// Get a function entry by its index.
    pub fn get_function(&self, entry_no: usize) -> Option<&FunctionSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Function(f) => Some(f),
                _ => None,
            })
            .flatten()
    }
    /// Get a type entry by its index.
    pub fn get_type(&self, entry_no: usize) -> Option<&TypeSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Type(t) => Some(t),
                _ => None,
            })
            .flatten()
    }
    /// Get a enum entry by its index.
    pub fn get_enum(&self, entry_no: usize) -> Option<&EnumSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Enum(e) => Some(e),
                _ => None,
            })
            .flatten()
    }
    /// Get a variable entry by its index.
    pub fn get_variable(&self, entry_no: usize) -> Option<&VariableSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Variable(v) => Some(v),
                _ => None,
            })
            .flatten()
    }
    /// Get a variable entry by its index.
    pub fn get_variable_mut(&mut self, entry_no: usize) -> Option<&mut VariableSignature> {
        self.entries
            .get_mut(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Variable(v) => Some(v),
                _ => None,
            })
            .flatten()
    }
    /// Get a trait entry by its index.
    pub fn get_trait(&self, entry_no: usize) -> Option<&TraitSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Trait(t) => Some(t),
                _ => None,
            })
            .flatten()
    }
    /// Get a scope entry.
    pub fn get_entry(&self, entry_no: usize) -> Option<&ScopeEntry> {
        self.entries.get(entry_no)
    }
    /// Get a scope entry mutably.
    pub fn get_entry_mut(&mut self, entry_no: usize) -> Option<&mut ScopeEntry> {
        self.entries.get_mut(entry_no)
    }
    /// Returns true if the scope is a global scope.
    pub fn is_global(&self) -> bool {
        matches!(self._type, ScopeType::Global)
    }
}
