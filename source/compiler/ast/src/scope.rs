use std::hash::Hash;

use crate::{
    EnumSignature, FunctionSignature, InterfaceSignature, LoopLabel, LoopVariable, ModelSignature,
    Parameter, ShorthandVariableSignature, TypeEquationSignature, UseTargetSignature,
    VariablePattern, VariableSignature,
};

/// An id for the scope containing the declaration of a symbol in a module.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ScopeId(pub u32);

/// An entry to a local scope.
#[derive(Debug, Hash)]
pub enum ScopeEntry {
    Function(FunctionSignature),
    Type(TypeEquationSignature),
    Model(ModelSignature),
    Enum(EnumSignature),
    ShorthandVariable(ShorthandVariableSignature),
    Variable(VariableSignature),
    Interface(InterfaceSignature),
    Parameter(Parameter),
    UseImport(UseTargetSignature),
    LoopVariable(LoopVariable),
    LoopLabel(LoopLabel),
    // Entry reserved for a not yet parsed atom.
    ReservedSpace,
}

#[derive(Debug, Default, Clone)]
pub enum ScopeType {
    Local,
    IfConsequent,
    IfAlternate,
    Test,
    Functional,
    WhileLoop,
    ForLoop,
    ModelConstructorOf {
        model: ScopeAddress,
    },
    ModelMethodOf {
        model: ScopeAddress,
    },
    InterfaceMethodOf {
        _interface: ScopeAddress,
    },
    Global,
    /// A placeholder scope.
    #[default]
    VoidScope,
}

/// The location of a symbol in the whole project.
#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct ScopeAddress {
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
impl<'a> ScopeSearch<'a> {
    /// Rebuild the scope address from the search.
    pub fn construct_address(&self, module_id: usize) -> ScopeAddress {
        ScopeAddress {
            module_id,
            scope_id: self.scope.id,
            entry_no: self.index,
        }
    }
}

impl From<[usize; 3]> for ScopeAddress {
    fn from(value: [usize; 3]) -> Self {
        ScopeAddress {
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
            | ScopeEntry::Type(TypeEquationSignature { name, .. })
            | ScopeEntry::Model(ModelSignature { name, .. })
            | ScopeEntry::Enum(EnumSignature { name, .. })
            | ScopeEntry::ShorthandVariable(ShorthandVariableSignature { name, .. })
            | ScopeEntry::Interface(InterfaceSignature { name, .. })
            | ScopeEntry::Parameter(Parameter { name, .. })
            | ScopeEntry::UseImport(UseTargetSignature { name, .. })
            | ScopeEntry::Variable(VariableSignature {
                name: VariablePattern::Identifier(name) | VariablePattern::ArrayPattern(name),
                ..
            })
            | ScopeEntry::Variable(VariableSignature {
                name:
                    VariablePattern::ObjectPattern {
                        alias: Some(name), ..
                    },
                ..
            })
            | ScopeEntry::Variable(VariableSignature {
                name:
                    VariablePattern::ObjectPattern {
                        real_name: name, ..
                    },
                ..
            })
            | ScopeEntry::LoopVariable(LoopVariable {
                name: VariablePattern::Identifier(name) | VariablePattern::ArrayPattern(name),
                ..
            })
            | ScopeEntry::LoopVariable(LoopVariable {
                name:
                    VariablePattern::ObjectPattern {
                        alias: Some(name), ..
                    },
                ..
            })
            | ScopeEntry::LoopVariable(LoopVariable {
                name:
                    VariablePattern::ObjectPattern {
                        real_name: name, ..
                    },
                ..
            })
            | ScopeEntry::LoopLabel(LoopLabel(name)) => &name.name,
            ScopeEntry::ReservedSpace => {
                unreachable!("Cannot retrieve the name of a reserved space. What are you doing?")
            }
        }
    }

    /// Returns the identifier of an entry.
    pub fn ident(&self) -> Option<&crate::Identifier> {
        match self {
            ScopeEntry::Function(FunctionSignature { name, .. })
            | ScopeEntry::Type(TypeEquationSignature { name, .. })
            | ScopeEntry::Model(ModelSignature { name, .. })
            | ScopeEntry::Enum(EnumSignature { name, .. })
            | ScopeEntry::ShorthandVariable(ShorthandVariableSignature { name, .. })
            | ScopeEntry::Interface(InterfaceSignature { name, .. })
            | ScopeEntry::Parameter(Parameter { name, .. })
            | ScopeEntry::UseImport(UseTargetSignature { name, .. })
            | ScopeEntry::Variable(VariableSignature {
                name: VariablePattern::Identifier(name) | VariablePattern::ArrayPattern(name),
                ..
            })
            | ScopeEntry::Variable(VariableSignature {
                name:
                    VariablePattern::ObjectPattern {
                        alias: Some(name), ..
                    },
                ..
            })
            | ScopeEntry::Variable(VariableSignature {
                name:
                    VariablePattern::ObjectPattern {
                        real_name: name, ..
                    },
                ..
            })
            | ScopeEntry::LoopVariable(LoopVariable {
                name: VariablePattern::Identifier(name) | VariablePattern::ArrayPattern(name),
                ..
            })
            | ScopeEntry::LoopVariable(LoopVariable {
                name:
                    VariablePattern::ObjectPattern {
                        alias: Some(name), ..
                    },
                ..
            })
            | ScopeEntry::LoopVariable(LoopVariable {
                name:
                    VariablePattern::ObjectPattern {
                        real_name: name, ..
                    },
                ..
            })
            | ScopeEntry::LoopLabel(LoopLabel(name)) => Some(&name),
            ScopeEntry::ReservedSpace => None,
        }
    }
    /// Returns true if the entry is marked as public.
    pub fn is_public(&self) -> bool {
        match self {
            ScopeEntry::Function(f) => f.is_public,
            ScopeEntry::Type(t) => t.is_public,
            ScopeEntry::Model(m) => m.is_public,
            ScopeEntry::Enum(e) => e.is_public,
            ScopeEntry::Interface(t) => t.is_public,
            ScopeEntry::UseImport(u) => u.is_public,
            ScopeEntry::Variable(v) => v.is_public,
            _ => false,
        }
    }

    /// Returns an entry as a variable signature. Panics if the entry is not a variable variant.
    pub fn shortvar(&self) -> &ShorthandVariableSignature {
        match self {
            ScopeEntry::ShorthandVariable(v) => v,
            _ => panic!("{} is not a variable!", self.name()),
        }
    }

    /// Returns an entry as a mutable variable signature. Panics if the entry is not a variable variant.
    pub fn shortvar_mut(&mut self) -> &mut ShorthandVariableSignature {
        match self {
            ScopeEntry::ShorthandVariable(v) => v,
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
    /// Returns an entry as a type signature. Panics if the entry is not a type variant.
    pub fn type_(&self) -> &TypeEquationSignature {
        match self {
            ScopeEntry::Type(t) => t,
            _ => panic!("{} is not a type!", self.name()),
        }
    }
    /// Returns an entry as a mutable type signature. Panics if the entry is not a type variant.
    pub fn type_mut(&mut self) -> &mut TypeEquationSignature {
        match self {
            ScopeEntry::Type(t) => t,
            _ => panic!("{} is not a type!", self.name()),
        }
    }
    /// Returns an entry as an enum signature. Panics if the entry is not a enum variant.
    pub fn enum_(&self) -> &EnumSignature {
        match self {
            ScopeEntry::Enum(e) => e,
            _ => panic!("{} is not an enum!", self.name()),
        }
    }
    /// Returns an entry as a mutable enum signature. Panics if the entry is not a enum variant.
    pub fn enum_mut(&mut self) -> &mut EnumSignature {
        match self {
            ScopeEntry::Enum(e) => e,
            _ => panic!("{} is not an enum!", self.name()),
        }
    }
    /// Returns an entry as a variable. Panics if the entry is not a variable variant.
    pub fn var(&self) -> &VariableSignature {
        match self {
            ScopeEntry::Variable(v) => v,
            _ => panic!("{} is not a variable!!", self.name()),
        }
    }
    /// Returns the entry as a loop variable. Panics if the entry is not a loop variable variant
    pub fn loop_var(&self) -> &LoopVariable {
        match self {
            ScopeEntry::LoopVariable(l) => l,
            _ => panic!("{} is not a loop variable!!", self.name()),
        }
    }
    /// Returns the entry as a mutable loop variable. Panics if the entry is not a loop variable variant
    pub fn loop_var_mut(&mut self) -> &mut LoopVariable {
        match self {
            ScopeEntry::LoopVariable(l) => l,
            _ => panic!("{} is not a loop variable!!", self.name()),
        }
    }
    /// Returns true of the entry is reserved.
    pub fn is_reserved(&self) -> bool {
        matches!(self, ScopeEntry::ReservedSpace)
    }

    pub fn _interface(&self) -> &InterfaceSignature {
        match self {
            ScopeEntry::Interface(t) => t,
            _ => panic!("{} is not a interface!", self.name()),
        }
    }

    pub fn _interface_mut(&mut self) -> &mut InterfaceSignature {
        match self {
            ScopeEntry::Interface(t) => t,
            _ => panic!("{} is not a interface!", self.name()),
        }
    }

    /// Returns `true` if the scope entry is [`Parameter`].
    ///
    /// [`Parameter`]: ScopeEntry::Parameter
    #[must_use]
    pub fn is_parameter(&self) -> bool {
        matches!(self, Self::Parameter(..))
    }

    /// Returns `true` if the scope entry is [`LoopVariable`].
    ///
    /// [`LoopVariable`]: ScopeEntry::LoopVariable
    #[must_use]
    pub fn is_loop_variable(&self) -> bool {
        matches!(self, Self::LoopVariable(..))
    }

    /// Returns `true` if the scope entry is [`LoopLabel`].
    ///
    /// [`LoopLabel`]: ScopeEntry::LoopLabel
    #[must_use]
    pub fn is_loop_label(&self) -> bool {
        matches!(self, Self::LoopLabel(..))
    }
}

impl PartialEq<Self> for &ScopeEntry {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for &ScopeEntry {}

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
        // check for parameters, loop vars and labels first.
        let parameters = self
            .entries
            .iter()
            .enumerate()
            .filter(|entry| entry.1.is_parameter());
        let loopvars = self
            .entries
            .iter()
            .enumerate()
            .filter(|entry| entry.1.is_loop_variable());
        let loop_labels = self
            .entries
            .iter()
            .enumerate()
            .filter(|entry| entry.1.is_loop_label());

        let other_entries = self.entries.iter().enumerate().filter(|entry| {
            !entry.1.is_parameter() && !entry.1.is_loop_variable() && !entry.1.is_loop_label()
        });
        for (index, entry) in parameters
            .chain(loopvars)
            .chain(loop_labels)
            .chain(other_entries)
        {
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
    pub fn get_type(&self, entry_no: usize) -> Option<&TypeEquationSignature> {
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
    pub fn get_shorthand_variable(&self, entry_no: usize) -> Option<&ShorthandVariableSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::ShorthandVariable(v) => Some(v),
                _ => None,
            })
            .flatten()
    }
    /// Get a variable entry by its index.
    pub fn get_shorthand_variable_mut(
        &mut self,
        entry_no: usize,
    ) -> Option<&mut ShorthandVariableSignature> {
        self.entries
            .get_mut(entry_no)
            .map(|entry| match entry {
                ScopeEntry::ShorthandVariable(v) => Some(v),
                _ => None,
            })
            .flatten()
    }
    /// Get a interface entry by its index.
    pub fn get_interface(&self, entry_no: usize) -> Option<&InterfaceSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Interface(t) => Some(t),
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
