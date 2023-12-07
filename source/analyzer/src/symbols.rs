use crate::{EvaluatedType, PathIndex};
use ast::{
    ConstantSignature, EnumSignature, ShorthandVariableSignature, Span, TypeSignature, WhirlNumber,
    WhirlString,
};
use std::{path::Path, vec};

impl From<usize> for PathIndex {
    fn from(value: usize) -> Self {
        PathIndex(value as u32)
    }
}

/// An id for the scope containing the declaration of a symbol in a module.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ScopeId(pub u32);

/// An index into the symbol table.
#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct SymbolIndex(pub PathIndex, pub u32);

/// A symbol in the context of a fully resolved program.
#[derive(Debug)]
pub struct SemanticSymbol {
    pub name: String,
    pub kind: SemanticSymbolKind,
    pub references: Vec<SymbolReferenceList>,
    pub doc_info: Option<Vec<String>>,
    pub origin_span: Span,
    pub origin_scope_id: Option<ScopeId>,
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
#[derive(Debug, Clone)]
pub enum VariablePatternForm {
    Normal,
    DestructuredFromObject { from_property: SymbolIndex },
    DestructuredFromArray,
}

impl VariablePatternForm {
    /// Returns `true` if the variable pattern form is [`Normal`].
    ///
    /// [`Normal`]: VariablePatternForm::Normal
    #[must_use]
    pub fn is_normal(&self) -> bool {
        matches!(self, Self::Normal)
    }
}

#[derive(Debug)]
pub enum SemanticSymbolKind {
    Module {
        parent_modules: Vec<SymbolIndex>,
        /// Declarations from other modules that are referenced in this module.
        external_symbols: Vec<SymbolIndex>,
        /// The declarations in the global scope of this module.
        global_declaration_symbols: Vec<SymbolIndex>,
    },
    Interface {
        is_public: bool,
        implementations: Vec<IntermediateType>,
        generic_params: Vec<SymbolIndex>,
        methods: Vec<SymbolIndex>,
    },
    Model {
        is_public: bool,
        is_constructable: bool,
        generic_params: Vec<SymbolIndex>,
        constructor_parameters: Option<Vec<SymbolIndex>>,
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
        pattern_type: VariablePatternForm,
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
    },
    /// A method of a interface or model.
    Method {
        is_public: bool,
        is_static: bool,
        is_async: bool,
        owner_model_or_interface: SymbolIndex,
        property_index: usize,
        params: Vec<SymbolIndex>,
        generic_params: Vec<SymbolIndex>,
        return_type: Option<IntermediateType>,
    },
    /// Parameter of a function.
    Parameter {
        is_optional: bool,
        param_type: Option<IntermediateType>,
        inferred_type: EvaluatedType,
    },
    GenericParameter {
        interfaces: Vec<IntermediateType>,
        default_value: Option<IntermediateType>,
    },
    Function {
        is_public: bool,
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
        /// When typechecking, whether or not the property is accessed on an opaque type.
        is_opaque: bool,
        resolved: Option<SymbolIndex>,
    },
}

impl SemanticSymbolKind {
    pub fn is_public(&self) -> bool {
        match self {
            SemanticSymbolKind::Interface { is_public, .. }
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

#[derive(Debug, PartialEq, Clone)]
pub struct IntermediateTypeProperty {
    pub actual: Option<SymbolIndex>,
    pub name: String,
    pub generic_args: Vec<IntermediateType>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntermediateType {
    FunctionType {
        params: Vec<ParameterType>,
        return_type: Option<Box<IntermediateType>>,
        span: Span,
    },
    MemberType {
        object: Box<IntermediateType>,
        property: IntermediateTypeProperty,
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

impl IntermediateType {
    pub fn span(&self) -> Span {
        match self {
            IntermediateType::FunctionType { span, .. }
            | IntermediateType::MemberType { span, .. }
            | IntermediateType::SimpleType { span, .. }
            | IntermediateType::UnionType { span, .. }
            | IntermediateType::This { span, .. }
            | IntermediateType::BorrowedType { span, .. } => *span,
            _ => unreachable!("Cannot retrieve span of placeholder."),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParameterType {
    pub name: String,
    pub is_optional: bool,
    pub type_label: Option<IntermediateType>,
    pub inferred_type: EvaluatedType,
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

impl SemanticSymbol {
    /// Add a reference to this symbol.
    pub fn add_reference(&mut self, module_path: PathIndex, span: Span) {
        match self
            .references
            .iter_mut()
            .find(|list| list.module_path == module_path)
        {
            Some(list) => {
                if !list.starts.iter().any(|start| *start == span.start) {
                    list.starts.push(span.start);
                }
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
        origin_scope_id: Option<ScopeId>,
    ) -> SemanticSymbol {
        Self {
            // taking the name makes it un-lookup-able.
            name: variable.name.name.to_owned(),
            kind: SemanticSymbolKind::Variable {
                pattern_type: VariablePatternForm::Normal,
                is_public: false,
                declared_type: None,
                inferred_type: EvaluatedType::Unknown,
            },
            references: vec![SymbolReferenceList {
                module_path: path_to_module,
                starts: vec![variable.name.span.start],
            }],
            doc_info: variable.info.clone(), // todo.
            origin_span,
            origin_scope_id,
        }
    }
    /// Create a new symbol from a constant.
    pub fn from_constant(
        constant: &ConstantSignature,
        path_to_module: PathIndex,
        origin_span: Span,
        origin_scope_id: Option<ScopeId>,
    ) -> Self {
        Self {
            // taking the name makes it un-lookup-able.
            name: constant.name.name.to_owned(),
            kind: SemanticSymbolKind::Constant {
                is_public: constant.is_public,
                declared_type: IntermediateType::Placeholder,
                inferred_type: EvaluatedType::Unknown,
            },
            references: vec![SymbolReferenceList {
                module_path: path_to_module,
                starts: vec![constant.name.span.start],
            }],
            doc_info: constant.info.clone(), // todo.
            origin_span,
            origin_scope_id,
        }
    }
    /// Create a new symbol from a type.
    pub fn from_type(
        _type: &TypeSignature,
        path_to_module: PathIndex,
        origin_span: Span,
        origin_scope_id: Option<ScopeId>,
    ) -> Self {
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
            origin_scope_id,
        }
    }
    /// Create a new symbol from an enum signature.
    pub fn from_enum(
        _enum: &EnumSignature,
        path_to_module: PathIndex,
        origin_span: Span,
        origin_scope_id: Option<ScopeId>,
    ) -> Self {
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
            origin_scope_id,
        }
    }
    /// Create a new symbol from a function signature.
    pub fn from_function(
        function: &ast::FunctionSignature,
        path_idx: PathIndex,
        origin_span: Span,
        origin_scope_id: Option<ScopeId>,
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
            origin_scope_id,
        }
    }

    /// Create a new symbol from a use import.
    pub fn from_use_import(
        u: &ast::UseTargetSignature,
        path_idx: PathIndex,
        origin_span: Span,
        origin_scope_id: Option<ScopeId>,
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
            origin_scope_id,
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
    /// Checks if the symbol was declared in a module.
    pub fn was_declared_in(&self, module_path: PathIndex) -> bool {
        self.references
            .first()
            .is_some_and(|reference| reference.module_path == module_path)
    }
}
