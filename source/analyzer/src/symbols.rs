use crate::{evaluate, utils::symbol_to_type, EvaluatedType, PathIndex, TypedExpression};
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

/// An index into the list of literals in the program.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct LiteralIndex(pub usize);

/// An id for the scope containing the declaration of a symbol in a module.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ScopeId(pub u32);

/// An index into the symbol table.
#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct SymbolIndex(pub usize);

/// An identifier, basically. Contains an index to the symbol name, and a reference number.
#[derive(Debug, PartialEq)]
pub struct SymbolLocator {
    pub symbol_idx: SymbolIndex,
    pub start: [u32; 2],
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
#[derive(Debug)]
pub enum VariablePatternForm {
    Normal,
    DestructuredFromObject { from_property: Option<SymbolIndex> },
    DestructuredFromArray,
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
    Trait {
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
        inferred_type: EvaluatedType,
    },
    GenericParameter {
        traits: Vec<IntermediateType>,
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

#[derive(Debug, PartialEq, Clone)]
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

/// The symbol table for the whole project.
#[derive(Debug, Default)]
pub struct SymbolTable {
    holes: Vec<usize>,
    symbols: Vec<SymbolEntry>,
    pub string_symbol: Option<SymbolIndex>,
    pub array_symbol: Option<SymbolIndex>,
    pub bool_symbol: Option<SymbolIndex>,
    pub prospect_symbol: Option<SymbolIndex>,
    pub never_symbol: Option<SymbolIndex>,
    pub addition_symbol: Option<SymbolIndex>,
    pub maybe_symbol: Option<SymbolIndex>, // todo ...
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

impl SymbolTable {
    /// Create a new symbol table.
    pub fn new() -> Self {
        Self {
            holes: vec![],
            symbols: vec![],
            ..Default::default()
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
    /// Returns the type of an expression.
    pub fn get_expression_type(
        &self,
        expression: &TypedExpression,
        literals: &Vec<Literal>,
    ) -> Option<EvaluatedType> {
        Some(match expression {
            TypedExpression::Identifier(ident) => {
                let symbol = self.get_forwarded(ident.value)?;
                return symbol_to_type(symbol, ident.value, self).ok();
            }
            TypedExpression::Literal(literal) => EvaluatedType::ModelInstance {
                model: match literals.get(literal.0)? {
                    Literal::StringLiteral { .. } => self.string_symbol?,
                    Literal::NumericLiteral { .. } => return Some(EvaluatedType::Unknown), // todo.
                    Literal::BooleanLiteral { .. } => self.bool_symbol?,
                },
                generic_arguments: vec![],
            },
            TypedExpression::NewExpr(new) => new.inferred_type.clone(),
            TypedExpression::ThisExpr(this) => this.inferred_type.clone(),
            TypedExpression::CallExpr(call) => call.inferred_type.clone(),
            TypedExpression::FnExpr(f) => f.inferred_type.clone(),
            TypedExpression::Block(block) => block.inferred_type.clone(),
            TypedExpression::IfExpr(if_) => if_.inferred_type.clone(),
            TypedExpression::AccessExpr(access) => access.inferred_type.clone(),
            TypedExpression::ArrayExpr(array) => array.inferred_type.clone(),
            TypedExpression::IndexExpr(i) => i.inferred_type.clone(),
            TypedExpression::BinaryExpr(b) => b.inferred_type.clone(),
            TypedExpression::AssignmentExpr(a) => a.inferred_type.clone(),
            TypedExpression::UnaryExpr(u) => u.inferred_type.clone(),
            TypedExpression::LogicExpr(l) => l.inferred_type.clone(),
            TypedExpression::UpdateExpr(u) => u.inferred_type.clone(),
        })
    }
    /// Get a symbol mutably using its index.
    pub fn get_mut(&mut self, idx: SymbolIndex) -> Option<&mut SemanticSymbol> {
        match self.symbols.get_mut(idx.0)? {
            SymbolEntry::Removed => None,
            SymbolEntry::Symbol(symbol) => Some(symbol),
        }
    }
    /// A modified version of the `get()` method that also accounts for import and property redirections.
    pub fn get_forwarded(&self, idx: SymbolIndex) -> Option<&SemanticSymbol> {
        let base = self.get(idx)?;
        match &base.kind {
            SemanticSymbolKind::Property {
                resolved: Some(next),
            }
            | SemanticSymbolKind::Import {
                source: Some(next), ..
            } => self.get_forwarded(*next),
            _ => Some(base),
        }
    }
    /// Follow a property or an import chain to its source index.
    pub fn forward(&self, idx: SymbolIndex) -> SymbolIndex {
        match self.get(idx) {
            Some(base) => match &base.kind {
                SemanticSymbolKind::Property {
                    resolved: Some(next),
                }
                | SemanticSymbolKind::Import {
                    source: Some(next), ..
                } => self.forward(*next),
                _ => idx,
            },
            None => idx,
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

    /// Prints a list of generic types.
    fn format_generics_into<'a>(
        &self,
        generics: impl Iterator<Item = &'a EvaluatedType>,
        string: &mut String,
        len: usize,
    ) {
        if len > 0 {
            string.push('<');
            for (index, genarg) in generics.enumerate() {
                string.push_str(&self.format_evaluated_type(genarg));
                if index + 1 < len {
                    string.push_str(", ")
                }
            }
            string.push('>');
        }
    }

    /// Prints an evaluated type using the symbol table.
    pub fn format_evaluated_type(&self, eval_type: &EvaluatedType) -> String {
        let mut string = String::new();
        match eval_type {
            EvaluatedType::ModelInstance {
                model,
                generic_arguments,
            } => {
                let symbol = self.get(*model).unwrap();
                string = symbol.name.clone();

                self.format_generics_into(
                    get_just_types(generic_arguments),
                    &mut string,
                    generic_arguments.len(),
                );
            }
            EvaluatedType::TraitInstance {
                trait_,
                generic_arguments,
            } => {
                let symbol = self.get(*trait_).unwrap();
                string = symbol.name.clone();
                self.format_generics_into(
                    get_just_types(generic_arguments),
                    &mut string,
                    generic_arguments.len(),
                );
            }
            EvaluatedType::Model(_) => string.push_str("{model}"),
            EvaluatedType::Trait(_) => string.push_str("{trait}"),
            EvaluatedType::EnumInstance {
                enum_,
                generic_arguments,
            } => {
                let symbol = self.get(*enum_).unwrap();
                string = symbol.name.clone();
                self.format_generics_into(
                    get_just_types(generic_arguments),
                    &mut string,
                    generic_arguments.len(),
                );
            }
            EvaluatedType::FunctionInstance {
                function,
                generic_arguments,
            } => {
                self.format_function_details(function, &mut string, generic_arguments);
            }
            EvaluatedType::FunctionExpressionInstance {
                is_async,
                params,
                return_type,
                ..
            } => {
                if *is_async {
                    string.push_str("async ")
                }
                string.push_str("fn(");
                for (idx, param) in params.iter().enumerate() {
                    string.push_str(&param.name);
                    if param.is_optional {
                        string.push('?');
                    }
                    string.push_str(": ");
                    string.push_str(&self.format_evaluated_type(&param.inferred_type));
                    if idx != params.len() - 1 {
                        string.push_str(", ");
                    }
                }
                string.push_str(")");
                if !return_type.is_void() {
                    string.push_str(&format!(": {}", self.format_evaluated_type(return_type)));
                }
            }
            EvaluatedType::MethodInstance {
                method,
                generic_arguments,
            } => {
                self.format_function_details(method, &mut string, &generic_arguments);
            }
            EvaluatedType::Enum(_enum) => {
                string.push_str("{enum ");
                let symbol = self.get(*_enum).unwrap();
                string.push_str(&symbol.name);
                string.push_str("}");
            }
            EvaluatedType::Module(module) => {
                string.push_str("{module ");
                let symbol = self.get(*module).unwrap();
                string.push_str(&symbol.name);
                string.push_str("}");
            }
            EvaluatedType::Generic { base, .. } => {
                string.push_str("{type ");
                let symbol = self.get(*base).unwrap();
                string.push_str(&symbol.name);
                string.push_str("}");
            }
            EvaluatedType::HardGeneric { base } => {
                let symbol = self.get(*base).unwrap();
                string.push_str(&symbol.name);
            }
            EvaluatedType::Borrowed { base } => {
                string.push('&');
                string.push_str(&self.format_evaluated_type(base));
            }
            EvaluatedType::OpaqueTypeInstance { collaborators, .. } => {
                for (index, collaborator) in collaborators.iter().enumerate() {
                    let name = &self.get(*collaborator).unwrap().name;
                    string.push_str(name);
                    if index + 1 < collaborators.len() {
                        string.push_str(" | ");
                    }
                    // Show at most 5 types + the last one.
                    if index == 4 && collaborators.len() > 6 {
                        let len = collaborators.len();
                        string.push_str("... ");
                        string.push_str(&(len - 6).to_string());
                        string.push_str(" more ... | ");
                        let name = &self.get(*collaborators.last().unwrap()).unwrap().name;
                        string.push_str(name);
                        break;
                    }
                }
            }
            EvaluatedType::Void => string.push_str("{void}"),
            EvaluatedType::Never => string.push_str("never"),
            EvaluatedType::Unknown { .. } => string.push_str("{unknown}"),
            EvaluatedType::Partial { types } => {
                for (idx, typ) in types.iter().enumerate() {
                    string.push_str(&self.format_evaluated_type(&typ));
                    if idx + 1 != types.len() {
                        string.push_str(" | ");
                    }
                }
            }
        }
        string
    }

    /// Prints out a functional evaluated type.
    fn format_function_details(
        &self,
        function: &SymbolIndex,
        string: &mut String,
        generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    ) {
        let function_symbol = self.get(*function).unwrap();
        let (is_async, _, params, return_type) = match &function_symbol.kind {
            SemanticSymbolKind::Function {
                is_async,
                params,
                generic_params,
                return_type,
                ..
            }
            | SemanticSymbolKind::Method {
                is_async,
                params,
                generic_params,
                return_type,
                ..
            } => (*is_async, generic_params, params, return_type),
            _ => unreachable!("Expected a functional type but got {:?}", &function_symbol),
        };
        if is_async {
            string.push_str("async ");
        }
        string.push_str("fn(");
        // format (evaluated) parameter types.
        for (idx, param) in params.iter().enumerate() {
            let parameter_symbol = self.get(*param).unwrap();
            string.push_str(&parameter_symbol.name);
            if let SemanticSymbolKind::Parameter {
                is_optional,
                param_type,
                ..
            } = &parameter_symbol.kind
            {
                if *is_optional {
                    string.push('?');
                }
                string.push_str(": ");
                let eval_type = param_type
                    .as_ref()
                    .map(|param_type| {
                        evaluate(param_type, self, Some(generic_arguments), &mut None)
                    })
                    .unwrap_or(EvaluatedType::Unknown);
                string.push_str(&self.format_evaluated_type(&eval_type));
            }
            if idx != params.len() - 1 {
                string.push_str(", ");
            }
        }
        string.push(')');
        if let Some(typ) = return_type {
            let evaluated = evaluate(typ, self, Some(&generic_arguments), &mut None);
            if !evaluated.is_void() {
                string.push_str(": ");
                string.push_str(&self.format_evaluated_type(&evaluated));
            }
        }
    }
}

fn get_just_types(
    generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
) -> impl Iterator<Item = &EvaluatedType> {
    generic_arguments
        .iter()
        .map(|(_, evaluated_type)| evaluated_type)
}
#[cfg(test)]
mod tests {
    use ast::Span;

    use crate::{ScopeId, SemanticSymbol, SemanticSymbolKind, SymbolTable};

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
            origin_scope_id: Some(ScopeId(0)),
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
            origin_scope_id: Some(ScopeId(0)),
        });
        assert_eq!(symboltable.len(), 1);

        let removed = symboltable.remove(symbol_index).unwrap();

        assert_eq!(removed.name, "newVariable");

        assert_eq!(symboltable.len(), 0);

        assert!(symboltable.get(symbol_index).is_none(),)
    }
}
