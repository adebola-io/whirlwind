mod programerror;
mod typed_expr;
mod typed_module;
mod typed_statement;

use crate::{
    EvaluatedType, IntermediateType, Literal, LiteralIndex, Module, ModuleGraph, PathIndex,
    SemanticSymbol, SemanticSymbolKind, SymbolIndex, SymbolLocator, SymbolReferenceList,
    SymbolTable,
};
use ast::{
    Block, ConstantDeclaration, EnumDeclaration, EnumVariant, Expression, FunctionExpr,
    GenericParameter, Identifier, ModelBody, ModelDeclaration, ModelProperty, ModelPropertyType,
    ModelSignature, Parameter, ReturnStatement, ScopeAddress, ScopeEntry, ScopeSearch, ScopeType,
    ShorthandVariableDeclaration, Span, Statement, TypeDeclaration, TypeExpression, WhileStatement,
};
use errors::ContextError;
pub use programerror::*;
use std::{cell::RefCell, collections::HashMap, mem::take, path::PathBuf, vec};
pub use typed_expr::*;
pub use typed_module::*;
pub use typed_statement::*;

/// The binder takes each module and joins all its symbols to their original declaration.
/// It does this by updating a symbol table with semantic entries and returning the index to be stored by the referrer.
/// After this phase, all variables are no longer strings, but indexes to the original declarations.
///
/// It also handles errors like block scoped variable redeclaration, use of undeclared variables, etc.
pub struct Binder<'ctx> {
    /// The module to bind.
    module: RefCell<Module>,
    /// The (emptied) module.
    _graph: &'ctx ModuleGraph,
    paths: &'ctx mut Vec<PathBuf>,
    /// The context symbol table.
    symbol_table: RefCell<&'ctx mut SymbolTable>,
    /// The context errors.
    errors: RefCell<&'ctx mut Vec<ProgramError>>,
    /// Bound values. The span is the closest thing to a unique marker for each symbol identifier.
    known_values: RefCell<HashMap<Span, SymbolIndex>>,
    /// Values that are undeclared in the scope tree.
    unknown_values: RefCell<HashMap<(String, usize), SymbolIndex>>,
    /// Generic parameter values.
    generic_pools: RefCell<Vec<Vec<(String, SymbolIndex)>>>,
    /// Literal values.
    literals: RefCell<&'ctx mut Vec<Literal>>,
    /// The current scope in which to search for a match.
    current_scope: RefCell<usize>,
    /// The model `This` currently refers to.
    this_type: RefCell<Vec<SymbolIndex>>,
}

pub struct TemporaryParameterDetails {
    name: String,
    is_optional: bool,
    index: SymbolIndex,
}

pub struct TemporaryVariantDetails {
    name: String,
    index: SymbolIndex,
}

impl<'ctx> Binder<'ctx> {
    /// Create new node reducer.
    pub fn new(
        module: Module,
        graph: &'ctx ModuleGraph,
        paths: &'ctx mut Vec<PathBuf>,
        symbol_table: &'ctx mut SymbolTable,
        literals: &'ctx mut Vec<Literal>,
        errors: &'ctx mut Vec<ProgramError>,
    ) -> Self {
        Self {
            module: RefCell::new(module),
            _graph: graph,
            paths,
            symbol_table: RefCell::new(symbol_table),
            errors: RefCell::new(errors),
            known_values: RefCell::new(HashMap::new()),
            unknown_values: RefCell::new(HashMap::new()),
            generic_pools: RefCell::new(vec![]),
            literals: RefCell::new(literals),
            current_scope: RefCell::new(0),
            this_type: RefCell::new(vec![]),
        }
    }
    /// Takes a module and converts it to its typed equivalent.
    pub fn bind(&'ctx mut self) -> Option<TypedModule> {
        let path = PathIndex(self.paths.len() as u32);
        self.paths.push(self.module().module_path.as_ref()?.clone());
        self.collect_prior_errors(path);
        let line_lengths = take(&mut self.module()._line_lens);
        let statements = take(&mut self.module().statements)
            .into_iter()
            .map(|statement| self.statement(statement))
            .collect();
        let typed_module = TypedModule {
            path,
            line_lengths,
            statements,
        };
        Some(typed_module)
    }
    /// Collect the import, syntax and lexing errors,
    fn collect_prior_errors(&'ctx self, path: PathIndex) {
        let mut errors = self.errors.borrow_mut();
        // Collect lexical errors.
        let l_errors = take(&mut self.module().lexical_errors);
        l_errors.into_iter().for_each(|lex_error| {
            errors.push(ProgramError {
                offending_file: path,
                error_type: ProgramErrorType::Lexical(lex_error),
            })
        });
        // Collect syntax errors.
        let p_errors = take(&mut self.module().syntax_errors);
        p_errors.into_iter().for_each(|parse_error| {
            errors.push(ProgramError {
                offending_file: path,
                error_type: ProgramErrorType::Syntax(parse_error),
            })
        });
        // Collect import errors.
        let i_errors = take(&mut self.module().import_errors);
        i_errors.into_iter().for_each(|import_error| {
            errors.push(ProgramError {
                offending_file: path,
                error_type: ProgramErrorType::Importing(import_error),
            })
        });
    }
    /// Set the current scope of the binder.
    fn set_scope(&self, scope: usize) {
        *self.current_scope.borrow_mut() = scope
    }
}

// Binder Utilities.
impl<'ctx> Binder<'ctx> {
    /// Returns a scope entry.
    fn entry(&'ctx self, address: ScopeAddress) -> &mut ScopeEntry {
        self.module().ambience.get_entry_unguarded_mut(address)
    }
    /// Returns a mutable reference to the context symbol table.
    fn symbol_table(&self) -> &mut SymbolTable {
        unsafe { &mut *self.symbol_table.as_ptr() }
    }
    /// Returns a mutable reference to the map of known values.
    fn known_values(&self) -> &mut HashMap<Span, SymbolIndex> {
        unsafe { &mut *self.known_values.as_ptr() }
    }
    /// Returns a mutable reference to the module being contextualized.
    fn module(&self) -> &mut Module {
        unsafe { &mut *self.module.as_ptr() }
    }
    /// Returns the module's path index.
    /// It is only valid after the path index has been added.
    fn path_idx(&self) -> PathIndex {
        PathIndex((self.paths.len() - 1) as u32)
    }
    /// Returns the reference number for the last reference added to the symbol.
    fn last_reference_no(&'ctx self, symbol_index: SymbolIndex) -> usize {
        self.symbol_table()
            .get(symbol_index)
            .unwrap()
            .references
            .len()
            - 1
    }
    /// Pushes an error to the list of context errors.
    fn add_ctx_error(&'ctx self, error: ContextError) {
        self.errors
            .borrow_mut()
            .push(ProgramError::contextual(self.path_idx(), error))
    }
    /// Returns the current ambience scope.
    fn scope(&self) -> usize {
        *self.current_scope.borrow()
    }
    /// Finds an already existing symbol or creates a new one.
    fn find_or_create(&'ctx self, name: &Identifier, in_type_context: bool) -> SymbolIndex {
        let known_values = self.known_values();
        let shadow = self.module().ambience.create_shadow(self.scope());
        let index = (|| {
            // generic parameters take precedence over values if in type context.
            if in_type_context {
                if let Some(index) = self.lookup_generic_parameter(&name.name) {
                    return index;
                }
            }
            match shadow.lookup(&name.name) {
                Some(entry) => {
                    // Reconstruct the address and get a mutable reference to the entry instead.
                    // Sad, unethical stuff.
                    let address = ScopeAddress {
                        module_id: self.path_idx().0 as usize,
                        scope_id: entry.scope.id,
                        entry_no: entry.index,
                    };
                    let entry = self.entry(address);
                    match known_values.get(&entry.ident().unwrap().span) {
                        // Entry has been added the symbol table.
                        Some(index) => *index,
                        // Entry exists but it has not been added to the symbol table yet.
                        // Pause the current binding and jump to bind that symbol.
                        // It binds the symbol with a placeholder declaration range,
                        // that will be overwritten when the actual declaration is encountered.
                        // If the symbol is a constant or a variable, then this process will result in a context error later on.
                        None => self.bind_signature(entry, Span::default()),
                    }
                }
                None => {
                    // Try to see if there is a generic parameter with this name.
                    if let Some(index) = self.lookup_generic_parameter(&name.name) {
                        return index;
                    }
                    // Entry does not exist.
                    self.add_ctx_error(errors::unknown_value(name.name.to_owned(), name.span));
                    let shadow = self.module().ambience.create_shadow(self.scope());
                    let mut unknown_values = self.unknown_values.borrow_mut();
                    // If there is another unknown symbol with the same name being tracked in the same or a parent scope, equate the two symbols.
                    // Else, add new undeclared symbol to table and unknown values map.
                    for ((unknown_name, scope), symbol_index) in unknown_values.iter() {
                        if unknown_name == &name.name && shadow.is_inclusive_child_of(*scope) {
                            return *symbol_index;
                        }
                    }
                    let new_symbol = SemanticSymbol {
                        name: name.name.to_owned(),
                        symbol_kind: SemanticSymbolKind::UndeclaredValue,
                        references: vec![],
                        doc_info: None,
                        origin_span: name.span,
                    };
                    let index = self.symbol_table().add(new_symbol);
                    unknown_values.insert((name.name.to_owned(), self.scope()), index);
                    return index;
                }
            }
        })();
        // Add this reference.
        let symbol = self.symbol_table().get_mut(index).unwrap();
        symbol.add_reference(self.path_idx(), name.span);
        return index;
    }
    /// Bind an entry within a scope.
    fn handle_scope_entry(
        &'ctx self,
        address: ScopeAddress,
        span: Span,
        allow_hoisting: bool,
    ) -> Result<SymbolIndex, SymbolIndex> {
        let ambience = &self.module().ambience;
        let entry = ambience.get_entry_unguarded(address);
        // Check if entry already exists.
        let shadow = ambience.create_shadow(self.scope());
        let symbol_idx = if let Some(search) = shadow.lookaround(entry.name()) {
            // There is a clashing name in the same scope.
            // The first name will always take precedence.
            if !std::ptr::eq(search.entry, entry) {
                Err(self.error_and_return_first_instance(search.entry, entry))
            } else {
                // Use the mutable address instead.
                // This happens so some entry fields can be taken, not cloned when they are made into symbols.
                let entry = self.entry(address);
                // There is no other scope-bound instance of this name that is not equal to this signature.
                // If the symbol has already been used before, equate the instances.
                // If the symbol cannot be hoisted, e.g. a constant or a variable, cause an error.
                Ok(self
                    .maybe_bound(&entry, !allow_hoisting, span)
                    // No clashes in the current scope, and no previous usage.
                    // create new symbol for this constant.
                    .unwrap_or_else(|| self.bind_signature(entry, span)))
            }
        } else {
            unreachable!("Mismatched scope address. How is that possible?")
        };
        symbol_idx
    }
    /// Create a binding for a signature from a scope entry.
    fn bind_signature(&'ctx self, entry: &mut ScopeEntry, origin_span: Span) -> SymbolIndex {
        match entry {
            ScopeEntry::Function(_) => todo!(),
            ScopeEntry::Type(_type) => {
                let symbol = SemanticSymbol::from_type(_type, self.path_idx(), origin_span);
                let index = self.symbol_table().add(symbol);
                let span = _type.name.span;
                self.known_values().insert(span, index);
                // Type value and generic parameters are bound at declaration to prevent recursive loops.
                index
            }
            ScopeEntry::Model(model) => {
                let symbol = SemanticSymbol {
                    // taking the string will make the entry un-lookup-able.
                    name: model.name.name.to_owned(),
                    symbol_kind: SemanticSymbolKind::Model {
                        is_public: model.is_public,
                        // Signatures may have been prematurely categorized as needing no AST tree data.
                        // The values after are written later and not now
                        // to prevent model-implements-itself or attribute-refers-to-parent-model recursive loops.
                        // Allowing jumps and hoisting makes many things hard.
                        is_constructable: false,
                        generic_params: vec![],
                        implementations: vec![],
                        methods: vec![],
                        attributes: vec![],
                    },
                    references: vec![SymbolReferenceList {
                        module_path: self.path_idx(),
                        starts: vec![model.name.span.start],
                    }],
                    doc_info: model.info.take(), // todo.
                    origin_span,
                };
                let index = self.symbol_table().add(symbol);
                let span = model.name.span;
                self.known_values().insert(span, index);
                index
            }
            ScopeEntry::Enum(_enum) => {
                let symbol = SemanticSymbol::from_enum(_enum, self.path_idx(), origin_span);
                let index = self.symbol_table().add(symbol);
                let span = _enum.name.span;
                self.known_values().insert(span, index);
                index
            }
            ScopeEntry::ShorthandVariable(variable) => {
                let symbol =
                    SemanticSymbol::from_shorthand_variable(variable, self.path_idx(), origin_span);
                let index = self.symbol_table().add(symbol);
                let span = variable.name.span;
                self.known_values().insert(span, index);
                // Add type. Hackery to prevent recursive real-name-is-also-type-name loops.
                if let SemanticSymbolKind::Variable { declared_type, .. } =
                    &mut self.symbol_table().get_mut(index).unwrap().symbol_kind
                {
                    *declared_type = variable
                        .var_type
                        .as_ref()
                        .map(|type_expr| self.type_expression(type_expr));
                }
                index
            }
            ScopeEntry::Trait(_) => todo!(),
            ScopeEntry::Parameter(_) => todo!(),
            ScopeEntry::UseImport(_) => todo!(),
            ScopeEntry::ReservedSpace => {
                unreachable!("Encountered a reserved space while binding.")
            }
            ScopeEntry::Constant(constant) => {
                let symbol = SemanticSymbol::from_constant(constant, self.path_idx(), origin_span);
                let index = self.symbol_table().add(symbol);
                self.known_values().insert(constant.name.span, index);
                // Add type. Hackery to prevent recursive real-name-is-also-type-name loops.
                if let SemanticSymbolKind::Constant { declared_type, .. } =
                    &mut self.symbol_table().get_mut(index).unwrap().symbol_kind
                {
                    *declared_type = self.type_expression(&constant.var_type)
                }
                index
            }
            ScopeEntry::Variable(_) => todo!(),
        }
    }
    /// Check if a symbol has been jumped to already.
    fn maybe_bound(
        &'ctx self,
        entry: &ScopeEntry,
        should_cause_error: bool,
        declaration_range: Span,
    ) -> Option<SymbolIndex> {
        let known_values = self.known_values();
        let symbol_table = self.symbol_table();
        let span = entry.ident()?.span;
        // Value has already been added to symbol table before its declaration was encountered,
        if let Some(symbolindex) = known_values.get(&span) {
            let symbol = symbol_table.get(*symbolindex).unwrap();
            if should_cause_error {
                symbol
                    .references
                    .iter()
                    .filter(|reference| reference.module_path == self.path_idx())
                    .map(|reference| reference.starts.iter())
                    .flatten()
                    .for_each(|start| {
                        self.add_ctx_error(errors::use_before_declare(
                            entry.name().to_owned(),
                            span_from_start(*start, entry.name()),
                        ))
                    });
            }
            let symbol = symbol_table.get_mut(*symbolindex).unwrap();
            // Mark new found declaration range.
            symbol.origin_span = declaration_range;
            return Some(*symbolindex);
        }
        return None;
    }
    /// Add an error for a duplicate declaration, and return the symbol index of the first declaration symbol.
    fn error_and_return_first_instance(
        &'ctx self,
        entry_1: &ScopeEntry,
        entry_2: &ScopeEntry,
    ) -> SymbolIndex {
        self.add_ctx_error(errors::already_declared_in_scope(
            entry_1.name().to_owned(),
            entry_2.ident().unwrap().span,
        ));
        let span = entry_1.ident().unwrap().span;
        let symbol_index = *(self.known_values.borrow().get(&span).unwrap());
        // Add reference to this.
        self.symbol_table
            .borrow_mut()
            .get_mut(symbol_index)
            .unwrap()
            .add_reference(self.path_idx(), entry_2.ident().unwrap().span);
        // return index of former declaration.
        symbol_index
    }
}

// Generics.
impl<'ctx> Binder<'ctx> {
    /// Add a new generic pool.
    fn push_generic_pool(&self) {
        self.generic_pools.borrow_mut().push(vec![]);
    }
    /// Remove the last added generic pool.
    fn pop_generic_pool(&self) {
        self.generic_pools.borrow_mut().pop();
    }
    /// Check in the list of counting pools for a generic parameter.
    fn lookup_generic_parameter(&'ctx self, name: &str) -> Option<SymbolIndex> {
        let pools = self.generic_pools.borrow();
        for pool in pools.iter().rev() {
            for tuple in pool {
                if tuple.0 == name {
                    return Some(tuple.1);
                }
            }
        }
        return None;
    }
    /// Check in the current pool for a generic parameter.
    fn lookaround_for_generic_parameter(&'ctx self, name: &str) -> Option<SymbolIndex> {
        let pools = self.generic_pools.borrow();
        let current_pool = pools.last()?;
        current_pool
            .iter()
            .find(|param_tuple| param_tuple.0 == name)
            .map(|tuple| tuple.1)
    }
    /// Add a generic parameter to the current pool.
    fn add_generic_parameter(&'ctx self, name: &str, index: SymbolIndex) {
        let mut pools = self.generic_pools.borrow_mut();
        let current_pool = pools
            .last_mut()
            .expect("Cannot add generic parameter, because no pool has been created.");
        current_pool.push((name.to_owned(), index));
    }
}

// Literals.
impl<'ctx> Binder<'ctx> {
    /// Bind a string.
    fn string(&'ctx self, value: ast::WhirlString) -> LiteralIndex {
        // todo: can literals be deduplicated?
        let literal = Literal::StringLiteral {
            module: self.path_idx(),
            value,
        };
        let index = LiteralIndex(self.literals.borrow().len());
        self.literals.borrow_mut().push(literal);
        index
    }
    /// Bind a number.
    fn number(&self, value: ast::WhirlNumber) -> LiteralIndex {
        // todo: can literals be deduplicated?
        let literal = Literal::NumericLiteral {
            module: self.path_idx(),
            value,
        };
        let index = LiteralIndex(self.literals.borrow().len());
        self.literals.borrow_mut().push(literal);
        index
    }
    // Bind a boolean.
    fn boolean(&self, value: ast::WhirlBoolean) -> LiteralIndex {
        // todo: can literals be deduplicated?
        let literal = Literal::BooleanLiteral {
            module: self.path_idx(),
            start_line: value.span.start[0],
            start_character: value.span.start[1],
            value: value.value,
        };
        let index = LiteralIndex(self.literals.borrow().len());
        self.literals.borrow_mut().push(literal);
        index
    }
}

// Statements
impl<'ctx> Binder<'ctx> {
    // Bind a statement.
    fn statement(&'ctx self, statement: Statement) -> TypedStmnt {
        match statement {
            Statement::TestDeclaration(_) => todo!(),
            Statement::UseDeclaration(_) => todo!(),
            Statement::VariableDeclaration(_) => todo!(),
            Statement::ShorthandVariableDeclaration(shorthandvariable) => {
                TypedStmnt::ShorthandVariableDeclaration(
                    self.shorthand_variable_declaration(shorthandvariable),
                )
            }
            Statement::ConstantDeclaration(constant) => {
                TypedStmnt::ConstantDeclaration(self.constant_declaration(constant))
            }
            Statement::ModelDeclaration(model) => {
                TypedStmnt::ModelDeclaration(self.model_declaration(model))
            }
            Statement::ModuleDeclaration(module) => {
                TypedStmnt::ModuleDeclaration(TypedModuleDeclaration { span: module.span })
            }
            Statement::FunctionDeclaration(_) => todo!(),
            Statement::RecordDeclaration => todo!(),
            Statement::TraitDeclaration(_) => todo!(),
            Statement::EnumDeclaration(_enum) => {
                TypedStmnt::EnumDeclaration(self.enum_declaration(_enum))
            }
            Statement::TypeDeclaration(typedecl) => {
                TypedStmnt::TypeDeclaration(self.type_declaration(typedecl))
            }
            Statement::WhileStatement(while_statement) => {
                TypedStmnt::WhileStatement(self.while_statement(while_statement))
            }
            Statement::ReturnStatement(returnstmnt) => {
                TypedStmnt::ReturnStatement(self.return_statement(returnstmnt))
            }
            Statement::ForStatement => todo!(),
            Statement::ExpressionStatement(expression) | Statement::FreeExpression(expression) => {
                TypedStmnt::FreeExpression(self.expression(expression))
            }
        }
    }
    /// Binds a return statement.
    fn return_statement(&'ctx self, returnstmnt: ReturnStatement) -> TypedReturnStatement {
        TypedReturnStatement {
            value: returnstmnt
                .value
                .map(|expression| self.expression(expression)),
            span: returnstmnt.span,
        }
    }
    /// Binds a while statement.
    fn while_statement(&'ctx self, while_statement: WhileStatement) -> TypedWhileStatement {
        TypedWhileStatement {
            condition: self.expression(while_statement.condition),
            body: self.block(while_statement.body),
            span: while_statement.span,
        }
    }
}

// Declarations.
impl<'ctx> Binder<'ctx> {
    /// Binds a shorthand variable declaration.
    fn shorthand_variable_declaration(
        &'ctx self,
        shorthand: ShorthandVariableDeclaration,
    ) -> TypedShorthandVariableDeclaration {
        let symbol_idx = match self.handle_scope_entry(shorthand.address, shorthand.span, false) {
            Ok(idx) | Err(idx) => idx,
        };
        let ref_number = self.last_reference_no(symbol_idx);
        return TypedShorthandVariableDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            value: self.expression(shorthand.value),
            span: shorthand.span,
        };
    }

    /// Bind a constant declaration.
    fn constant_declaration(&'ctx self, constant: ConstantDeclaration) -> TypedConstantDeclaration {
        let symbol_idx = match self.handle_scope_entry(constant.address, constant.span, false) {
            Ok(idx) | Err(idx) => idx,
        };
        let ref_number = self.last_reference_no(symbol_idx);
        return TypedConstantDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            value: self.expression(constant.value),
            span: constant.span,
        };
    }

    /// Binds a type declaration.
    fn type_declaration(&'ctx self, type_decl: TypeDeclaration) -> TypedTypeDeclaration {
        let binding_result = self.handle_scope_entry(type_decl.address, type_decl.span, true);
        let signature = self.entry(type_decl.address).type_mut();
        let symbol_idx = match binding_result {
            Ok(symbol_idx) => {
                // Add generic parameters, if the binding to this type was successful.
                self.push_generic_pool(); // List of parameters.
                match &mut self.symbol_table().get_mut(symbol_idx).unwrap().symbol_kind {
                    SemanticSymbolKind::TypeName {
                        generic_params,
                        value,
                        ..
                    } => {
                        *generic_params =
                            self.generic_parameters(signature.generic_params.as_ref());
                        *value = self.type_expression(&signature.value);
                    }
                    _ => unreachable!("Bound a type but could not retrieve its value."),
                }
                self.pop_generic_pool();
                symbol_idx
            }
            Err(idx) => idx,
        };
        let ref_number = self.last_reference_no(symbol_idx);
        TypedTypeDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            span: type_decl.span,
        }
    }

    /// Bind a model declaration.
    fn model_declaration(&'ctx self, model: ModelDeclaration) -> TypedModelDeclaration {
        let binding_result = self.handle_scope_entry(model.address, model.span, true); // models can be hoisted.
        let signature = self.entry(model.address).model_mut();
        self.push_generic_pool(); // List of parameters.
        let symbol_idx = match binding_result {
            Ok(symbol_idx) => {
                // Add generic parameters, if the binding to this type was successful.
                if let Some(SemanticSymbol {
                    symbol_kind:
                        SemanticSymbolKind::Model {
                            is_constructable,
                            implementations,
                            generic_params,
                            ..
                        },
                    ..
                }) = &mut self.symbol_table().get_mut(symbol_idx)
                {
                    *is_constructable = model.body.constructor.is_some();
                    // Generic parameters should always be first.
                    *generic_params = self.generic_parameters(signature.generic_params.as_ref());
                    *implementations = signature
                        .implementations
                        .iter()
                        .map(|implementation| self.type_expression(implementation))
                        .collect();
                }
                symbol_idx
            }
            Err(idx) => idx,
        };
        self.this_type.borrow_mut().push(symbol_idx); // Set meaning of `This`.
        let ref_number = self.last_reference_no(symbol_idx);
        let typed_model_declaration = TypedModelDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            body: self.model_body(model.body, symbol_idx, signature.parameters.take()),
            span: model.span,
        };
        self.pop_generic_pool();
        self.this_type.borrow_mut().pop(); // Remove meaning of This.
        return typed_model_declaration;
    }

    /// Bind a model body.
    fn model_body(
        &'ctx self,
        body: ModelBody,
        owner_idx: SymbolIndex,
        constructor_params: Option<Vec<Parameter>>,
    ) -> TypedModelBody {
        TypedModelBody {
            constructor: body.constructor.map(|constructor_block| {
                let (block, parameters) =
                    self.function_block(constructor_block, constructor_params.unwrap());
                TypedModelConstructor { parameters, block }
            }),
            properties: body
                .properties
                .into_iter()
                .map(|property| self.model_property(property, owner_idx))
                .collect(),
            span: body.span,
        }
    }

    /// Binds a model property.
    fn model_property(
        &'ctx self,
        property: ModelProperty,
        owner: SymbolIndex,
    ) -> TypedModelProperty {
        let ambience = &self.module().ambience;
        let shadow = ambience.create_shadow(self.scope());
        let symbol_table = self.symbol_table();
        let search = shadow
            .lookaround(&symbol_table.get(owner).unwrap().name)
            .unwrap();
        let address_of_model = search.construct_address(self.path_idx().0 as usize);
        let ambience = &mut self.module().ambience; // 😬
        let model = ambience
            .get_entry_unguarded_mut(address_of_model)
            .model_mut();
        match property._type {
            // bind a model attribute.
            ModelPropertyType::Attribute => {
                self.model_attribute(model, property.index, property.span, search, owner)
            }
            ModelPropertyType::Method { body } => {
                self.model_method(model, property.index, property.span, search, owner, body)
            }
            ModelPropertyType::TraitImpl { .. } => todo!(),
        }
    }

    /// Binds a model attribute.
    fn model_attribute(
        &'ctx self,
        model: &mut ModelSignature,
        index: usize,
        span: Span,
        search: ScopeSearch,
        owner: SymbolIndex,
    ) -> TypedModelProperty {
        let attribute = model
            .attributes
            .get_mut(index)
            .expect("Could not find an attribute with the correct signature.");
        // Check for duplicates.
        // find first instance of an attribute name that matches this one.
        let parent_model = search.entry.model();
        let first_instance = parent_model
            .attributes
            .iter()
            .find(|attr| attr.name.name == attribute.name.name)
            .unwrap();
        let symbol_idx = if !std::ptr::eq(first_instance, attribute) {
            // instances of the same name are not equal.
            self.add_ctx_error(errors::duplicate_property(attribute.name.to_owned()));
            // fault tolerance for the type.
            self.type_expression(&attribute.var_type);
            // By definition, if the attribute being bound is not the first attribute instance with
            // the given name, then the first has already been bound,
            // add a reference to the first instance and return its index.
            let symbol_index = *self.known_values().get(&first_instance.name.span).unwrap();
            let symbol = self.symbol_table().get_mut(symbol_index).unwrap();
            symbol.add_reference(self.path_idx(), attribute.name.span);
            symbol_index
            // block method and attribute clashes.
        } else if let Some(method) = parent_model
            .methods
            .iter()
            .filter(|method| method.name.span.is_before(attribute.name.span))
            .find(|method| method.name.name == attribute.name.name)
        {
            self.add_ctx_error(errors::duplicate_property(method.name.to_owned()));
            // fault tolerance for the type.
            self.type_expression(&attribute.var_type);
            let symbol_index = *self.known_values().get(&method.name.span).unwrap();
            let symbol = self.symbol_table().get_mut(symbol_index).unwrap();
            symbol.add_reference(self.path_idx(), attribute.name.span);
            symbol_index
        } else {
            // attribute being bound is the first attribute to be bound with this name.
            let symbol = SemanticSymbol {
                name: attribute.name.name.to_owned(),
                symbol_kind: SemanticSymbolKind::Attribute {
                    is_public: attribute.is_public,
                    declared_type: self.type_expression(&attribute.var_type),
                    inferred_type: EvaluatedType::unknown(),
                    owner_model: owner,
                    property_index: index,
                },
                // add first reference.
                references: vec![SymbolReferenceList {
                    module_path: self.path_idx(),
                    starts: vec![attribute.name.span.start],
                }],
                doc_info: attribute.info.take(),
                origin_span: span,
            };
            let index = self.symbol_table().add(symbol);
            self.known_values().insert(attribute.name.span, index);
            index
        };
        let ref_number = self.last_reference_no(symbol_idx);
        TypedModelProperty {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            _type: TypedModelPropertyType::TypedAttribute,
            span,
        }
    }

    /// Binds a model method.
    fn model_method(
        &'ctx self,
        model: &mut ModelSignature,
        index: usize,
        span: Span,
        search: ScopeSearch,
        owner: SymbolIndex,
        body: Block,
    ) -> TypedModelProperty {
        self.push_generic_pool(); // Add a list of reachable generic parameters.
        let method = model
            .methods
            .get_mut(index)
            .expect("Could not find method with correct index.");
        // check for duplicate names.
        let parent_model = search.entry.model();
        let first_instance = parent_model
            .methods
            .iter()
            .find(|meth| meth.name.name == method.name.name)
            .unwrap();
        let symbol_idx = if !std::ptr::eq(first_instance, method) {
            self.add_ctx_error(errors::duplicate_property(method.name.to_owned()));
            let symbol_index = *self.known_values().get(&first_instance.name.span).unwrap();
            let symbol = self.symbol_table().get_mut(symbol_index).unwrap();
            symbol.add_reference(self.path_idx(), method.name.span);
            symbol_index
            // block method and attribute clashes.
        } else if let Some(attribute) = parent_model
            .attributes
            .iter()
            .filter(|attr| attr.name.span.is_before(method.name.span))
            .find(|attr| attr.name.name == method.name.name)
        {
            self.add_ctx_error(errors::duplicate_property(method.name.to_owned()));
            let symbol_index = *self.known_values().get(&attribute.name.span).unwrap();
            let symbol = self.symbol_table().get_mut(symbol_index).unwrap();
            symbol.add_reference(self.path_idx(), method.name.span);
            symbol_index
        } else {
            // first property with this name to be bound.
            let symbol = SemanticSymbol {
                name: method.name.name.to_owned(),
                symbol_kind: SemanticSymbolKind::Method {
                    is_public: method.is_public,
                    is_static: method.is_static,
                    is_async: method.is_async,
                    owner_model_or_trait: owner,
                    property_index: index,
                    params: vec![],
                    generic_params: vec![],
                    return_type: None,
                },
                references: vec![SymbolReferenceList {
                    module_path: self.path_idx(),
                    starts: vec![method.name.span.start],
                }],
                doc_info: method.info.take(),
                origin_span: span,
            };
            let index = self.symbol_table().add(symbol);
            self.known_values().insert(method.name.span, index);
            index
        };
        let ref_number = self.last_reference_no(symbol_idx);
        let (body, parameters) = self.function_block(body, take(&mut method.params));
        // Add return type, generic parameters and parameters.
        if let SemanticSymbolKind::Method {
            params,
            generic_params,
            return_type,
            ..
        } = &mut self.symbol_table().get_mut(symbol_idx).unwrap().symbol_kind
        {
            *params = parameters;
            *generic_params = match method.generic_params {
                Some(ref generic_parameters) => generic_parameters
                    .iter()
                    .map(|parameter| self.bind_generic_parameter(parameter))
                    .collect(),
                None => vec![],
            };
            *return_type = method
                .return_type
                .as_ref()
                .map(|rtype| self.type_expression(rtype))
        }
        let method = TypedModelProperty {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            _type: TypedModelPropertyType::TypedMethod { body },
            span,
        };
        self.pop_generic_pool(); // Remove list of reachable generic parameters.
        return method;
    }

    /// Binds a function block.
    fn function_block(
        &'ctx self,
        block: Block,
        params: Vec<Parameter>,
    ) -> (TypedBlock, Vec<SymbolIndex>) {
        let ambience = &mut self.module().ambience;
        ambience.jump_to_scope(block.scope_id);
        let mut parameters: Vec<TemporaryParameterDetails> = vec![];
        for mut parameter in params {
            // Block the use of parameters with the same name.
            if parameters
                .iter()
                .find(|details| details.name == parameter.name.name)
                .is_some()
            {
                self.add_ctx_error(errors::duplicate_parameter_names(parameter.name.to_owned()));
                continue;
            }
            // Block the use of required parameters after optional ones.
            if let Some(TemporaryParameterDetails {
                is_optional: last_was_optional,
                ..
            }) = parameters.last()
            {
                if *last_was_optional && !parameter.is_optional {
                    self.add_ctx_error(errors::required_parameter_after_optional(parameter.span));
                }
            }
            let mut symbol = SemanticSymbol {
                name: parameter.name.name.to_owned(),
                symbol_kind: SemanticSymbolKind::Parameter {
                    is_optional: parameter.is_optional,
                    param_type: parameter
                        .type_label
                        .as_ref()
                        .map(|type_exp| self.type_expression(type_exp)),
                },
                references: vec![],
                doc_info: parameter.info.take(),
                origin_span: parameter.span,
            };
            symbol.add_reference(self.path_idx(), parameter.name.span);
            // add symbol.
            let index = self.symbol_table().add(symbol);
            self.known_values().insert(parameter.name.span, index);
            // store for next param check.
            parameters.push(TemporaryParameterDetails {
                name: parameter.name.name.to_owned(),
                is_optional: parameter.is_optional,
                index,
            });
            // parameters are treated as mock declarations in the block.
            // The mechanism for searching for an entry ensures that they will always take
            // precedence over other declarations.
            self.module()
                .ambience
                .register(ScopeEntry::Parameter(parameter));
        }
        ambience.leave_scope();
        (
            self.block(block),
            parameters
                .into_iter()
                .map(|details| details.index)
                .collect(),
        )
    }

    /// Binds an enum declaration.
    fn enum_declaration(&'ctx self, enumdecl: EnumDeclaration) -> TypedEnumDeclaration {
        let binding_result = self.handle_scope_entry(enumdecl.address, enumdecl.span, true); // enums can be hoisted.
        self.push_generic_pool();
        let signature = self.entry(enumdecl.address).enum_mut();
        let symbol_idx = match binding_result {
            Ok(symbol_idx) => {
                self.this_type.borrow_mut().push(symbol_idx); // Set meaning of `This`.
                match &mut self.symbol_table().get_mut(symbol_idx) {
                    // Add generic parameters, if the binding to this type was successful.
                    Some(SemanticSymbol {
                        symbol_kind:
                            SemanticSymbolKind::Enum {
                                generic_params,
                                variants,
                                ..
                            },
                        ..
                    }) => {
                        // Generic parameters should always be first.
                        *generic_params =
                            self.generic_parameters(signature.generic_params.as_ref());
                        *variants = self.enum_variants(&signature.variants, symbol_idx);
                    }
                    _ => unreachable!("Cannot retrieve bound enum."),
                }
                symbol_idx
            }
            Err(idx) => {
                self.this_type.borrow_mut().push(idx); // Set meaning of `This`.
                idx
            }
        };
        let ref_number = self.last_reference_no(symbol_idx);
        let enum_declaration = TypedEnumDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            span: enumdecl.span,
        };
        self.this_type.borrow_mut().pop(); // Remove meaning of This.
        self.pop_generic_pool();
        return enum_declaration;
    }

    /// Bind a list enum variants
    fn enum_variants(
        &'ctx self,
        variants: &[EnumVariant],
        symbol_idx: SymbolIndex,
    ) -> Vec<SymbolIndex> {
        let mut temp_variants: Vec<TemporaryVariantDetails> = vec![];
        for (index, variant) in variants.into_iter().enumerate() {
            // Block multiple variants with the same name.
            if temp_variants
                .iter()
                .find(|temp| temp.name == variant.name.name)
                .is_some()
            {
                self.add_ctx_error(errors::duplicate_enum_variant(variant.name.to_owned()));
                continue;
            }
            let symbol = SemanticSymbol {
                name: variant.name.name.to_owned(),
                symbol_kind: SemanticSymbolKind::Variant {
                    owner_enum: symbol_idx,
                    variant_index: index,
                    tagged_types: variant
                        .tagged_types
                        .iter()
                        .map(|tagged_type_exp| self.type_expression(tagged_type_exp))
                        .collect(),
                },
                references: vec![SymbolReferenceList {
                    module_path: self.path_idx(),
                    starts: vec![variant.span.start],
                }],
                doc_info: None, // todo: doc info for variants.
                origin_span: variant.span,
            };
            let symbol_idx = self.symbol_table().add(symbol);
            self.known_values().insert(variant.name.span, symbol_idx);
            // store for next variant check.
            temp_variants.push(TemporaryVariantDetails {
                name: variant.name.name.to_owned(),
                index: symbol_idx,
            });
        }
        temp_variants.into_iter().map(|temp| temp.index).collect()
    }
}

// Expressions.
impl<'ctx> Binder<'ctx> {
    // Binds an expression.
    fn expression(&'ctx self, expression: Expression) -> TypedExpr {
        match expression {
            Expression::Identifier(identfier) => TypedExpr::Ident(self.identifier(identfier)),
            Expression::StringLiteral(string) => TypedExpr::Literal(self.string(string)),
            Expression::NumberLiteral(number) => TypedExpr::Literal(self.number(number)),
            Expression::BooleanLiteral(boolean) => TypedExpr::Literal(self.boolean(boolean)),
            Expression::NewExpr(new_expr) => {
                TypedExpr::NewExpr(Box::new(self.new_expression(new_expr)))
            }
            Expression::ThisExpr(this) => TypedExpr::ThisExpr(self.this_expression(this)),
            Expression::CallExpr(call) => TypedExpr::CallExpr(Box::new(self.call_expression(call))),
            Expression::FnExpr(fnexpr) => {
                TypedExpr::FnExpr(Box::new(self.function_expression(*fnexpr)))
            }
            Expression::IfExpr(_) => todo!(),
            Expression::ArrayExpr(_) => todo!(),
            Expression::AccessExpr(_) => todo!(),
            Expression::IndexExpr(_) => todo!(),
            Expression::BinaryExpr(binexp) => TypedExpr::BinaryExpr(self.binary_expression(binexp)),
            Expression::AssignmentExpr(_) => todo!(),
            Expression::UnaryExpr(_) => todo!(),
            Expression::LogicExpr(_) => todo!(),
            Expression::UpdateExpr(_) => todo!(),
            Expression::BlockExpr(_) => todo!(),
        }
    }
    /// Bind an identifier.
    fn identifier(&'ctx self, identifier: Identifier) -> TypedIdent {
        let symbol_index = self.find_or_create(&identifier, false);
        let ref_number = self.last_reference_no(symbol_index);
        TypedIdent {
            value: SymbolLocator {
                symbol_idx: symbol_index,
                ref_number,
            },
        }
    }
    /// Bind a new expression.
    fn new_expression(&'ctx self, new_expr: Box<ast::NewExpr>) -> TypedNewExpr {
        TypedNewExpr {
            value: self.expression(new_expr.value),
            span: new_expr.span,
        }
    }
    /// Bind a `this` expression to its meaning.
    fn this_expression(&'ctx self, this: ast::ThisExpr) -> TypedThisExpr {
        let shadow = self.module().ambience.create_shadow(self.scope());
        let symbol_index = match shadow.get_method_context() {
            // find the referenced model or trait.
            Some(search) => Some(self.find_or_create(search.entry.ident().unwrap(), false)),
            // 'this' is being used outside of a model or trait.
            None => {
                self.add_ctx_error(errors::this_outside_method(this.span));
                None
            }
        };
        TypedThisExpr {
            model_or_trait: symbol_index,
            start_line: this.span.start[0],
            start_character: this.span.start[1],
        }
    }
    /// Bind a call expression.
    fn call_expression(&'ctx self, call: Box<ast::CallExpr>) -> TypedCallExpr {
        TypedCallExpr {
            caller: self.expression(call.caller),
            arguments: call
                .arguments
                .into_iter()
                .map(|argument| self.expression(argument))
                .collect(),
        }
    }
    /// Bind a function expression.
    fn function_expression(&'ctx self, func: FunctionExpr) -> TypedFnExpr {
        // bind generic parameters.
        self.push_generic_pool();
        let generic_params = self.generic_parameters(func.generic_params.as_ref());
        let return_type = func
            .return_type
            .map(|ref rettype| self.type_expression(rettype));
        // If the expression is a block, all well and good.
        // if the expression is anything else, it needs to be scoped to a block so that the parameter values have meaning and scope.
        let function_expr = match func.body {
            Expression::BlockExpr(block) => {
                let (body, params) = self.function_block(block, func.params);
                TypedFnExpr {
                    is_async: func.is_async,
                    generic_params,
                    params,
                    return_type,
                    body: TypedExpr::Block(body),
                    span: func.span,
                }
            }
            body => {
                self.module().ambience.enter(ScopeType::Functional);
                let fake_scope_id = self.module().ambience.current_scope();
                let fakeblock = Block {
                    scope_id: fake_scope_id,
                    statements: vec![Statement::FreeExpression(body)],
                    span: Span::default(), // unecessary span.
                };
                let (mut block, params) = self.function_block(fakeblock, func.params);
                self.module().ambience.jump_to_scope(fake_scope_id);
                self.module().ambience.leave_scope();
                TypedFnExpr {
                    is_async: func.is_async,
                    generic_params,
                    params,
                    return_type,
                    body: match block.statements.remove(0) {
                        TypedStmnt::FreeExpression(expression) => expression,
                        _ => unreachable!(
                            "Did not enclose the expression of a block as an expression."
                        ),
                    },
                    span: func.span,
                }
            }
        };
        self.pop_generic_pool();
        function_expr
    }
    /// Bind a block.
    fn block(&'ctx self, block: Block) -> TypedBlock {
        let prior_scope = self.scope();
        self.set_scope(block.scope_id);
        let block = TypedBlock {
            statements: block
                .statements
                .into_iter()
                .map(|statement| self.statement(statement))
                .collect(),
            return_type: EvaluatedType::unknown(),
            span: block.span,
        };
        self.set_scope(prior_scope);
        return block;
    }
    /// Binds a binary expression.
    fn binary_expression(&'ctx self, binexp: Box<ast::BinaryExpr>) -> Box<TypedBinExpr> {
        Box::new(TypedBinExpr {
            left: self.expression(binexp.left),
            operator: binexp.operator,
            right: self.expression(binexp.right),
            span: binexp.span,
        })
    }
}

// Types
impl<'ctx> Binder<'ctx> {
    /// Binds a type expression.
    fn type_expression(&'ctx self, type_exp: &TypeExpression) -> IntermediateType {
        match type_exp {
            TypeExpression::Union(union_type) => IntermediateType::UnionType {
                types: union_type
                    .types
                    .iter()
                    .map(|type_exp| self.type_expression(type_exp))
                    .collect(),
                span: union_type.span,
            },
            TypeExpression::Functional(_) => todo!(),
            TypeExpression::Member(_) => todo!(),
            TypeExpression::Discrete(discrete_type) => self.discrete_type(discrete_type),
            TypeExpression::This { span } => IntermediateType::This {
                meaning: self.this_type.borrow().last().copied(),
                span: *span,
            },
            TypeExpression::BorrowedType(borrowedtype) => IntermediateType::BorrowedType {
                value: Box::new(self.type_expression(&borrowedtype.value)),
                span: borrowedtype.span,
            },
            TypeExpression::Invalid => IntermediateType::Placeholder,
        }
    }
    /// Bind a discrete type.
    fn discrete_type(&'ctx self, discrete_type: &ast::DiscreteType) -> IntermediateType {
        let symbol_index = self.find_or_create(&discrete_type.name, true);
        let mut generic_args = vec![];
        if let Some(ref arguments) = discrete_type.generic_args {
            for argument in arguments {
                generic_args.push(self.type_expression(argument))
            }
        }
        IntermediateType::SimpleType {
            value: symbol_index,
            generic_args,
            span: discrete_type.span,
        }
    }
    /// Bind an optional list of generic parameters.
    fn generic_parameters(
        &'ctx self,
        param_list: Option<&Vec<GenericParameter>>,
    ) -> Vec<SymbolIndex> {
        match param_list {
            Some(ref params) => params
                .iter()
                .map(|parameter| self.bind_generic_parameter(parameter))
                .collect(),
            None => vec![],
        }
    }
    /// Bind a generic parameter.
    fn bind_generic_parameter(&'ctx self, parameter: &GenericParameter) -> SymbolIndex {
        // Check if a generic parameter with this name already exists in this pool.
        if let Some(index) = self.lookaround_for_generic_parameter(&parameter.name.name) {
            self.add_ctx_error(errors::duplicate_generic_parameter(
                parameter.name.to_owned(),
            ));
            let symbol = self.symbol_table().get_mut(index).unwrap();
            symbol.add_reference(self.path_idx(), parameter.span);
            return index;
        }
        let symbol = SemanticSymbol {
            name: parameter.name.name.to_owned(),
            symbol_kind: SemanticSymbolKind::GenericParameter {
                traits: parameter
                    .traits
                    .iter()
                    .map(|_trait| self.type_expression(_trait))
                    .collect(),
                default_value: parameter
                    .default
                    .as_ref()
                    .map(|default_value| self.type_expression(default_value)),
            },
            references: vec![SymbolReferenceList {
                module_path: self.path_idx(),
                starts: vec![parameter.span.start],
            }],
            doc_info: None, // todo. Collect documentation for generic params.
            origin_span: parameter.span,
        };
        let index = self.symbol_table().add(symbol);
        self.add_generic_parameter(&parameter.name.name, index);
        self.known_values().insert(parameter.name.span, index);
        index
    }
}

fn span_from_start(start: [u32; 2], width: &str) -> Span {
    Span::from([start, [start[0], start[1] + width.len() as u32]])
}