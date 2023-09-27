mod programerror;

use std::{cell::RefCell, collections::HashMap, mem::take, path::PathBuf};

use crate::{
    IntermediateType, Literal, LiteralIndex, Module, ModuleGraph, PathIndex, SemanticSymbol,
    SemanticSymbolKind, SemanticSymbolReferenceList, SymbolIndex, SymbolLocator, SymbolTable,
    TypedConstantDeclaration, TypedExpr, TypedIdent, TypedModule,
    TypedShorthandVariableDeclaration, TypedStmnt,
};
pub use programerror::*;
use whirl_ast::{
    ConstantDeclaration, Expression, Identifier, ScopeEntry, ScopeSearch,
    ShorthandVariableDeclaration, Span, Statement, TypeExpression,
};
use whirl_errors::ContextError;

/// The binder takes each module and joins all its symbols to their original declaration.
/// It does this by updating a symbol table with semantic entries and returning the index to be stored by the referrer.
/// After this phase, all variables are no longer strings, but indexes to the original declarations.
///
/// It also handles errors like block scoped variable redeclaration, use of undeclared variables, etc.
pub struct Binder<'ctx> {
    /// The module to bind.
    module: RefCell<Module>,
    /// The (emptied) module.
    graph: &'ctx ModuleGraph,
    paths: &'ctx mut Vec<PathBuf>,
    /// The context symbol table.
    symbol_table: RefCell<&'ctx mut SymbolTable>,
    /// The context errors.
    errors: RefCell<&'ctx mut Vec<ProgramError>>,
    /// Bound values. The span is the closest thing to a unique marker for each symbol identifier.
    known_values: RefCell<HashMap<Span, SymbolIndex>>,
    /// Values that are undeclared in the scope tree.
    unknown_values: RefCell<HashMap<(String, usize), SymbolIndex>>,
    literals: RefCell<&'ctx mut Vec<Literal>>,
    /// The current scope in which to search for a match.
    current_scope: RefCell<usize>,
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
            graph,
            paths,
            symbol_table: RefCell::new(symbol_table),
            errors: RefCell::new(errors),
            known_values: RefCell::new(HashMap::new()),
            unknown_values: RefCell::new(HashMap::new()),
            literals: RefCell::new(literals),
            current_scope: RefCell::new(0),
        }
    }
    /// Takes a module and converts it to its typed equivalent.
    pub fn bind(&'ctx mut self) -> Option<TypedModule> {
        let path = PathIndex(self.paths.len());
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
}

impl<'ctx> Binder<'ctx> {
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
        PathIndex(self.paths.len() - 1)
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
    fn find_or_create(&'ctx self, name: &Identifier) -> SymbolIndex {
        let known_values = self.known_values();
        let shadow = self.module().ambience.create_shadow(self.scope());
        match shadow.lookup(&name.name) {
            Some(ScopeSearch { entry, .. }) => {
                match known_values.get(&entry.ident().unwrap().span) {
                    // Entry has been added the symbol table.
                    Some(index) => {
                        // new reference to this symbol.
                        self.symbol_table()
                            .get_mut(*index)
                            .unwrap()
                            .add_reference(self.path_idx(), name.span);
                        *index
                    }
                    // Entry exists but it has not been added to the symbol table yet.
                    // Pause the current binding and jump to bind that symbol.
                    // It binds the symbol with a placeholder declaration range
                    // that will be overwritten when the actual declaration is encountered.
                    // If the symbol is a constant or a variable, then this process will result in a context error later on.
                    None => self.bind_signature(entry, Span::default()),
                }
            }
            None => {
                // Entry does not exist.
                self.add_ctx_error(whirl_errors::unknown_value(name.name.to_owned(), name.span));
                let shadow = self.module().ambience.create_shadow(self.scope());
                let mut unknown_values = self.unknown_values.borrow_mut();
                // If there is another unknown symbol being tracked in the same or a parent scope, equate the two symbols.
                // Else, add new undeclared symbol to table and unknown values map.
                for ((unknown_name, scope), symbol_index) in unknown_values.iter() {
                    if unknown_name == &name.name && shadow.is_inclusive_child_of(*scope) {
                        return *symbol_index;
                    }
                }
                let mut new_symbol = SemanticSymbol {
                    name: name.name.to_owned(),
                    symbol_kind: SemanticSymbolKind::UndeclaredValue,
                    references: vec![],
                    doc_info_range: None,
                    origin_span: name.span,
                };
                new_symbol.add_reference(self.path_idx(), name.span);
                let index = self.symbol_table().add(new_symbol);
                unknown_values.insert((name.name.to_owned(), self.scope()), index);
                return index;
            }
        }
    }
    /// Create a binding for a bariable signature.
    fn bind_signature(&'ctx self, entry: &ScopeEntry, origin_span: Span) -> SymbolIndex {
        match entry {
            ScopeEntry::Function(_) => todo!(),
            ScopeEntry::Type(_type) => {
                let mut symbol = SemanticSymbol::from_type(_type, origin_span);
                todo!()
            }
            ScopeEntry::Model(_) => todo!(),
            ScopeEntry::Enum(_) => todo!(),
            ScopeEntry::Variable(variable) => {
                let mut symbol = SemanticSymbol::from_variable(variable, origin_span);
                // Add first reference, to its declaration.
                symbol.references.push(SemanticSymbolReferenceList {
                    module_path: self.path_idx(),
                    starts: vec![variable.name.span.start],
                });
                let index = self.symbol_table().add(symbol);
                let span = entry.ident().unwrap().span;
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
                let mut symbol = SemanticSymbol::from_constant(constant, origin_span);
                // Add first reference, to its declaration.
                symbol.references.push(SemanticSymbolReferenceList {
                    module_path: self.path_idx(),
                    starts: vec![constant.name.span.start],
                });
                let index = self.symbol_table().add(symbol);
                self.known_values()
                    .insert(entry.ident().unwrap().span, index);
                // Add type. Hackery to prevent recursive real-name-is-also-type-name loops.
                if let SemanticSymbolKind::Constant { declared_type, .. } =
                    &mut self.symbol_table().get_mut(index).unwrap().symbol_kind
                {
                    *declared_type = self.type_expression(&constant.var_type)
                }
                index
            }
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
                        self.add_ctx_error(whirl_errors::use_before_declare(
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

    fn error_and_return_first_instance(
        &'ctx self,
        entry_1: &ScopeEntry,
        entry_2: &ScopeEntry,
    ) -> SymbolIndex {
        self.add_ctx_error(whirl_errors::already_declared_in_scope(
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

// Literals.
impl<'ctx> Binder<'ctx> {
    /// Bind a string.
    fn string(&'ctx self, value: whirl_ast::WhirlString) -> LiteralIndex {
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
    fn number(&self, value: whirl_ast::WhirlNumber) -> LiteralIndex {
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
    fn boolean(&self, value: whirl_ast::WhirlBoolean) -> LiteralIndex {
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
            Statement::VariableDeclaration => todo!(),
            Statement::ShorthandVariableDeclaration(shorthandvariable) => {
                TypedStmnt::ShorthandVariableDeclaration(
                    self.shorthand_variable_declaration(shorthandvariable),
                )
            }
            Statement::ConstantDeclaration(constant) => {
                TypedStmnt::ConstantDeclaration(self.constant_declaration(constant))
            }
            Statement::ModelDeclaration(_) => todo!(),
            Statement::ModuleDeclaration(_) => todo!(),
            Statement::FunctionDeclaration(_) => todo!(),
            Statement::RecordDeclaration => todo!(),
            Statement::TraitDeclaration(_) => todo!(),
            Statement::EnumDeclaration(_) => todo!(),
            Statement::TypeDeclaration(_) => todo!(),
            Statement::WhileStatement(_) => todo!(),
            Statement::ReturnStatement(_) => todo!(),
            Statement::ForStatement => todo!(),
            Statement::ExpressionStatement(_) => todo!(),
            Statement::FreeExpression(expression) => {
                TypedStmnt::FreeExpression(self.expression(expression))
            }
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
        let ambience = &self.module().ambience;
        let entry = ambience.get_entry_unguarded(shorthand.address);
        // Check if entry already exists.
        let shadow = &ambience.create_shadow(self.scope());
        let symbol_idx = if let Some(search) = shadow.lookaround(entry.name()) {
            // There is a clashing name in the same scope.
            // The first name will always take precedence.
            if !std::ptr::eq(search.entry, entry) {
                self.error_and_return_first_instance(search.entry, entry)
            } else {
                // No clashes in the current scope,
                // create new symbol for this variable.
                self.maybe_bound(entry, true, shorthand.span)
                    .unwrap_or_else(|| self.bind_signature(entry, shorthand.span))
            }
        } else {
            unreachable!("Mismatched scope address. How is that possible?")
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
        let ambience = &self.module().ambience;
        let entry = ambience.get_entry_unguarded(constant.address);
        // Check if entry already exists.
        let shadow = &ambience.create_shadow(self.scope());
        let symbol_idx = if let Some(search) = shadow.lookaround(entry.name()) {
            // There is a clashing name in the same scope.
            // The first name will always take precedence.
            if !std::ptr::eq(search.entry, entry) {
                self.error_and_return_first_instance(search.entry, entry)
            } else {
                // There is no other scope-bound instance of this name that is not equal to this signature.
                // If the symbol has already been used before, equate the instances and cause an error
                // (i.e. constant is being used before its declaration.)
                self.maybe_bound(&entry, true, constant.span)
                    // No clashes in the current scope, and no previous usage.
                    // create new symbol for this constant.
                    .unwrap_or_else(|| self.bind_signature(entry, constant.span))
            }
        } else {
            unreachable!("Mismatched scope address. How is that possible?")
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
            Expression::NewExpr(_) => todo!(),
            Expression::ThisExpr(_) => todo!(),
            Expression::CallExpr(_) => todo!(),
            Expression::FnExpr(_) => todo!(),
            Expression::IfExpr(_) => todo!(),
            Expression::ArrayExpr(_) => todo!(),
            Expression::AccessExpr(_) => todo!(),
            Expression::IndexExpr(_) => todo!(),
            Expression::BinaryExpr(_) => todo!(),
            Expression::AssignmentExpr(_) => todo!(),
            Expression::UnaryExpr(_) => todo!(),
            Expression::LogicExpr(_) => todo!(),
            Expression::UpdateExpr(_) => todo!(),
            Expression::BlockExpr(_) => todo!(),
        }
    }
    /// Bind an identifier.
    fn identifier(&'ctx self, identifier: Identifier) -> TypedIdent {
        let symbol_index = self.find_or_create(&identifier);
        let ref_number = self.last_reference_no(symbol_index);
        TypedIdent {
            value: SymbolLocator {
                symbol_idx: symbol_index,
                ref_number,
            },
        }
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
            TypeExpression::This { .. } => todo!(),
            TypeExpression::Invalid => IntermediateType::Placeholder,
        }
    }
    /// Bind a discrete type.
    fn discrete_type(&'ctx self, discrete_type: &whirl_ast::DiscreteType) -> IntermediateType {
        let symbol_index = self.find_or_create(&discrete_type.name);
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
}

fn span_from_start(start: [u32; 2], width: &str) -> Span {
    Span::from([start, [start[0], start[1] + width.len() as u32]])
}
