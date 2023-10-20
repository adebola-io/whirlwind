mod typed_expr;
mod typed_module;
mod typed_statement;

use crate::{
    EvaluatedType, IntermediateType, Literal, LiteralIndex, Module, PathIndex, ProgramError,
    ProgramErrorType, SemanticSymbol, SemanticSymbolKind, SymbolIndex, SymbolLocator,
    SymbolReferenceList, SymbolTable,
};
use ast::{
    Block, ConstantDeclaration, EnumDeclaration, EnumVariant, Expression, FunctionExpr,
    GenericParameter, Identifier, ModelBody, ModelDeclaration, ModelProperty, ModelPropertyType,
    ModuleAmbience, Parameter, ReturnStatement, ScopeAddress, ScopeEntry, ScopeType,
    ShorthandVariableDeclaration, Span, Statement, TestDeclaration, TypeDeclaration,
    TypeExpression, UseDeclaration, UseTarget, WhileStatement, WhirlString,
};
use errors::ContextError;
use std::{cell::RefCell, collections::HashMap, mem::take, vec};
pub use typed_expr::*;
pub use typed_module::*;
pub use typed_statement::*;

/// The binder takes each module and joins all its symbols to their original declaration.
/// It does this by updating a symbol table with semantic entries and returning the index to be stored by the referrer.
/// After this phase, all variables are no longer strings, but indexes to the original declarations.
///
/// It also handles errors like block scoped variable redeclaration, use of undeclared variables, etc.
pub struct Binder {
    path: PathIndex,
    /// Bound values. The span is the closest thing to a unique marker for each symbol identifier.
    known_values: HashMap<Span, SymbolIndex>,
    /// Values that are undeclared in the scope tree.
    unknown_values: HashMap<(String, usize), SymbolIndex>,
    /// The imported symbols into the module, and the target that shows their full path.
    imported_values: Vec<(UseTarget, Vec<SymbolIndex>)>,
    /// Symbols in this module.
    module_symbols: Vec<SymbolIndex>,
    /// Generic parameter values.
    generic_pools: RefCell<Vec<Vec<(String, SymbolIndex)>>>,
    /// The current scope in which to search for a match.
    current_scope: usize,
    /// The model `This` currently refers to.
    this_type: Vec<SymbolIndex>,
    /// The span of the module declaration inside this module.
    module_decl_span: Option<Span>,
    // The index of the core library module, if it is allowed.
    corelib_symbol_idx: Option<SymbolIndex>,
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

/// Takes a module and converts it to its typed equivalent.
pub fn bind(
    // The module name (in case the module is anonymous)
    module_name: String,
    // The module to bind.
    mut module: Module,
    // The reserved module path.
    path_idx: PathIndex,
    // The context symbol table.
    symbol_table: &mut SymbolTable,
    // The context errors.
    errors: &mut Vec<ProgramError>,
    // Literal values.
    literals: &mut Vec<Literal>,
    // The symbol index of the core library.
    corelib_symbol_idx: Option<SymbolIndex>,
) -> Option<TypedModule> {
    let mut binder = Binder::new(path_idx, corelib_symbol_idx);
    bind_utils::collect_prior_errors(path_idx, &mut module, errors);
    let line_lengths = module._line_lens;
    let path_buf = module.module_path?;

    let mut statements = vec![];
    let module_statements = take(&mut module.statements);
    for statement in module_statements {
        statements.push(statements::bind_statement(
            statement,
            &mut binder,
            symbol_table,
            errors,
            literals,
            &mut module.ambience,
        ));
    }

    let origin_span = binder.module_decl_span.unwrap_or_else(|| Span::default());
    let module_symbol = SemanticSymbol {
        name: module_name,
        kind: SemanticSymbolKind::Module {
            parent_modules: vec![],
            imports: binder
                .imported_values
                .iter()
                .map(|tuple| tuple.1.iter())
                .flatten()
                .map(|idx| *idx)
                .collect(),
            symbols: binder.module_symbols,
        },
        references: vec![SymbolReferenceList {
            module_path: path_idx,
            starts: vec![
                module
                    .ambience
                    .module_name
                    .map(|name| name.span)
                    .unwrap_or_default()
                    .start,
            ],
        }],
        doc_info: module.ambience.module_info,
        origin_span,
    };
    let symbol_idx = symbol_table.add(module_symbol);
    Some(TypedModule {
        path_idx,
        path_buf,
        symbol_idx,
        line_lengths,
        statements,
        imports: binder.imported_values,
    })
}

mod bind_utils {
    use super::*;

    /// Bind an entry within a scope.
    pub fn handle_scope_entry(
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
        address: ScopeAddress,
        span: Span,
        allow_hoisting: bool,
    ) -> Result<SymbolIndex, SymbolIndex> {
        let entry = ambience.get_entry_unguarded(address);
        // Check if entry already exists.
        let shadow = ambience.create_shadow(binder.current_scope);
        let symbol_idx = if let Some(search) = shadow.lookaround(entry.name()) {
            // There is a clashing name in the same scope.
            // The first name will always take precedence.
            if !std::ptr::eq(search.entry, entry) {
                Err(error_and_return_first_instance(
                    &binder,
                    symbol_table,
                    errors,
                    search.entry,
                    entry,
                ))
            } else {
                // There is no other scope-bound instance of this name that is not equal to this signature.
                // If the symbol has already been used before, equate the instances.
                // If the symbol cannot be hoisted, e.g. a constant or a variable, cause an error.
                Ok(
                    maybe_bound(&binder, symbol_table, errors, entry, !allow_hoisting, span)
                        // No clashes in the current scope, and no previous usage.
                        // create new symbol for this constant.
                        .unwrap_or_else(|| {
                            bind_signature(binder, symbol_table, errors, ambience, entry, span)
                        }),
                )
            }
        } else {
            unreachable!("Mismatched scope address. How is that possible?")
        };
        // mark a global value.
        if shadow.is_in_global_scope() && symbol_idx.is_ok() {
            let symbol_idx = symbol_idx.unwrap();
            binder.module_symbols.push(symbol_idx);
            Ok(symbol_idx)
        } else {
            symbol_idx
        }
    }

    /// Pushes an error to the list of context errors.
    pub fn add_ctx_error(binder: &Binder, errors: &mut Vec<ProgramError>, error: ContextError) {
        errors.push(ProgramError::contextual(binder.path, error))
    }

    /// Add an error for a duplicate declaration, and return the symbol index of the first declaration symbol.
    pub fn error_and_return_first_instance(
        binder: &Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        entry_1: &ScopeEntry,
        entry_2: &ScopeEntry,
    ) -> SymbolIndex {
        add_ctx_error(
            binder,
            errors,
            errors::already_declared_in_scope(
                entry_1.name().to_owned(),
                entry_2.ident().unwrap().span,
            ),
        );
        let span = entry_1.ident().unwrap().span;
        let symbol_index = *(binder.known_values.get(&span).unwrap());
        // Add reference to this.
        symbol_table
            .get_mut(symbol_index)
            .unwrap()
            .add_reference(binder.path, entry_2.ident().unwrap().span);
        // return index of former declaration.
        symbol_index
    }

    /// Check if a symbol has been jumped to already.
    pub fn maybe_bound(
        binder: &Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        entry: &ScopeEntry,
        should_cause_error: bool,
        declaration_range: Span,
    ) -> Option<SymbolIndex> {
        let span = entry.ident()?.span;
        // Value has already been added to symbol table before its declaration was encountered,
        if let Some(symbolindex) = binder.known_values.get(&span) {
            let symbol = symbol_table.get(*symbolindex).unwrap();
            if should_cause_error {
                symbol
                    .references
                    .iter()
                    .filter(|reference| reference.module_path == binder.path)
                    .map(|reference| reference.starts.iter())
                    .flatten()
                    .for_each(|start| {
                        add_ctx_error(
                            binder,
                            errors,
                            errors::use_before_declare(
                                entry.name().to_owned(),
                                Span::on_line(*start, entry.name().len() as u32),
                            ),
                        )
                    });
            }
            let symbol = symbol_table.get_mut(*symbolindex).unwrap();
            // Mark new found declaration range.
            symbol.origin_span = declaration_range;
            return Some(*symbolindex);
        }
        return None;
    }

    /// Create a binding for a signature from a scope entry.
    pub fn bind_signature(
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
        entry: &ScopeEntry,
        origin_span: Span,
    ) -> SymbolIndex {
        match entry {
            ScopeEntry::Function(function) => {
                let symbol = SemanticSymbol::from_function(function, binder.path, origin_span);
                let index = symbol_table.add(symbol);
                let span = function.name.span;
                binder.known_values.insert(span, index);
                // Return type, parameters and generic parameters are bound at declaration to prevent recursive loops.
                index
            }
            ScopeEntry::Type(_type) => {
                let symbol = SemanticSymbol::from_type(_type, binder.path, origin_span);
                let index = symbol_table.add(symbol);
                let span = _type.name.span;
                binder.known_values.insert(span, index);
                // Type value and generic parameters are bound at declaration to prevent recursive loops.
                index
            }
            ScopeEntry::Model(model) => {
                let symbol = SemanticSymbol {
                    // taking the string will make the entry un-lookup-able.
                    name: model.name.name.to_owned(),
                    kind: SemanticSymbolKind::Model {
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
                        module_path: binder.path,
                        starts: vec![model.name.span.start],
                    }],
                    doc_info: model.info.clone(), // todo.
                    origin_span,
                };
                let index = symbol_table.add(symbol);
                let span = model.name.span;
                binder.known_values.insert(span, index);
                index
            }
            ScopeEntry::Enum(_enum) => {
                let symbol = SemanticSymbol::from_enum(_enum, binder.path, origin_span);
                let index = symbol_table.add(symbol);
                let span = _enum.name.span;
                binder.known_values.insert(span, index);
                index
            }
            ScopeEntry::ShorthandVariable(variable) => {
                let symbol =
                    SemanticSymbol::from_shorthand_variable(variable, binder.path, origin_span);
                let index = symbol_table.add(symbol);
                let span = variable.name.span;
                binder.known_values.insert(span, index);
                // Add type. Hackery to prevent recursive real-name-is-also-type-name loops.
                let declared_type_idx = variable.var_type.as_ref().map(|type_expr| {
                    types::bind_type_expression(type_expr, binder, symbol_table, errors, ambience)
                });
                if let SemanticSymbolKind::Variable { declared_type, .. } =
                    &mut symbol_table.get_mut(index).unwrap().kind
                {
                    *declared_type = declared_type_idx;
                }
                index
            }
            ScopeEntry::Trait(_) => todo!(),
            ScopeEntry::UseImport(use_signature) => {
                // Full path is added later.
                let symbol =
                    SemanticSymbol::from_use_import(use_signature, binder.path, origin_span);
                let index = symbol_table.add(symbol);
                let span = use_signature.name.span;
                binder.known_values.insert(span, index);
                index
            }
            ScopeEntry::Parameter(_) => {
                unreachable!("Encountered an unbound parameter while binding.")
            }
            ScopeEntry::ReservedSpace => {
                unreachable!("Encountered a reserved space while binding.")
            }
            ScopeEntry::Constant(constant) => {
                let symbol = SemanticSymbol::from_constant(constant, binder.path, origin_span);
                let index = symbol_table.add(symbol);
                let span = constant.name.span;
                binder.known_values.insert(span, index);
                let declared_const_type = types::bind_type_expression(
                    &constant.var_type,
                    binder,
                    symbol_table,
                    errors,
                    ambience,
                );
                // Add type. Hackery to prevent recursive real-name-is-also-type-name loops.
                if let SemanticSymbolKind::Constant { declared_type, .. } =
                    &mut symbol_table.get_mut(index).unwrap().kind
                {
                    *declared_type = declared_const_type;
                }
                index
            }
            ScopeEntry::Variable(_) => todo!(),
            ScopeEntry::LoopVariable(_) => todo!(),
            ScopeEntry::LoopLabel(_) => todo!(),
        }
    }

    /// Finds an already existing symbol or creates a new one.
    pub fn find_or_create(
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
        name: &Identifier,
        in_type_context: bool,
    ) -> SymbolIndex {
        let shadow = ambience.create_shadow(binder.current_scope);
        let index = (|| {
            // generic parameters take precedence over values if in type context.
            if in_type_context {
                if let Some(index) = binder.lookup_generic_parameter(&name.name) {
                    return index;
                }
            }
            match shadow.lookup(&name.name) {
                Some(entry) => {
                    let entry = &entry.entry;
                    match binder.known_values.get(&entry.ident().unwrap().span) {
                        // Entry has been added the symbol table.
                        Some(index) => *index,
                        // Entry exists but it has not been added to the symbol table yet.
                        // Pause the current binding and jump to bind that symbol.
                        // It binds the symbol with a placeholder declaration range,
                        // that will be overwritten when the actual declaration is encountered.
                        // If the symbol is a constant or a variable,
                        // then this process will result in a context error (using variable before declaration) later on.
                        None => bind_signature(
                            binder,
                            symbol_table,
                            errors,
                            ambience,
                            entry,
                            Span::default(),
                        ),
                    }
                }
                None => {
                    // Try to see if there is a generic parameter with this name.
                    if let Some(index) = binder.lookup_generic_parameter(&name.name) {
                        return index;
                    }
                    // Is it the Core library?
                    if name.name == "Core" {
                        if let Some(symbol_idx) = binder.corelib_symbol_idx {
                            return symbol_idx;
                        }
                    }
                    // Entry does not exist.
                    add_ctx_error(
                        binder,
                        errors,
                        errors::unknown_value(name.name.to_owned(), name.span),
                    );
                    let shadow = ambience.create_shadow(binder.current_scope);
                    // If there is another unknown symbol with the same name being tracked in the same or a parent scope, equate the two symbols.
                    // Else, add new undeclared symbol to table and unknown values map.
                    for ((unknown_name, scope), symbol_index) in binder.unknown_values.iter() {
                        if unknown_name == &name.name && shadow.is_inclusive_child_of(*scope) {
                            return *symbol_index;
                        }
                    }
                    let new_symbol = SemanticSymbol {
                        name: name.name.to_owned(),
                        kind: SemanticSymbolKind::UndeclaredValue,
                        references: vec![],
                        doc_info: None,
                        origin_span: name.span,
                    };
                    let index = symbol_table.add(new_symbol);
                    binder
                        .unknown_values
                        .insert((name.name.to_owned(), binder.current_scope), index);
                    return index;
                }
            }
        })();
        // Add this reference.
        let symbol = symbol_table.get_mut(index).unwrap();
        symbol.add_reference(binder.path, name.span);
        return index;
    }

    /// Returns the reference number for the last reference added to the symbol.
    pub fn last_reference_no(symbol_table: &SymbolTable, symbol_idx: SymbolIndex) -> usize {
        symbol_table.get(symbol_idx).unwrap().references.len() - 1
    }

    /// Collect the import, syntax and lexing errors,
    pub fn collect_prior_errors(
        path: PathIndex,
        module: &mut Module,
        errors: &mut Vec<ProgramError>,
    ) {
        // Collect lexical errors.
        let l_errors = take(&mut module.lexical_errors);
        l_errors.into_iter().for_each(|lex_error| {
            errors.push(ProgramError {
                offending_file: path,
                error_type: ProgramErrorType::Lexical(lex_error),
            })
        });
        // Collect syntax errors.
        let p_errors = take(&mut module.syntax_errors);
        p_errors.into_iter().for_each(|parse_error| {
            errors.push(ProgramError {
                offending_file: path,
                error_type: ProgramErrorType::Syntax(parse_error),
            })
        });
        // Collect import errors.
        let i_errors = take(&mut module.import_errors);
        i_errors.into_iter().for_each(|import_error| {
            errors.push(ProgramError {
                offending_file: path,
                error_type: ProgramErrorType::Importing(import_error),
            })
        });
    }
}

/// Statements
mod statements {
    use super::{
        bind_utils::{add_ctx_error, handle_scope_entry, last_reference_no},
        expressions::{bind_block, bind_expression},
        literals::bind_string,
        types::{bind_generic_parameters, bind_type_expression},
        *,
    };

    // Bind a statement.
    pub fn bind_statement(
        statement: Statement,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedStmnt {
        match statement {
            Statement::TestDeclaration(test) => TypedStmnt::TestDeclaration(bind_test_declaration(
                test,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            )),
            Statement::UseDeclaration(usedecl) => TypedStmnt::UseDeclaration(bind_use_declaration(
                usedecl,
                binder,
                symbol_table,
                errors,
                ambience,
            )),
            Statement::ShorthandVariableDeclaration(shorthand) => {
                TypedStmnt::ShorthandVariableDeclaration(bind_shorthand_variable_declaration(
                    shorthand,
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                ))
            }
            Statement::ConstantDeclaration(constant) => {
                TypedStmnt::ConstantDeclaration(bind_constant_declaration(
                    constant,
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                ))
            }
            Statement::ModelDeclaration(model) => TypedStmnt::ModelDeclaration(
                bind_model_declaration(model, binder, symbol_table, errors, literals, ambience),
            ),
            Statement::ModuleDeclaration(module) => {
                binder.module_decl_span = Some(module.span);
                TypedStmnt::ModuleDeclaration(TypedModuleDeclaration { span: module.span })
            }
            Statement::FunctionDeclaration(func) => TypedStmnt::FunctionDeclaration(
                function_declaration(func, binder, symbol_table, errors, literals, ambience),
            ),
            Statement::EnumDeclaration(_enum) => TypedStmnt::EnumDeclaration(enum_declaration(
                _enum,
                binder,
                symbol_table,
                errors,
                &ambience,
            )),
            Statement::TypeDeclaration(typedecl) => TypedStmnt::TypeDeclaration(
                bind_type_declaration(typedecl, binder, symbol_table, errors, ambience),
            ),
            Statement::WhileStatement(while_statement) => {
                TypedStmnt::WhileStatement(bind_while_statement(
                    while_statement,
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                ))
            }
            Statement::ReturnStatement(returnstmnt) => {
                TypedStmnt::ReturnStatement(bind_return_statement(
                    returnstmnt,
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                ))
            }
            Statement::ExpressionStatement(expression) | Statement::FreeExpression(expression) => {
                TypedStmnt::FreeExpression(bind_expression(
                    expression,
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                ))
            }
            Statement::BreakStatement(_break) => TypedStmnt::BreakStatement(bind_break_statement(
                _break,
                binder,
                symbol_table,
                errors,
                ambience,
            )),
            Statement::ContinueStatement(cont) => TypedStmnt::ContinueStatement(
                bind_continue_statement(cont, binder, symbol_table, errors, ambience),
            ),
            Statement::RecordDeclaration => todo!(),
            Statement::TraitDeclaration(trait_decl) => {
                TypedStmnt::TraitDeclaration(trait_declaration(trait_decl))
            }
            Statement::ForStatement(for_stat) => TypedStmnt::ForStatement(for_statement(for_stat)),
            Statement::VariableDeclaration(variable) => {
                TypedStmnt::VariableDeclaration(variable_declaration(variable))
            }
        }
    }

    /// Bind a test declaration.
    pub fn bind_test_declaration(
        test: TestDeclaration,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedTestDeclaration {
        TypedTestDeclaration {
            name: bind_string(
                WhirlString {
                    value: test.name,
                    span: test.name_span, // todo: ?????
                },
                binder.path,
                literals,
            ),
            body: bind_block(test.body, binder, symbol_table, errors, literals, ambience),
            span: test.span,
        }
    }

    /// Binds a use declaration.
    pub fn bind_use_declaration(
        usedecl: UseDeclaration,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &mut ModuleAmbience,
    ) -> TypedUseDeclaration {
        let mut imports = vec![];
        for address in usedecl.addresses {
            if let Ok(symbol_idx) = handle_scope_entry(
                binder,
                symbol_table,
                errors,
                ambience,
                address,
                usedecl.span,
                false,
            ) {
                imports.push(symbol_idx);
            };
        }
        let typed_use_decl = TypedUseDeclaration {
            is_public: usedecl.is_public,
            imports: imports
                .iter()
                .map(|import| SymbolLocator {
                    symbol_idx: *import,
                    ref_number: last_reference_no(&symbol_table, *import),
                })
                .collect(),
            span: usedecl.span,
        };
        // Add imported values so they can be resolved later.
        binder.imported_values.push((usedecl.target, imports));
        return typed_use_decl;
    }

    /// Binds a shorthand variable declaration.
    fn bind_shorthand_variable_declaration(
        shorthand: ShorthandVariableDeclaration,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedShorthandVariableDeclaration {
        let symbol_idx = match handle_scope_entry(
            binder,
            symbol_table,
            errors,
            ambience,
            shorthand.address,
            shorthand.span,
            false,
        ) {
            Ok(idx) | Err(idx) => idx,
        };
        let ref_number = bind_utils::last_reference_no(&symbol_table, symbol_idx);
        return TypedShorthandVariableDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            value: bind_expression(
                shorthand.value,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: shorthand.span,
        };
    }

    /// Bind a constant declaration.
    fn bind_constant_declaration(
        constant: ConstantDeclaration,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedConstantDeclaration {
        let symbol_idx = match handle_scope_entry(
            binder,
            symbol_table,
            errors,
            ambience,
            constant.address,
            constant.span,
            false,
        ) {
            Ok(idx) | Err(idx) => idx,
        };
        let ref_number = bind_utils::last_reference_no(&symbol_table, symbol_idx);
        return TypedConstantDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            value: bind_expression(
                constant.value,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: constant.span,
        };
    }

    /// Bind a model declaration.
    fn bind_model_declaration(
        model: ModelDeclaration,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedModelDeclaration {
        let binding_result = handle_scope_entry(
            binder,
            symbol_table,
            errors,
            ambience,
            model.address,
            model.span,
            true, // models can be hoisted.
        );
        let signature = ambience.get_entry_unguarded(model.address).model();
        binder.push_generic_pool(); // List of parameters.
        let generic_param_idxs = bind_generic_parameters(
            signature.generic_params.as_ref(),
            binder,
            symbol_table,
            errors,
            &ambience,
        );
        let implementation_idxs = signature
            .implementations
            .iter()
            .map(|implementation| {
                bind_type_expression(implementation, binder, symbol_table, errors, &ambience)
            })
            .collect();
        let symbol_idx = match binding_result {
            Ok(symbol_idx) => {
                // Add generic parameters, if the binding to this type was successful.
                if let Some(SemanticSymbol {
                    kind:
                        SemanticSymbolKind::Model {
                            is_constructable,
                            implementations,
                            generic_params,
                            ..
                        },
                    ..
                }) = &mut symbol_table.get_mut(symbol_idx)
                {
                    *is_constructable = model.body.constructor.is_some();
                    // Generic parameters should always be first.
                    *generic_params = generic_param_idxs;
                    *implementations = implementation_idxs;
                }
                symbol_idx
            }
            Err(idx) => idx,
        };
        binder.this_type.push(symbol_idx); // Set meaning of `This`.
        let ref_number = bind_utils::last_reference_no(&symbol_table, symbol_idx);
        let typed_model_declaration = TypedModelDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            body: model_body(
                model.body,
                symbol_idx,
                signature.parameters.clone(), // todo:
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: model.span,
        };
        binder.pop_generic_pool();
        binder.this_type.pop(); // Remove meaning of This.
        return typed_model_declaration;
    }

    /// Bind a model body.
    fn model_body(
        body: ModelBody,
        owner_idx: SymbolIndex,
        constructor_params: Option<Vec<Parameter>>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedModelBody {
        TypedModelBody {
            constructor: body.constructor.map(|constructor_block| {
                let (block, parameters) = bind_function_block(
                    constructor_block,
                    constructor_params.unwrap(),
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                );
                TypedModelConstructor { parameters, block }
            }),
            properties: body
                .properties
                .into_iter()
                .map(|property| {
                    bind_model_property(
                        property,
                        owner_idx,
                        binder,
                        symbol_table,
                        errors,
                        literals,
                        ambience,
                    )
                })
                .collect(),
            span: body.span,
        }
    }

    /// Binds a model property.
    fn bind_model_property(
        property: ModelProperty,
        owner: SymbolIndex,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedModelProperty {
        match property._type {
            // bind a model attribute.
            ModelPropertyType::Attribute => bind_model_attribute(
                property.index,
                property.span,
                owner,
                binder,
                symbol_table,
                errors,
                ambience,
            ),
            ModelPropertyType::Method { body } => bind_model_method(
                property.index,
                property.span,
                owner,
                body,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            ModelPropertyType::TraitImpl { .. } => todo!(),
        }
    }

    /// Binds a model attribute.
    pub fn bind_model_attribute(
        index: usize,
        span: Span,
        owner: SymbolIndex,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> TypedModelProperty {
        // Check for duplicates.
        // find first instance of an attribute name that matches this one.
        let shadow = ambience.create_shadow(binder.current_scope);
        let address_of_owner_model = shadow
            .lookaround(&symbol_table.get(owner).unwrap().name)
            .unwrap()
            .construct_address(binder.path.0 as usize);
        let parent_model = ambience.get_entry_unguarded(address_of_owner_model).model();

        let attribute = parent_model
            .attributes
            .get(index)
            .expect("Could not find an attribute with the correct signature.");
        let first_instance = parent_model
            .attributes
            .iter()
            .find(|attr| attr.name.name == attribute.name.name)
            .unwrap();
        let symbol_idx = if !std::ptr::eq(first_instance, attribute) {
            // instances of the same name are not equal.
            bind_utils::add_ctx_error(
                &binder,
                errors,
                errors::duplicate_property(attribute.name.to_owned()),
            );
            // fault tolerance for type.
            types::bind_type_expression(
                &attribute.var_type,
                binder,
                symbol_table,
                errors,
                ambience,
            );
            // By definition, if the attribute being bound is not the first attribute instance with
            // the given name, then the first has already been bound,
            // add a reference to the first instance and return its index.
            let symbol_index = *binder.known_values.get(&first_instance.name.span).unwrap();
            let symbol = symbol_table.get_mut(symbol_index).unwrap();
            symbol.add_reference(binder.path, attribute.name.span);
            symbol_index
            // block method and attribute clashes.
        } else if let Some(method) = parent_model
            .methods
            .iter()
            .filter(|method| method.name.span.is_before(attribute.name.span))
            .find(|method| method.name.name == attribute.name.name)
        {
            bind_utils::add_ctx_error(
                &binder,
                errors,
                errors::duplicate_property(method.name.to_owned()),
            );
            // fault tolerance for the type.
            types::bind_type_expression(
                &attribute.var_type,
                binder,
                symbol_table,
                errors,
                ambience,
            );
            let symbol_index = *binder.known_values.get(&method.name.span).unwrap();
            let symbol = symbol_table.get_mut(symbol_index).unwrap();
            symbol.add_reference(binder.path, attribute.name.span);
            symbol_index
        } else {
            // attribute being bound is the first attribute to be bound with this name.
            let symbol = SemanticSymbol {
                name: attribute.name.name.to_owned(),
                kind: SemanticSymbolKind::Attribute {
                    is_public: attribute.is_public,
                    declared_type: bind_type_expression(
                        &attribute.var_type,
                        binder,
                        symbol_table,
                        errors,
                        ambience,
                    ),
                    inferred_type: EvaluatedType::unknown(),
                    owner_model: owner,
                    property_index: index,
                },
                // add first reference.
                references: vec![SymbolReferenceList {
                    module_path: binder.path,
                    starts: vec![attribute.name.span.start],
                }],
                doc_info: attribute.info.clone(), //todo
                origin_span: span,
            };
            let index = symbol_table.add(symbol);
            binder.known_values.insert(attribute.name.span, index);
            index
        };
        let ref_number = bind_utils::last_reference_no(&symbol_table, symbol_idx);
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
    fn bind_model_method(
        index: usize,
        span: Span,
        owner: SymbolIndex,
        body: Block,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedModelProperty {
        // find first instance of an attribute name that matches this one.
        let shadow = ambience.create_shadow(binder.current_scope);
        let address_of_owner_model = shadow
            .lookaround(&symbol_table.get(owner).unwrap().name)
            .unwrap()
            .construct_address(binder.path.0 as usize);
        let parent_model = ambience
            .get_entry_unguarded_mut(address_of_owner_model)
            .model_mut();
        binder.push_generic_pool(); // Add a list of reachable generic parameters.
        let method = parent_model
            .methods
            .get(index)
            .expect("Could not find method with correct index.");
        let first_instance = parent_model
            .methods
            .iter()
            .find(|meth| meth.name.name == method.name.name)
            .unwrap();
        let symbol_idx = if !std::ptr::eq(first_instance, method) {
            // first instance is not equal to this method.
            bind_utils::add_ctx_error(
                binder,
                errors,
                errors::duplicate_property(method.name.to_owned()),
            );
            let symbol_index = *binder.known_values.get(&first_instance.name.span).unwrap();
            let symbol = symbol_table.get_mut(symbol_index).unwrap();
            symbol.add_reference(binder.path, method.name.span);
            symbol_index
            // block method and attribute clashes.
        } else if let Some(attribute) = parent_model
            .attributes
            .iter()
            .filter(|attr| attr.name.span.is_before(method.name.span))
            .find(|attr| attr.name.name == method.name.name)
        {
            bind_utils::add_ctx_error(
                binder,
                errors,
                errors::duplicate_property(attribute.name.to_owned()),
            );
            let symbol_index = *binder.known_values.get(&first_instance.name.span).unwrap();
            let symbol = symbol_table.get_mut(symbol_index).unwrap();
            symbol.add_reference(binder.path, method.name.span);
            symbol_index
        } else {
            // first property with this name to be bound.
            let symbol = SemanticSymbol {
                name: method.name.name.to_owned(),
                kind: SemanticSymbolKind::Method {
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
                    module_path: binder.path,
                    starts: vec![method.name.span.start],
                }],
                doc_info: method.info.clone(), //todo.
                origin_span: span,
            };
            let index = symbol_table.add(symbol);
            binder.known_values.insert(method.name.span, index);
            index
        };

        let method = parent_model
            .methods
            .get_mut(index)
            .expect("Could not find method with correct index.");
        let ref_number = bind_utils::last_reference_no(&symbol_table, symbol_idx);
        let method_generic_params = method.generic_params.take();
        let method_params = take(&mut method.params);
        let return_type = method.return_type.take();
        // Add return type, generic parameters and parameters.
        let generic_params_solved = bind_generic_parameters(
            method_generic_params.as_ref(),
            binder,
            symbol_table,
            errors,
            ambience,
        );
        let (body, parameters) = bind_function_block(
            body,
            method_params,
            binder,
            symbol_table,
            errors,
            literals,
            ambience,
        );
        let return_type_solved = return_type.as_ref().map(|rtype| {
            types::bind_type_expression(rtype, binder, symbol_table, errors, ambience)
        });

        if let SemanticSymbolKind::Method {
            params,
            generic_params,
            return_type,
            ..
        } = &mut symbol_table.get_mut(symbol_idx).unwrap().kind
        {
            *generic_params = generic_params_solved;
            *return_type = return_type_solved;
            *params = parameters;
        }
        let method = TypedModelProperty {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            _type: TypedModelPropertyType::TypedMethod { body },
            span,
        };
        binder.pop_generic_pool(); // Remove list of reachable generic parameters.
        return method;
    }

    /// Binds a function declaration.
    fn function_declaration(
        func: ast::FunctionDeclaration,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedFunctionDeclaration {
        let binding_result = handle_scope_entry(
            binder,
            symbol_table,
            errors,
            &ambience,
            func.address,
            func.span,
            true,
        );
        let signature = ambience.get_entry_unguarded_mut(func.address).func_mut();
        binder.push_generic_pool(); // List of generic params.
                                    // Generic parameters should always be first.

        let signature_params = take(&mut signature.params);
        let signature_generic_params = signature.generic_params.take();
        let return_type = signature.return_type.take();
        let generic_params_solved = bind_generic_parameters(
            signature_generic_params.as_ref(),
            binder,
            symbol_table,
            errors,
            &ambience,
        );
        let (body, parameters) = bind_function_block(
            func.body,
            signature_params,
            binder,
            symbol_table,
            errors,
            literals,
            ambience,
        );
        let (symbol_idx, body) = match binding_result {
            Ok(idx) => {
                let return_type_solved = return_type.as_ref().map(|type_exp| {
                    bind_type_expression(type_exp, binder, symbol_table, errors, &ambience)
                });
                // add generic params if binding was successful.
                let body = if let Some(SemanticSymbol {
                    kind:
                        SemanticSymbolKind::Function {
                            generic_params,
                            params,
                            return_type,
                            ..
                        },
                    ..
                }) = &mut symbol_table.get_mut(idx)
                {
                    *generic_params = generic_params_solved;
                    *return_type = return_type_solved;
                    *params = parameters;
                    body
                } else {
                    unreachable!("Could not retrieve bound function symbol.")
                };
                (idx, body)
            }
            // binding failed because it already exists.
            Err(idx) => (idx, body),
        };
        binder.pop_generic_pool();
        let ref_number = last_reference_no(&symbol_table, symbol_idx);
        TypedFunctionDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            body,
            span: func.span,
        }
    }

    /// Binds a function block.
    pub fn bind_function_block(
        block: Block,
        params: Vec<Parameter>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> (TypedBlock, Vec<SymbolIndex>) {
        ambience.jump_to_scope(block.scope_id);
        let mut parameters: Vec<TemporaryParameterDetails> = vec![];
        for mut parameter in params {
            // Block the use of parameters with the same name.
            if parameters
                .iter()
                .find(|details| details.name == parameter.name.name)
                .is_some()
            {
                bind_utils::add_ctx_error(
                    &binder,
                    errors,
                    errors::duplicate_parameter_names(parameter.name.to_owned()),
                );
                continue;
            }
            // Block the use of required parameters after optional ones.
            if let Some(TemporaryParameterDetails {
                is_optional: last_was_optional,
                ..
            }) = parameters.last()
            {
                if *last_was_optional && !parameter.is_optional {
                    bind_utils::add_ctx_error(
                        &binder,
                        errors,
                        errors::required_parameter_after_optional(parameter.span),
                    );
                }
            }
            let mut symbol = SemanticSymbol {
                name: parameter.name.name.to_owned(),
                kind: SemanticSymbolKind::Parameter {
                    is_optional: parameter.is_optional,
                    param_type: parameter.type_label.as_ref().map(|type_exp| {
                        types::bind_type_expression(
                            type_exp,
                            binder,
                            symbol_table,
                            errors,
                            ambience,
                        )
                    }),
                },
                references: vec![],
                doc_info: parameter.info.take(),
                origin_span: parameter.span,
            };
            symbol.add_reference(binder.path, parameter.name.span);
            // add symbol.
            let index = symbol_table.add(symbol);
            binder.known_values.insert(parameter.name.span, index);
            // store for next param check.
            parameters.push(TemporaryParameterDetails {
                name: parameter.name.name.to_owned(),
                is_optional: parameter.is_optional,
                index,
            });
            // parameters are treated as mock declarations in the block.
            // The mechanism for searching for an entry ensures that they will always take
            // precedence over other declarations, so no worries about appending them to a scope.
            ambience.register(ScopeEntry::Parameter(parameter));
        }
        ambience.leave_scope();
        (
            expressions::bind_block(block, binder, symbol_table, errors, literals, ambience),
            parameters
                .into_iter()
                .map(|details| details.index)
                .collect(),
        )
    }

    /// Binds an enum declaration.
    pub fn enum_declaration(
        enumdecl: EnumDeclaration,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> TypedEnumDeclaration {
        let binding_result = handle_scope_entry(
            binder,
            symbol_table,
            errors,
            ambience,
            enumdecl.address,
            enumdecl.span,
            true,
        ); // enums can be hoisted.
        binder.push_generic_pool();
        let signature = ambience.get_entry_unguarded(enumdecl.address).enum_();
        // Generic parameters should always be first.
        let generic_params_solved = bind_generic_parameters(
            signature.generic_params.as_ref(),
            binder,
            symbol_table,
            errors,
            ambience,
        );

        let symbol_idx = match binding_result {
            Ok(symbol_idx) => {
                binder.this_type.push(symbol_idx); // Set meaning of `This`.
                let variants_solved = enum_variants(
                    &signature.variants,
                    symbol_idx,
                    binder,
                    symbol_table,
                    errors,
                    ambience,
                );

                match &mut symbol_table.get_mut(symbol_idx) {
                    // Add generic parameters, if the binding to this type was successful.
                    Some(SemanticSymbol {
                        kind:
                            SemanticSymbolKind::Enum {
                                generic_params,
                                variants,
                                ..
                            },
                        ..
                    }) => {
                        *generic_params = generic_params_solved;
                        *variants = variants_solved;
                    }
                    _ => unreachable!("Cannot retrieve bound enum."),
                }
                symbol_idx
            }
            Err(idx) => {
                binder.this_type.push(idx); // Set meaning of `This`.
                idx
            }
        };
        let ref_number = last_reference_no(&symbol_table, symbol_idx);
        let enum_declaration = TypedEnumDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            span: enumdecl.span,
        };
        binder.this_type.pop(); // Remove meaning of This.
        binder.pop_generic_pool();
        return enum_declaration;
    }

    /// Bind a list enum variants
    pub fn enum_variants(
        variants: &[EnumVariant],
        symbol_idx: SymbolIndex,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> Vec<SymbolIndex> {
        let mut temp_variants: Vec<TemporaryVariantDetails> = vec![];
        for (index, variant) in variants.into_iter().enumerate() {
            // Block multiple variants with the same name.
            if temp_variants
                .iter()
                .find(|temp| temp.name == variant.name.name)
                .is_some()
            {
                add_ctx_error(
                    binder,
                    errors,
                    errors::duplicate_enum_variant(variant.name.to_owned()),
                );
                continue;
            }
            let symbol = SemanticSymbol {
                name: variant.name.name.to_owned(),
                kind: SemanticSymbolKind::Variant {
                    owner_enum: symbol_idx,
                    variant_index: index,
                    tagged_types: variant
                        .tagged_types
                        .iter()
                        .map(|tagged_type_exp| {
                            bind_type_expression(
                                tagged_type_exp,
                                binder,
                                symbol_table,
                                errors,
                                ambience,
                            )
                        })
                        .collect(),
                },
                references: vec![SymbolReferenceList {
                    module_path: binder.path,
                    starts: vec![variant.span.start],
                }],
                doc_info: None, // todo: doc info for variants.
                origin_span: variant.span,
            };
            let symbol_idx = symbol_table.add(symbol);
            binder.known_values.insert(variant.name.span, symbol_idx);
            // store for next variant check.
            temp_variants.push(TemporaryVariantDetails {
                name: variant.name.name.to_owned(),
                index: symbol_idx,
            });
        }
        temp_variants.into_iter().map(|temp| temp.index).collect()
    }

    /// Binds a trait declaration.
    pub fn trait_declaration(_trait_decl: ast::TraitDeclaration) -> TypedTraitDeclaration {
        todo!()
    }

    /// Binds a type declaration.
    pub fn bind_type_declaration(
        type_decl: TypeDeclaration,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> TypedTypeDeclaration {
        let binding_result = handle_scope_entry(
            binder,
            symbol_table,
            errors,
            ambience,
            type_decl.address,
            type_decl.span,
            true,
        );
        let signature = ambience.get_entry_unguarded(type_decl.address).type_();
        let symbol_idx = match binding_result {
            Ok(symbol_idx) => {
                // Add generic parameters, if the binding to this type was successful.
                binder.push_generic_pool(); // List of parameters.
                let generic_params_solved = bind_generic_parameters(
                    signature.generic_params.as_ref(),
                    binder,
                    symbol_table,
                    errors,
                    ambience,
                );
                let value_solved =
                    bind_type_expression(&signature.value, binder, symbol_table, errors, ambience);
                match &mut symbol_table.get_mut(symbol_idx).unwrap().kind {
                    SemanticSymbolKind::TypeName {
                        generic_params,
                        value,
                        ..
                    } => {
                        *generic_params = generic_params_solved;
                        *value = value_solved
                    }
                    _ => unreachable!("Bound a type but could not retrieve its value."),
                }
                binder.pop_generic_pool();
                symbol_idx
            }
            Err(idx) => idx,
        };
        let ref_number = last_reference_no(&symbol_table, symbol_idx);
        TypedTypeDeclaration {
            name: SymbolLocator {
                symbol_idx,
                ref_number,
            },
            span: type_decl.span,
        }
    }

    /// Binds a while statement.
    fn bind_while_statement(
        while_statement: WhileStatement,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedWhileStatement {
        TypedWhileStatement {
            condition: bind_expression(
                while_statement.condition,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            body: bind_block(
                while_statement.body,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: while_statement.span,
        }
    }

    /// Binds a return statement.
    fn bind_return_statement(
        returnstmnt: ReturnStatement,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedReturnStatement {
        TypedReturnStatement {
            value: returnstmnt.value.map(|expression| {
                bind_expression(expression, binder, symbol_table, errors, literals, ambience)
            }),
            span: returnstmnt.span,
        }
    }

    /// Binds a break statement.
    pub fn bind_break_statement(
        _break: ast::BreakStatement,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> TypedBreakStatement {
        TypedBreakStatement {
            label: _break.label.map(|identifier| {
                expressions::bind_identifier(identifier, binder, symbol_table, errors, ambience)
            }),
            span: _break.span,
        }
    }
    /// Binds a continue statement.
    pub fn bind_continue_statement(
        cont: ast::ContinueStatement,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> TypedContinueStatement {
        TypedContinueStatement {
            label: cont.label.map(|identifier| {
                expressions::bind_identifier(identifier, binder, symbol_table, errors, ambience)
            }),
            span: cont.span,
        }
    }
    /// Binds a variable declaration.
    pub fn variable_declaration(_variable: ast::VariableDeclaration) -> TypedVariableDeclaration {
        todo!()
    }
    /// Binds a for statement.
    pub fn for_statement(_for_statement: ast::ForStatement) -> TypedForStatement {
        todo!()
    }
}

/// Expressions.
mod expressions {
    use super::{bind_utils::last_reference_no, statements::bind_function_block, *};
    // Binds an expression.
    pub fn bind_expression(
        expression: Expression,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedExpression {
        match expression {
            Expression::Identifier(identfier) => TypedExpression::Identifier(bind_identifier(
                identfier,
                binder,
                symbol_table,
                errors,
                &ambience,
            )),
            Expression::StringLiteral(string) => {
                TypedExpression::Literal(literals::bind_string(string, binder.path, literals))
            }
            Expression::NumberLiteral(number) => {
                TypedExpression::Literal(literals::bind_number(number, binder.path, literals))
            }
            Expression::BooleanLiteral(boolean) => {
                TypedExpression::Literal(literals::bind_boolean(boolean, binder.path, literals))
            }
            Expression::NewExpr(new_expr) => TypedExpression::NewExpr(Box::new(
                bind_new_expression(new_expr, binder, symbol_table, errors, literals, ambience),
            )),
            Expression::ThisExpr(this) => TypedExpression::ThisExpr(this_expression(
                this,
                binder,
                symbol_table,
                errors,
                ambience,
            )),
            Expression::CallExpr(call) => TypedExpression::CallExpr(Box::new(
                bind_call_expression(call, binder, symbol_table, errors, literals, ambience),
            )),
            Expression::FnExpr(func) => TypedExpression::FnExpr(Box::new(function_expression(
                *func,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ))),
            Expression::IfExpr(ifexpr) => TypedExpression::IfExpr(bind_if_expression(
                ifexpr,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            )),
            Expression::ArrayExpr(array) => TypedExpression::ArrayExpr(bind_array_expression(
                array,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            )),
            Expression::AccessExpr(accessexp) => TypedExpression::AccessExpr(access_expression(
                accessexp,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            )),
            Expression::IndexExpr(indexexp) => TypedExpression::IndexExpr(index_expression(
                indexexp,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            )),
            Expression::BinaryExpr(binexp) => TypedExpression::BinaryExpr(binary_expression(
                binexp,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            )),
            Expression::AssignmentExpr(assexp) => {
                TypedExpression::AssignmentExpr(bind_assignment_expression(
                    assexp,
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                ))
            }
            Expression::UnaryExpr(unaryexp) => TypedExpression::UnaryExpr(bind_unary_expression(
                unaryexp,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            )),
            Expression::LogicExpr(logicexp) => TypedExpression::LogicExpr(bind_logic_expression(
                logicexp,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            )),
            Expression::UpdateExpr(updateexp) => TypedExpression::UpdateExpr(
                bind_update_expression(updateexp, binder, symbol_table, errors, literals, ambience),
            ),
            Expression::BlockExpr(block) => TypedExpression::Block(bind_block(
                block,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            )),
        }
    }

    /// Bind an identifier.
    pub fn bind_identifier(
        identifier: Identifier,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> TypedIdent {
        let symbol_index =
            bind_utils::find_or_create(binder, symbol_table, errors, ambience, &identifier, false);
        let ref_number = bind_utils::last_reference_no(&symbol_table, symbol_index);
        TypedIdent {
            value: SymbolLocator {
                symbol_idx: symbol_index,
                ref_number,
            },
        }
    }

    /// Bind a new expression.
    pub fn bind_new_expression(
        new_expr: Box<ast::NewExpr>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedNewExpr {
        TypedNewExpr {
            value: bind_expression(
                new_expr.value,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: new_expr.span,
        }
    }

    /// Bind a `this` expression to its meaning.
    pub fn this_expression(
        this: ast::ThisExpr,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> TypedThisExpr {
        let shadow = ambience.create_shadow(binder.current_scope);
        let symbol_index = match shadow.get_method_context() {
            // find the referenced model or trait.
            Some(search) => Some(bind_utils::find_or_create(
                binder,
                symbol_table,
                errors,
                ambience,
                search.entry.ident().unwrap(),
                false,
            )),
            // 'this' is being used outside of a model or trait.
            None => {
                bind_utils::add_ctx_error(binder, errors, errors::this_outside_method(this.span));
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
    fn bind_call_expression(
        call: Box<ast::CallExpr>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedCallExpr {
        let mut bind_expression = |expression| {
            bind_expression(expression, binder, symbol_table, errors, literals, ambience)
        };
        TypedCallExpr {
            caller: bind_expression(call.caller),
            arguments: call
                .arguments
                .into_iter()
                .map(|argument| bind_expression(argument))
                .collect(),
        }
    }
    /// Bind a function expression.
    fn function_expression(
        func: FunctionExpr,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedFnExpr {
        // bind generic parameters.
        binder.push_generic_pool();
        let generic_params = types::bind_generic_parameters(
            func.generic_params.as_ref(),
            binder,
            symbol_table,
            errors,
            &ambience,
        );
        let return_type = func.return_type.map(|ref rettype| {
            types::bind_type_expression(rettype, binder, symbol_table, errors, &ambience)
        });
        // If the expression is a block, all well and good.
        // if the expression is anything else, it needs to be scoped to a block so that the parameter values have meaning and scope.
        let function_expr = match func.body {
            Expression::BlockExpr(block) => {
                let (body, params) = bind_function_block(
                    block,
                    func.params,
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                );
                TypedFnExpr {
                    is_async: func.is_async,
                    generic_params,
                    params,
                    return_type,
                    body: TypedExpression::Block(body),
                    span: func.span,
                }
            }
            body => {
                ambience.enter(ScopeType::Functional);
                let fake_scope_id = ambience.current_scope();
                let fakeblock = Block {
                    scope_id: fake_scope_id,
                    statements: vec![Statement::FreeExpression(body)],
                    span: Span::default(), // unecessary span.
                };
                let (mut block, params) = bind_function_block(
                    fakeblock,
                    func.params,
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                );
                ambience.jump_to_scope(fake_scope_id);
                ambience.leave_scope();
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
        binder.pop_generic_pool();
        function_expr
    }
    /// Bind a block.
    pub fn bind_block(
        block: Block,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedBlock {
        let prior_scope = binder.current_scope;
        binder.current_scope = block.scope_id;
        let block = TypedBlock {
            statements: block
                .statements
                .into_iter()
                .map(|statement| {
                    statements::bind_statement(
                        statement,
                        binder,
                        symbol_table,
                        errors,
                        literals,
                        ambience,
                    )
                })
                .collect(),
            return_type: EvaluatedType::unknown(),
            span: block.span,
        };
        binder.current_scope = prior_scope;
        return block;
    }

    /// Binds a binary expression.
    fn binary_expression(
        binexp: Box<ast::BinaryExpr>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> Box<TypedBinExpr> {
        Box::new(TypedBinExpr {
            left: bind_expression(
                binexp.left,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            operator: binexp.operator,
            right: bind_expression(
                binexp.right,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: binexp.span,
        })
    }
    /// Binds an if expression.
    fn bind_if_expression(
        ifexpr: Box<ast::IfExpression>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> Box<TypedIfExpr> {
        Box::new(TypedIfExpr {
            condition: bind_expression(
                ifexpr.condition,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            consequent: bind_block(
                ifexpr.consequent,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            alternate: ifexpr.alternate.map(|_else| TypedElse {
                expression: bind_expression(
                    _else.expression,
                    binder,
                    symbol_table,
                    errors,
                    literals,
                    ambience,
                ),
                span: _else.span,
            }),
            span: ifexpr.span,
        })
    }
    /// Binds an array expression.
    fn bind_array_expression(
        array: ast::ArrayExpr,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> TypedArrayExpr {
        TypedArrayExpr {
            elements: array
                .elements
                .into_iter()
                .map(|expression| {
                    bind_expression(expression, binder, symbol_table, errors, literals, ambience)
                })
                .collect(),
            span: array.span,
        }
    }
    /// Binds an index expression.
    fn index_expression(
        indexexp: Box<ast::IndexExpr>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> Box<TypedIndexExpr> {
        Box::new(TypedIndexExpr {
            object: bind_expression(
                indexexp.object,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            index: bind_expression(
                indexexp.index,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: indexexp.span,
        })
    }
    /// Binds an assignment expression.
    fn bind_assignment_expression(
        assexp: Box<ast::AssignmentExpr>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> Box<TypedAssignmentExpr> {
        Box::new(TypedAssignmentExpr {
            left: bind_expression(
                assexp.left,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            operator: assexp.operator,
            right: bind_expression(
                assexp.right,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: assexp.span,
        })
    }

    /// Binds an access expression.
    fn access_expression(
        accessexp: Box<ast::AccessExpr>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> Box<TypedAccessExpr> {
        let object = bind_expression(
            accessexp.object,
            binder,
            symbol_table,
            errors,
            literals,
            ambience,
        );
        // It is unfeasible to bind the type of the accessed property,
        // so it wil have to be done during typechecking.
        // The Property Symbol serves as a placeholder to be mutated later.
        let property_ident = match accessexp.property {
            Expression::Identifier(i) => i,
            _ => unreachable!(
                "Attempting to bind a property that is not an identifier. Fix parsing."
            ),
        };
        let mut property_symbol = SemanticSymbol {
            name: property_ident.name,
            kind: SemanticSymbolKind::Property { resolved: None },
            references: vec![],
            doc_info: None,
            origin_span: property_ident.span,
        };
        property_symbol.add_reference(binder.path, property_ident.span);
        let symbol_idx = symbol_table.add(property_symbol);
        let ref_number = last_reference_no(symbol_table, symbol_idx);

        let property = TypedExpression::Identifier(TypedIdent {
            value: SymbolLocator {
                symbol_idx,
                ref_number,
            },
        });
        Box::new(TypedAccessExpr {
            object,
            property,
            span: accessexp.span,
        })
    }

    /// Binds a unary expression.
    fn bind_unary_expression(
        unaryexp: Box<ast::UnaryExpr>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> Box<TypedUnaryExpr> {
        Box::new(TypedUnaryExpr {
            operator: unaryexp.operator,
            operand: bind_expression(
                unaryexp.operand,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: unaryexp.span,
        })
    }
    /// Binds a logic expression.
    fn bind_logic_expression(
        logicexp: Box<ast::LogicExpr>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> Box<TypedLogicExpr> {
        Box::new(TypedLogicExpr {
            left: bind_expression(
                logicexp.left,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            operator: logicexp.operator,
            right: bind_expression(
                logicexp.right,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: logicexp.span,
        })
    }
    /// Binds an update expression.
    fn bind_update_expression(
        updateexp: Box<ast::UpdateExpr>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        literals: &mut Vec<Literal>,
        ambience: &mut ModuleAmbience,
    ) -> Box<TypedUpdateExpr> {
        Box::new(TypedUpdateExpr {
            operator: updateexp.operator,
            operand: bind_expression(
                updateexp.operand,
                binder,
                symbol_table,
                errors,
                literals,
                ambience,
            ),
            span: updateexp.span,
        })
    }
}

/// Literals.
mod literals {
    use super::*;
    /// Bind a string.
    pub fn bind_string(
        value: ast::WhirlString,
        path: PathIndex,
        literals: &mut Vec<Literal>,
    ) -> LiteralIndex {
        // todo: can literals be deduplicated?
        let literal = Literal::StringLiteral {
            module: path,
            value,
        };
        let index = LiteralIndex(literals.len());
        literals.push(literal);
        index
    }

    /// Bind a number.
    pub fn bind_number(
        value: ast::WhirlNumber,
        path: PathIndex,
        literals: &mut Vec<Literal>,
    ) -> LiteralIndex {
        // todo: can literals be deduplicated?
        let literal = Literal::NumericLiteral {
            module: path,
            value,
        };
        let index = LiteralIndex(literals.len());
        literals.push(literal);
        index
    }

    // Bind a boolean.
    pub fn bind_boolean(
        value: ast::WhirlBoolean,
        path: PathIndex,
        literals: &mut Vec<Literal>,
    ) -> LiteralIndex {
        // todo: can literals be deduplicated?
        let literal = Literal::BooleanLiteral {
            module: path,
            start_line: value.span.start[0],
            start_character: value.span.start[1],
            value: value.value,
        };
        let index = LiteralIndex(literals.len());
        literals.push(literal);
        index
    }
}

/// Types
mod types {
    use crate::ParameterType;

    use super::*;

    /// Binds a type expression.
    pub fn bind_type_expression(
        type_exp: &TypeExpression,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> IntermediateType {
        match type_exp {
            TypeExpression::Union(union_type) => IntermediateType::UnionType {
                types: union_type
                    .types
                    .iter()
                    .map(|type_exp| {
                        bind_type_expression(type_exp, binder, symbol_table, errors, ambience)
                    })
                    .collect(),
                span: union_type.span,
            },
            TypeExpression::Functional(func) => {
                let mut params = vec![];
                for param in &func.params {
                    let parametertype = ParameterType {
                        name: param.name.name.clone(),
                        is_optional: param.is_optional,
                        type_label: param.type_label.as_ref().map(|type_exp| {
                            bind_type_expression(type_exp, binder, symbol_table, errors, ambience)
                        }),
                    };
                    params.push(parametertype);
                }
                IntermediateType::FunctionType {
                    params,
                    return_type: func.return_type.as_ref().map(|type_exp| {
                        Box::new(bind_type_expression(
                            type_exp,
                            binder,
                            symbol_table,
                            errors,
                            ambience,
                        ))
                    }),
                    span: func.span,
                }
            }
            TypeExpression::Member(member_type) => {
                let object_type = bind_type_expression(
                    &member_type.namespace,
                    binder,
                    symbol_table,
                    errors,
                    ambience,
                );
                // Same as in access expressions, properties cannot be bound
                // until imports are resolved and types are inferred.
                // So, a placeholder is used.
                let property_type = match &*member_type.property {
                    TypeExpression::Discrete(discrete_type) => discrete_type,
                    _ => unreachable!("Found and attempted to bind a type property that is not a simple/discrete type. Fix parsing.")
                };
                let mut property_symbol = SemanticSymbol {
                    name: property_type.name.name.to_owned(),
                    kind: SemanticSymbolKind::Property { resolved: None },
                    references: vec![],
                    doc_info: None,
                    origin_span: property_type.name.span,
                };
                property_symbol.add_reference(binder.path, property_type.name.span);
                let property_symbol_idx = symbol_table.add(property_symbol);
                // Collect generics.
                let mut generic_args = vec![];
                if let Some(ref arguments) = property_type.generic_args {
                    for argument in arguments {
                        generic_args.push(bind_type_expression(
                            argument,
                            binder,
                            symbol_table,
                            errors,
                            ambience,
                        ))
                    }
                }

                let final_property_type = IntermediateType::SimpleType {
                    value: property_symbol_idx,
                    generic_args: generic_args,
                    span: property_type.span,
                };

                IntermediateType::MemberType {
                    object: Box::new(object_type),
                    property: Box::new(final_property_type),
                    span: member_type.span,
                }
            }
            TypeExpression::Discrete(discrete_type) => {
                bind_discrete_type(discrete_type, binder, symbol_table, errors, ambience)
            }
            TypeExpression::This { span } => IntermediateType::This {
                meaning: binder.this_type.last().copied(),
                span: *span,
            },
            TypeExpression::BorrowedType(borrowedtype) => IntermediateType::BorrowedType {
                value: Box::new(bind_type_expression(
                    &borrowedtype.value,
                    binder,
                    symbol_table,
                    errors,
                    ambience,
                )),
                span: borrowedtype.span,
            },
            TypeExpression::Invalid => IntermediateType::Placeholder,
        }
    }
    /// Bind a discrete type.
    pub fn bind_discrete_type(
        discrete_type: &ast::DiscreteType,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> IntermediateType {
        let symbol_index = bind_utils::find_or_create(
            binder,
            symbol_table,
            errors,
            ambience,
            &discrete_type.name,
            true,
        );
        let mut generic_args = vec![];
        if let Some(ref arguments) = discrete_type.generic_args {
            for argument in arguments {
                generic_args.push(bind_type_expression(
                    argument,
                    binder,
                    symbol_table,
                    errors,
                    ambience,
                ))
            }
        }
        IntermediateType::SimpleType {
            value: symbol_index,
            generic_args,
            span: discrete_type.span,
        }
    }
    /// Bind an optional list of generic parameters.
    pub fn bind_generic_parameters(
        param_list: Option<&Vec<GenericParameter>>,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
        ambience: &ModuleAmbience,
    ) -> Vec<SymbolIndex> {
        let mut symbol_indexes = vec![];
        if let Some(ref params) = param_list {
            for parameter in params.iter() {
                // Binds only the parameter names.
                let binding_result =
                    bind_generic_parameter_name(parameter, binder, symbol_table, errors);
                let mut type_exp_binder =
                    |typ| types::bind_type_expression(typ, binder, symbol_table, errors, ambience);
                // Bind the trait guards and default values.
                let trait_guards = parameter
                    .traits
                    .iter()
                    .map(|_trait| type_exp_binder(_trait))
                    .collect();
                let param_default_value = parameter
                    .default
                    .as_ref()
                    .map(|default_value| type_exp_binder(default_value));
                let idx = binding_result
                    .map(|idx| {
                        match symbol_table.get_mut(idx) {
                            Some(SemanticSymbol {
                                kind:
                                    SemanticSymbolKind::GenericParameter {
                                        traits,
                                        default_value,
                                        ..
                                    },
                                ..
                            }) => {
                                *traits = trait_guards;
                                *default_value = param_default_value;
                            }
                            _ => unreachable!("Could not retrieve bound generic parameter."),
                        }
                        idx
                    })
                    .unwrap_or_else(|err| err);
                symbol_indexes.push(idx);
            }
        }
        symbol_indexes
    }
    /// Bind a generic parameter.
    pub fn bind_generic_parameter_name(
        parameter: &GenericParameter,
        binder: &mut Binder,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
    ) -> Result<SymbolIndex, SymbolIndex> {
        // Check if a generic parameter with this name already exists in this pool.
        if let Some(index) = binder.lookaround_for_generic_parameter(&parameter.name.name) {
            bind_utils::add_ctx_error(
                &binder,
                errors,
                errors::duplicate_generic_parameter(parameter.name.to_owned()),
            );
            let symbol = symbol_table.get_mut(index).unwrap();
            symbol.add_reference(binder.path, parameter.span);
            return Err(index);
        }
        let symbol = SemanticSymbol {
            name: parameter.name.name.to_owned(),
            kind: SemanticSymbolKind::GenericParameter {
                // traits and default values are bound later.
                traits: vec![],
                default_value: None,
                solutions: vec![],
            },
            references: vec![SymbolReferenceList {
                module_path: binder.path,
                starts: vec![parameter.span.start],
            }],
            doc_info: None, // todo. Collect documentation for generic params.
            origin_span: parameter.span,
        };
        let index = symbol_table.add(symbol);
        binder.add_generic_parameter(&parameter.name.name, index);
        binder.known_values.insert(parameter.name.span, index);
        Ok(index)
    }
}

impl Binder {
    /// Create new binder for a module path.
    pub fn new(path: PathIndex, corelib_symbol_idx: Option<SymbolIndex>) -> Self {
        Self {
            path,
            known_values: HashMap::new(),
            unknown_values: HashMap::new(),
            generic_pools: RefCell::new(vec![]),
            imported_values: vec![],
            current_scope: 0,
            this_type: vec![],
            module_symbols: vec![],
            module_decl_span: None,
            corelib_symbol_idx,
        }
    }
}

// Generics.
impl Binder {
    /// Add a new generic pool.
    fn push_generic_pool(&mut self) {
        self.generic_pools.borrow_mut().push(vec![]);
    }
    /// Remove the last added generic pool.
    fn pop_generic_pool(&mut self) {
        self.generic_pools.borrow_mut().pop();
    }
    /// Check in the list of counting pools for a generic parameter.
    fn lookup_generic_parameter(&self, name: &str) -> Option<SymbolIndex> {
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
    fn lookaround_for_generic_parameter(&self, name: &str) -> Option<SymbolIndex> {
        let pools = self.generic_pools.borrow();
        let current_pool = pools.last()?;
        current_pool
            .iter()
            .find(|param_tuple| param_tuple.0 == name)
            .map(|tuple| tuple.1)
    }
    /// Add a generic parameter to the current pool.
    fn add_generic_parameter(&mut self, name: &str, index: SymbolIndex) {
        let mut pools = self.generic_pools.borrow_mut();
        let current_pool = pools
            .last_mut()
            .expect("Cannot add generic parameter, because no pool has been created.");
        current_pool.push((name.to_owned(), index));
    }
}
