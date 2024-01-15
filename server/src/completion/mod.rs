#![allow(unused)]
use crate::message_store::MessageStore;
use analyzer::{
    evaluate, span_of_typed_expression, span_of_typed_statement, utils::coerce, EvaluatedType,
    SemanticSymbolKind, Standpoint, SymbolIndex, TypedExpression, TypedInterfacePropertyType,
    TypedModelPropertyType, TypedModule, TypedStmnt,
};
use ast::{maybe, Span};
use pretty::SymbolWriter;
use serde_json::Value;
use std::{cell::RefCell, collections::HashMap};
use tower_lsp::lsp_types::{
    CompletionContext, CompletionItem, CompletionItemKind, CompletionItemLabelDetails,
    CompletionResponse, Documentation, InsertTextFormat, MarkupContent, MarkupKind,
};

/// A visitor that traverses a typed module to determine the most suitable
/// completion response for a completion prompt.
pub struct CompletionFinder<'a> {
    module: &'a TypedModule,
    standpoint: &'a Standpoint,
    pos: [u32; 2],
    pub context: Option<CompletionContext>,
    pub message_store: RefCell<MessageStore>,
    next_statement_span: RefCell<Option<Span>>,
    enclosing_model_or_interface: RefCell<Option<SymbolIndex>>,
}

#[derive(PartialEq, Clone, Copy)]
pub enum Trigger {
    NewInstance,
    UseImport,
    DotAccess,
    WhiteSpace,
    Ampersand,
    None,
    TypeLabel,
    Implements,
    Module,
}

#[derive(Clone, Copy)]
pub enum DotCompletionType {
    /// Provide completions for names, symbols, methods and attributes.
    Full,
    /// Provide only completions for module names and symbols.
    Half,
}

impl<'a> CompletionFinder<'a> {
    pub fn new(
        module: &'a TypedModule,
        standpoint: &'a Standpoint,
        pos: [u32; 2],
        messages: MessageStore,
        completion_context: Option<CompletionContext>,
    ) -> Self {
        Self {
            standpoint,
            module,
            pos,
            message_store: RefCell::new(messages),
            context: completion_context,
            next_statement_span: RefCell::new(None),
            enclosing_model_or_interface: RefCell::new(None),
        }
    }
    /// Main complete function for dot access.
    pub fn complete_from_dot(
        &self,
        inferred_type: EvaluatedType,
        completion_type: DotCompletionType,
    ) -> Option<CompletionResponse> {
        let writer = SymbolWriter::new(&self.standpoint);
        let symbollib = &self.standpoint.symbol_library;
        let mut completions = vec![];
        match inferred_type {
            EvaluatedType::Partial { types } => {
                return self.complete_from_dot(types.first()?.clone(), completion_type)
            }
            EvaluatedType::ModelInstance { model, .. } => {
                self.complete_instance_access(completion_type, model, &writer, &mut completions)?;
            }
            EvaluatedType::Model(owner) | EvaluatedType::Interface(owner) => {
                self.complete_model_or_interface_static_access(owner, &writer, &mut completions)?;
            }
            EvaluatedType::Enum(enum_) => {
                let enum_symbol = self.standpoint.symbol_library.get(enum_)?;
                match &enum_symbol.kind {
                    SemanticSymbolKind::Enum { variants, .. } => {
                        for variant in variants {
                            let symbol = self.standpoint.symbol_library.get(*variant)?;
                            let documentation =
                                Some(generate_documentation(&writer, variant, symbol));
                            let label = symbol.name.clone();
                            completions.push(CompletionItem {
                                label,
                                kind: Some(CompletionItemKind::ENUM_MEMBER),
                                documentation,
                                ..Default::default()
                            })
                        }
                    }
                    _ => return None,
                }
            }
            // Completion after a module name.
            EvaluatedType::Module(module) => {
                self.complete_module_access(module, completion_type, writer, &mut completions)?;
            }
            EvaluatedType::InterfaceInstance { interface_, .. } => self.complete_instance_access(
                completion_type,
                interface_,
                &writer,
                &mut completions,
            )?,
            EvaluatedType::Generic { base } | EvaluatedType::HardGeneric { base, .. } => {
                let symbol = self.standpoint.symbol_library.get(base)?;
                match &symbol.kind {
                    SemanticSymbolKind::GenericParameter { interfaces, .. } => {
                        for _interface in interfaces {
                            let evaled = evaluate(_interface, symbollib, None, &mut None, 0);
                            let response = self.complete_from_dot(evaled, completion_type);
                            if let Some(response) = response {
                                match response {
                                    CompletionResponse::Array(mut subcompletions) => {
                                        completions.append(&mut subcompletions)
                                    }
                                    CompletionResponse::List(mut list) => {
                                        completions.append(&mut list.items)
                                    }
                                }
                            }
                        }
                    }
                    _ => return None,
                }
            }
            EvaluatedType::OpaqueTypeInstance {
                available_methods,
                available_interfaces,
                generic_arguments,
                ..
            } => {
                *writer.is_opaque.borrow_mut() = true;

                for method in available_methods {
                    let symbol = symbollib.get(method)?;
                    let mut label = symbol.name.clone();
                    let mut final_eval_type = EvaluatedType::MethodInstance {
                        method,
                        generic_arguments: vec![],
                        is_invariant: false,
                    };
                    final_eval_type = coerce(final_eval_type, &generic_arguments);
                    let detail = Some(symbollib.format_evaluated_type(&final_eval_type));
                    let params = match &symbol.kind {
                        SemanticSymbolKind::Method {
                            params, is_public, ..
                        } => {
                            if !is_public {
                                continue;
                            }
                            params
                        }
                        _ => continue,
                    };
                    if params.len() > 0 {
                        label.push_str("(...)")
                    } else {
                        label.push_str("()")
                    }
                    let insert_text = Some(self.generate_function_completion(&symbol.name, params));
                    completions.push(CompletionItem {
                        label,
                        detail,
                        kind: Some(CompletionItemKind::METHOD),
                        insert_text,
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        ..Default::default()
                    })
                }
                for implementation in available_interfaces {
                    self.message_store.borrow_mut().inform("Writing implsss");
                    let owner = match implementation {
                        EvaluatedType::InterfaceInstance { interface_, .. } => interface_,
                        _ => continue,
                    };
                    self.complete_instance_access(
                        completion_type,
                        owner,
                        &writer,
                        &mut completions,
                    );
                }
            }
            _ => {}
        }
        sort_completions(&mut completions);
        return Some(CompletionResponse::Array(completions));
    }

    fn complete_model_or_interface_static_access(
        &self,
        model: SymbolIndex,
        writer: &SymbolWriter<'_>,
        completions: &mut Vec<CompletionItem>,
    ) -> Option<()> {
        let symbollib = &self.standpoint.symbol_library;
        let symbol = self.standpoint.symbol_library.get_forwarded(model)?;
        let static_methods = match &symbol.kind {
            SemanticSymbolKind::Model {
                methods,
                implementations,
                ..
            } => {
                // Get completions from implementations and methods.
                // todo: completion from super interfacefaces.
                implementations
                    .iter()
                    .filter_map(|typ| {
                        let eval_typ = evaluate(typ, symbollib, None, &mut None, 0);
                        match eval_typ {
                            EvaluatedType::InterfaceInstance { interface_, .. } => {
                                let interface_symbol = symbollib.get(interface_)?;
                                match &interface_symbol.kind {
                                    SemanticSymbolKind::Interface { methods, .. } => Some(methods),
                                    _ => return None,
                                }
                            }
                            _ => return None,
                        }
                    })
                    .map(|methods| methods.iter())
                    .flatten()
                    .chain(methods.iter()) // normal methods.
                    .filter_map(|method_idx| {
                        let method_symbol = self.standpoint.symbol_library.get(*method_idx)?;
                        match &method_symbol.kind {
                            SemanticSymbolKind::Method {
                                is_static,
                                is_public,
                                params,
                                ..
                            } => (*is_static
                                && (*is_public
                                    || self
                                        .enclosing_model_or_interface
                                        .borrow()
                                        .is_some_and(|enclosing| enclosing == model)))
                            .then_some(Some((method_idx, method_symbol, params))), // todo: allow private access in appriopriate contexts.
                            _ => None,
                        }
                    })
                    .map(|tuple| tuple.unwrap())
            }
            _ => return None,
        };
        Some(for (method_idx, symbol, params) in static_methods {
            let documentation = Some(generate_documentation(writer, method_idx, symbol));
            let label = symbol.name.clone();
            let detail = Some(
                symbollib.format_evaluated_type(&EvaluatedType::MethodInstance {
                    method: *method_idx,
                    generic_arguments: vec![],
                    is_invariant: false,
                }),
            );
            let insert_text = Some(self.generate_function_completion(&symbol.name, params));
            completions.push(CompletionItem {
                label,
                detail,
                kind: Some(CompletionItemKind::METHOD),
                documentation,
                insert_text,
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            })
        })
    }

    /// Complete a module, followed by a dot.
    fn complete_module_access(
        &self,
        module: SymbolIndex,
        completion_type: DotCompletionType,
        writer: SymbolWriter<'_>,
        completions: &mut Vec<CompletionItem>,
    ) -> Option<()> {
        let symbollib = &self.standpoint.symbol_library;
        let module_symbol = symbollib.get_forwarded(module)?;
        Some(
            if let SemanticSymbolKind::Module {
                global_declaration_symbols,
                ..
            } = &module_symbol.kind
            {
                for symbol_index in global_declaration_symbols {
                    // Not get_forwarded because publicity has to be checked.
                    let symbol = match symbollib.get(*symbol_index) {
                        Some(symbol) => symbol,
                        None => continue,
                    };
                    if !symbol.kind.is_public() {
                        continue;
                    }
                    // Specially complete functions.
                    let symbol = symbollib.get_forwarded(*symbol_index)?;
                    let symbol_index = symbollib.forward(*symbol_index);
                    if let Some(completionitem) = self.create_completion(
                        &writer,
                        symbol,
                        symbol_index,
                        matches!(completion_type, DotCompletionType::Full),
                    ) {
                        completions.push(completionitem);
                    }
                }
            },
        )
    }

    /// Complete a model or interfaceface instance, followed by a dot.
    fn complete_instance_access(
        &self,
        completion_type: DotCompletionType,
        owner: SymbolIndex,
        writer: &SymbolWriter<'_>,
        completions: &mut Vec<CompletionItem>,
    ) -> Option<()> {
        let symbollib = &self.standpoint.symbol_library;
        if matches!(completion_type, DotCompletionType::Half) {
            return None;
        }
        let owner_symbol = symbollib.get(owner)?;
        let methods = match &owner_symbol.kind {
            SemanticSymbolKind::Model { methods, .. }
            | SemanticSymbolKind::Interface { methods, .. } => methods,
            _ => return None,
        };
        for method in methods {
            let symbol = symbollib.get(*method)?;
            // Private completions are allowed if within model itself.
            if !symbol.kind.is_public()
                && *self.enclosing_model_or_interface.borrow() != Some(owner)
            {
                continue;
            }
            let params = match &symbol.kind {
                SemanticSymbolKind::Method {
                    is_static, params, ..
                } => {
                    if *is_static {
                        continue; // skip static methods for instances.
                    }
                    params
                }
                _ => continue,
            };
            let mut label = symbol.name.clone();
            if params.len() > 0 {
                label.push_str("(...)")
            } else {
                label.push_str("()")
            }
            let documentation = Some(generate_documentation(writer, method, symbol));
            let detail = Some(
                symbollib.format_evaluated_type(&EvaluatedType::MethodInstance {
                    method: *method,
                    generic_arguments: vec![],
                    is_invariant: false,
                }),
            );
            // Prevent redundant duplicate completions.
            if completions.iter().any(|item| item.label == label) {
                continue;
            }
            completions.push(CompletionItem {
                label,
                detail,
                kind: Some(CompletionItemKind::METHOD),
                documentation,
                insert_text: Some(self.generate_function_completion(&symbol.name, &params)),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        }
        // Implementations from other interfacefaces.
        let implementations = match &owner_symbol.kind {
            SemanticSymbolKind::Model {
                implementations, ..
            }
            | SemanticSymbolKind::Interface {
                implementations, ..
            } => implementations,
            _ => return Some(()),
        }
        .iter()
        .map(|int_type| evaluate(int_type, symbollib, None, &mut None, 0))
        .filter(|eval_typ| eval_typ.is_interface_instance());
        for implementation in implementations {
            let owner = match implementation {
                EvaluatedType::InterfaceInstance { interface_, .. } => interface_,
                _ => continue,
            };
            self.complete_instance_access(completion_type, owner, writer, completions);
        }
        let attributes = match &owner_symbol.kind {
            SemanticSymbolKind::Model { attributes, .. } => attributes,
            _ => return Some(()),
        };
        let encloser = self.enclosing_model_or_interface.borrow().clone();
        for attribute in attributes {
            let symbol = symbollib.get(*attribute)?;
            if !symbol.kind.is_public() && encloser != Some(owner) {
                continue; // todo: allow private completions in appriopriate context.
            }
            let label = symbol.name.clone();
            let detail = match &symbol.kind {
                SemanticSymbolKind::Attribute { declared_type, .. } => {
                    Some(symbollib.format_evaluated_type(&evaluate(
                        declared_type,
                        symbollib,
                        None,
                        &mut None,
                        0,
                    )))
                }
                _ => None,
            };
            let documentation = Some(generate_documentation(writer, attribute, symbol));
            completions.push(CompletionItem {
                label,
                detail,
                kind: Some(CompletionItemKind::PROPERTY),
                documentation,
                ..Default::default()
            });
        }
        Some(())
    }

    /// Checks that the trigger character matches a string.
    pub fn trigger_character_is(&self, arg: &str) -> bool {
        self.context.as_ref().is_some_and(|context| {
            context
                .trigger_character
                .as_ref()
                .is_some_and(|trigger| trigger == arg)
        })
    }

    /// Generate a complete function completion with parameters.
    fn generate_function_completion(&self, name: &str, params: &[analyzer::SymbolIndex]) -> String {
        let mut string = String::from(name);
        string.push_str("(");
        let symbollib = &self.standpoint.symbol_library;
        for (index, param) in params.iter().enumerate() {
            let param_symbol = match symbollib.get_forwarded(*param) {
                Some(symbol) => symbol,
                None => return name.to_string(),
            };
            if matches!(&param_symbol.kind, SemanticSymbolKind::Parameter { is_optional, .. } if *is_optional)
            {
                // Do not print optionals.
                break;
            }
            string.push_str("${");
            string.push_str(&(index + 1).to_string());
            string.push_str(":");
            string.push_str(&param_symbol.name);
            string.push_str("}");
            if index + 1 != params.len() {
                string.push_str(", ");
            }
        }
        string.push_str(")");
        return string;
    }
    /// Generates a completion for a symbol.
    pub fn create_completion(
        &self,
        writer: &SymbolWriter<'_>,
        symbol: &analyzer::SemanticSymbol,
        symbol_idx: SymbolIndex,
        complete_function_params: bool,
    ) -> Option<CompletionItem> {
        let label = symbol.name.clone();
        let symbol_library = &self.standpoint.symbol_library;
        let symbol_idx = symbol_library.forward(symbol_idx);
        let documentation = Some(generate_documentation(&writer, &symbol_idx, symbol));
        let symbol = symbol_library.get_forwarded(symbol_idx)?;
        let (kind, detail) = match &symbol.kind {
            SemanticSymbolKind::Module { .. } => (CompletionItemKind::MODULE, None),
            SemanticSymbolKind::Interface { .. } => (CompletionItemKind::INTERFACE, None),
            SemanticSymbolKind::Model { .. } => (CompletionItemKind::CLASS, None),
            SemanticSymbolKind::Enum { .. } => (CompletionItemKind::ENUM, None),
            SemanticSymbolKind::Variable { inferred_type, .. } => (
                CompletionItemKind::VARIABLE,
                Some(symbol_library.format_evaluated_type(inferred_type)),
            ),
            SemanticSymbolKind::Constant { inferred_type, .. } => (
                CompletionItemKind::CONSTANT,
                Some(symbol_library.format_evaluated_type(inferred_type)),
            ),
            SemanticSymbolKind::Function { .. } => (
                CompletionItemKind::FUNCTION,
                Some(
                    symbol_library.format_evaluated_type(&EvaluatedType::FunctionInstance {
                        function: symbol_idx,
                        generic_arguments: vec![],
                        is_invariant: false,
                    }),
                ),
            ),
            SemanticSymbolKind::TypeName { .. } => (CompletionItemKind::INTERFACE, None),
            _ => return None,
        };
        let mut data = None;
        // Auto importing, if the completion symbol was declared somewhere else.
        if !(symbol.was_declared_in(self.module.path_idx)
            || self
                .standpoint
                .prelude_path
                .is_some_and(|prelude| symbol.was_declared_in(prelude)))
        {
            data = Some(Value::String(String::from("use core.io.Stuff;")));
        }
        Some(CompletionItem {
            label,
            kind: Some(kind),
            detail,
            documentation,
            insert_text: if complete_function_params {
                match &symbol.kind {
                    SemanticSymbolKind::Function { params, .. } => {
                        Some(self.generate_function_completion(&symbol.name, params))
                    }
                    _ => None,
                }
            } else {
                None
            },
            insert_text_format: if complete_function_params {
                Some(InsertTextFormat::SNIPPET)
            } else {
                None
            },
            data,
            ..Default::default()
        })
    }
}

/// Sort an array of completions in alphabetical order.
pub fn sort_completions(completions: &mut Vec<CompletionItem>) {
    // Remove completions with duplicated names.
    let mut unique = HashMap::new();
    completions.iter_mut().for_each(|completion| {
        if !unique.get(&completion.label).is_some() {
            unique.insert(completion.label.clone(), std::mem::take(completion));
        }
    });
    completions.clear();
    for (_, completion) in unique {
        completions.push(completion)
    }
    completions.sort_by(|a, b| {
        a.label
            .chars()
            .next()
            .map(|ch| ch.cmp(&b.label.chars().next().unwrap_or('z')))
            .unwrap_or(std::cmp::Ordering::Less)
    });
}

fn generate_documentation(
    writer: &SymbolWriter<'_>,
    symbol_idx: &analyzer::SymbolIndex,
    symbol: &analyzer::SemanticSymbol,
) -> Documentation {
    let header = writer.print_symbol_with_idx(*symbol_idx);
    let mut value = format!("```wrl\n{header}\n```");
    let symbol = writer.standpoint.symbol_library.get_forwarded(*symbol_idx);
    let symbol = match symbol {
        Some(symbol) => symbol,
        _ => return Documentation::String(String::new()),
    };
    // Documentation?
    if let Some(ref docs) = symbol.doc_info {
        let mut info = String::new();
        for line in docs.iter() {
            info.push_str(line);
            info.push('\n')
        }
        value.push('\n');
        value.push('\n');
        value.push_str(&info);
    };
    let documentation = Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value,
    });
    documentation
}

impl<'a> CompletionFinder<'a> {
    pub fn statement(&self, statement: &'a TypedStmnt) -> Option<CompletionResponse> {
        let span = span_of_typed_statement(
            statement,
            &self.standpoint.symbol_library,
            &self.standpoint.literals,
        );
        let next_statement_span = self.next_statement_span.borrow().clone();
        let single_span = Span::at(self.pos);
        if span.contains(self.pos)
            || (span.is_before(single_span)
                && (next_statement_span.is_none()
                    || next_statement_span.is_some_and(|span| span.is_after(single_span))))
        {
            return match statement {
                TypedStmnt::FunctionDeclaration(f) => self.function(f),
                TypedStmnt::TypeDeclaration(t) => self.type_decl(t),
                TypedStmnt::ModelDeclaration(m) => self.model_decl(m),
                TypedStmnt::ShorthandVariableDeclaration(v) => self.shorthand_var_decl(v),
                TypedStmnt::ExpressionStatement(e) => self.expr_statement(e),
                TypedStmnt::FreeExpression(e) => self.free_expr(e),
                TypedStmnt::InterfaceDeclaration(t) => self.interface_declaration(t),
                TypedStmnt::ModuleDeclaration(m) => self.module_declaration(m),
                TypedStmnt::ConstantDeclaration(c) => self.constant(c),
                TypedStmnt::TestDeclaration(t) => self.test_declaration(t),
                TypedStmnt::ReturnStatement(rettye) => self.return_statement(rettye),
                TypedStmnt::ForStatement(for_stat) => self.for_statement(for_stat),
                TypedStmnt::WhileStatement(whilestat) => self.while_statement(whilestat),
                TypedStmnt::VariableDeclaration(variable) => self.var_decl(variable),
                TypedStmnt::RecordDeclaration => todo!(),
                _ => return None,
            };
        }
        return None;
    }

    fn module_declaration(
        &self,
        module: &'a analyzer::TypedModuleDeclaration,
    ) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }

    fn expr_statement(&self, exp: &'a analyzer::TypedExpression) -> Option<CompletionResponse> {
        self.expr(exp)
    }

    fn free_expr(&self, exp: &'a analyzer::TypedExpression) -> Option<CompletionResponse> {
        self.expr(exp)
    }

    fn expr(&self, exp: &'a analyzer::TypedExpression) -> Option<CompletionResponse> {
        let symbollib = &self.standpoint.symbol_library;
        let literals = &self.standpoint.literals;
        let expression_span = span_of_typed_expression(exp, symbollib, literals);
        // Position is prior, expecting an actual completion.
        // Todo: spacing?
        if expression_span.is_before(Span::at(self.pos)) {
            if self.trigger_character_is(".") && expression_span.is_adjacent_to(self.pos) {
                let inferred_type = symbollib.get_expression_type(exp, literals)?;
                let response = self.complete_from_dot(inferred_type, DotCompletionType::Full);
                return response;
            } else {
                return None;
            }
        }
        // Position is probably nested.
        match exp {
            TypedExpression::NewExpr(n) => self.new_expr(n),
            TypedExpression::CallExpr(c) => self.call_expr(c),
            TypedExpression::FnExpr(f) => self.function_expr(f),
            TypedExpression::IfExpr(i) => self.if_expr(i),
            TypedExpression::ArrayExpr(a) => self.array(a),
            TypedExpression::AccessExpr(a) => self.access(a),
            TypedExpression::IndexExpr(i) => self.index(i),
            TypedExpression::BinaryExpr(b) => self.bin_exp(b),
            TypedExpression::AssignmentExpr(a) => self.ass_exp(a),
            TypedExpression::UnaryExpr(u) => self.un_exp(u),
            TypedExpression::LogicExpr(l) => self.log_exp(l),
            TypedExpression::Block(b) => self.block(b),
            TypedExpression::UpdateExpr(u) => self.update(u),
            _ => None,
        }
    }

    fn if_expr(&self, ifexp: &'a analyzer::TypedIfExpr) -> Option<CompletionResponse> {
        maybe!(self.expr(&ifexp.condition));
        maybe!(self.block(&ifexp.consequent));
        if let Some(el) = &ifexp.alternate {
            maybe!(self.expr(&el.expression));
        }
        <Option<CompletionResponse>>::default()
    }

    fn block(&self, block: &'a analyzer::TypedBlock) -> Option<CompletionResponse> {
        for (index, stat) in block.statements.iter().enumerate() {
            self.next_statement_is(index + 1, &block.statements);
            maybe!(self.statement(stat));
        }
        None
    }

    pub fn next_statement_is(&self, index: usize, statements: &[TypedStmnt]) {
        *self.next_statement_span.borrow_mut() = statements.get(index + 1).map(|statement| {
            span_of_typed_statement(
                statement,
                &self.standpoint.symbol_library,
                &self.standpoint.literals,
            )
        });
    }

    fn log_exp(&self, logexp: &'a analyzer::TypedLogicExpr) -> Option<CompletionResponse> {
        maybe!(self.expr(&logexp.left));
        self.expr(&logexp.right)
    }

    fn un_exp(&self, unexp: &'a analyzer::TypedUnaryExpr) -> Option<CompletionResponse> {
        self.expr(&unexp.operand)
    }

    fn update(&self, update: &'a analyzer::TypedUpdateExpr) -> Option<CompletionResponse> {
        self.expr(&update.operand)
    }

    fn ass_exp(&self, assexp: &'a analyzer::TypedAssignmentExpr) -> Option<CompletionResponse> {
        maybe!(self.expr(&assexp.left));
        self.expr(&assexp.right)
    }

    fn bin_exp(&self, binexp: &'a analyzer::TypedBinExpr) -> Option<CompletionResponse> {
        maybe!(self.expr(&binexp.left));
        self.expr(&binexp.right)
    }

    fn index(&self, index_expr: &'a analyzer::TypedIndexExpr) -> Option<CompletionResponse> {
        maybe!(self.expr(&index_expr.object));
        self.expr(&index_expr.index)
    }

    fn access(&self, acces_expr: &'a analyzer::TypedAccessExpr) -> Option<CompletionResponse> {
        maybe!(self.expr(&acces_expr.object));
        maybe!(self.expr(&acces_expr.property));
        // Account for mistakes when typing out completions.
        let property_span = span_of_typed_expression(
            &acces_expr.property,
            &self.standpoint.symbol_library,
            &self.standpoint.literals,
        );
        if property_span.is_adjacent_to(self.pos) || property_span.contains(self.pos) {
            let object_type = self
                .standpoint
                .symbol_library
                .get_expression_type(&acces_expr.object, &self.standpoint.literals)?;
            let property_name = &match &acces_expr.property {
                TypedExpression::Identifier(i) => self.standpoint.symbol_library.get(i.value),
                _ => return None,
            }?
            .name;
            let response = self.complete_from_dot(object_type, DotCompletionType::Full)?;
            let items = match response {
                CompletionResponse::Array(completions) => completions,
                CompletionResponse::List(list) => list.items,
            };
            return Some(CompletionResponse::Array(
                items
                    .into_iter()
                    .filter(|item| {
                        item.label
                            .to_lowercase()
                            .starts_with(&property_name.to_lowercase())
                    })
                    .collect(),
            ));
        }
        return None;
    }

    fn array(&self, arr: &'a analyzer::TypedArrayExpr) -> Option<CompletionResponse> {
        for elem in &arr.elements {
            maybe!(self.expr(elem));
        }
        <Option<CompletionResponse>>::default()
    }

    fn function_expr(
        &self,
        function_expr: &'a analyzer::TypedFnExpr,
    ) -> Option<CompletionResponse> {
        match &function_expr.body {
            TypedExpression::Block(block) => self.block(block),
            expression => self.expr(&expression),
        }
    }

    fn call_expr(&self, call: &'a analyzer::TypedCallExpr) -> Option<CompletionResponse> {
        maybe!(self.expr(&call.caller));
        for arg in &call.arguments {
            maybe!(self.expr(arg));
        }
        None
    }

    fn type_decl(
        &self,
        type_decl: &'a analyzer::TypedTypeDeclaration,
    ) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }

    fn new_expr(&self, new_exp: &'a analyzer::TypedNewExpr) -> Option<CompletionResponse> {
        self.expr(&new_exp.value)
    }

    fn shorthand_var_decl(
        &self,
        var_decl: &'a analyzer::TypedShorthandVariableDeclaration,
    ) -> Option<CompletionResponse> {
        self.expr(&var_decl.value)
    }

    fn var_decl(
        &self,
        var_decl: &analyzer::TypedVariableDeclaration,
    ) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }

    fn constant(
        &self,
        constant: &analyzer::TypedConstantDeclaration,
    ) -> Option<CompletionResponse> {
        self.expr(&constant.value)
    }

    fn test_declaration(
        &self,
        test: &'a analyzer::TypedTestDeclaration,
    ) -> Option<CompletionResponse> {
        let body = &test.body;
        self.block(body)
    }

    fn for_statement(&self, forstat: &analyzer::TypedForStatement) -> Option<CompletionResponse> {
        maybe!(self.expr(&forstat.iterator));
        self.block(&forstat.body)
    }

    fn while_statement(
        &self,
        _while: &analyzer::TypedWhileStatement,
    ) -> Option<CompletionResponse> {
        maybe!(self.expr(&_while.condition));
        let body = &_while.body;
        self.block(body)
    }

    fn return_statement(
        &self,
        rettye: &analyzer::TypedReturnStatement,
    ) -> Option<CompletionResponse> {
        rettye.value.as_ref().and_then(|value| self.expr(value))
    }

    fn function(
        &self,
        function: &'a analyzer::TypedFunctionDeclaration,
    ) -> Option<CompletionResponse> {
        let body = &function.body;
        self.block(body)
    }

    fn model_decl(&self, model: &analyzer::TypedModelDeclaration) -> Option<CompletionResponse> {
        let mut enclosing = self.enclosing_model_or_interface.borrow_mut();
        let former = enclosing.take();
        *enclosing = Some(model.name);
        std::mem::drop(enclosing);
        maybe!(model
            .body
            .constructor
            .as_ref()
            .and_then(|constructor| { self.block(constructor) }));
        for property in &model.body.properties {
            match &property._type {
                TypedModelPropertyType::TypedMethod { body }
                | TypedModelPropertyType::InterfaceImpl { body, .. } => {
                    maybe!(self.block(body))
                }
                _ => continue,
            }
        }
        *self.enclosing_model_or_interface.borrow_mut() = former;
        return None;
    }

    fn interface_declaration(
        &self,
        _interface: &'a analyzer::TypedInterfaceDeclaration,
    ) -> Option<CompletionResponse> {
        let mut enclosing = self.enclosing_model_or_interface.borrow_mut();
        let former = enclosing.take();
        *enclosing = Some(_interface.name);
        std::mem::drop(enclosing);
        for property in &_interface.body.properties {
            match &property._type {
                TypedInterfacePropertyType::Method { body } => {
                    maybe!(self.block(body))
                }
                _ => continue,
            }
        }
        *self.enclosing_model_or_interface.borrow_mut() = former;
        return None;
    }
}
