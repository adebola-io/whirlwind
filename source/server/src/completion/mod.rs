use crate::message_store::MessageStore;
use analyzer::{
    span_of_typed_expression, span_of_typed_statement, EvaluatedType, SemanticSymbolKind,
    Standpoint, SymbolIndex, TypedExpression, TypedModule, TypedStmnt,
};
use ast::Span;
use printer::SymbolWriter;
use std::cell::RefCell;
use tower_lsp::lsp_types::{
    CompletionContext, CompletionItem, CompletionItemKind, CompletionResponse, Documentation,
    InsertTextFormat, MarkupContent, MarkupKind,
};

/// Exits a function and returns an option value if it is Some.
/// Basically the direct opposite of `impl Try for Option`.
macro_rules! maybe {
    ($exp: expr) => {
        if let Some(exp) = $exp {
            return Some(exp);
        }
    };
}

/// A visitor that traverses a typed module to determine the most suitable
/// completion response for a completion prompt.
pub struct CompletionFinder<'a> {
    _module: &'a TypedModule,
    standpoint: &'a Standpoint,
    pos: [u32; 2],
    completion_context: Option<CompletionContext>,
    pub message_store: RefCell<MessageStore>,
    next_statement_span: RefCell<Option<Span>>,
}

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
            _module: module,
            pos,
            message_store: RefCell::new(messages),
            completion_context,
            next_statement_span: RefCell::new(None),
        }
    }
    /// Main complete function for dot access.
    pub fn complete_from_dot(
        &self,
        inferred_type: EvaluatedType,
        completion_type: DotCompletionType,
    ) -> Option<CompletionResponse> {
        let writer = SymbolWriter::new(&self.standpoint);
        let symboltable = &self.standpoint.symbol_table;
        self.message_store
            .borrow_mut()
            .inform(format!("Completing {inferred_type:?}"));
        let mut completions = vec![];
        match inferred_type {
            EvaluatedType::Partial { types } => {
                return self.complete_from_dot(types.first()?.clone(), completion_type)
            }
            EvaluatedType::ModelInstance { model, .. } => {
                if matches!(completion_type, DotCompletionType::Half) {
                    return None;
                }
                let model_symbol = self.standpoint.symbol_table.get(model)?;
                let (methods, attributes) = match &model_symbol.kind {
                    SemanticSymbolKind::Model {
                        methods,
                        attributes,
                        ..
                    } => (methods, attributes),
                    _ => return None,
                };
                for attribute in attributes {
                    let symbol = self.standpoint.symbol_table.get(*attribute)?;
                    if !symbol.kind.is_public() {
                        continue; // todo: allow private completions in appriopriate context.
                    }
                    let label = symbol.name.clone();
                    let documentation = Some(generate_documentation(&writer, attribute, symbol));
                    completions.push(CompletionItem {
                        label,
                        kind: Some(CompletionItemKind::PROPERTY),
                        documentation,
                        ..Default::default()
                    });
                }
                for method in methods {
                    let symbol = self.standpoint.symbol_table.get(*method)?;
                    if !symbol.kind.is_public() {
                        continue; // todo: allow private completions in appriopriate context.
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
                    let label = symbol.name.clone();
                    let documentation = Some(generate_documentation(&writer, method, symbol));
                    completions.push(CompletionItem {
                        label,
                        kind: Some(CompletionItemKind::METHOD),
                        documentation,
                        insert_text: Some(self.generate_function_completion(&symbol.name, &params)),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        ..Default::default()
                    });
                }
            }
            // EvaluatedType::TraitInstance {
            //     trait_,
            //     generic_arguments,
            // } => todo!(),
            // EvaluatedType::EnumInstance {
            //     enum_,
            //     generic_arguments,
            // } => todo!(),
            // EvaluatedType::FunctionInstance {
            //     function,
            //     generic_arguments,
            // } => todo!(),
            // EvaluatedType::FunctionExpressionInstance {
            //     is_async,
            //     params,
            //     return_type,
            //     generic_args,
            // } => todo!(),
            // EvaluatedType::MethodInstance {
            //     method,
            //     generic_arguments,
            // } => todo!(),
            // EvaluatedType::Model(_) => todo!(),
            // EvaluatedType::Trait(_) => todo!(),
            // EvaluatedType::Enum(_) => todo!(),
            // Completion after a module name.
            EvaluatedType::Module(module) => {
                let module_symbol = self.standpoint.symbol_table.get_forwarded(module)?;
                if let SemanticSymbolKind::Module {
                    global_declaration_symbols,
                    ..
                } = &module_symbol.kind
                {
                    for symbol_index in global_declaration_symbols {
                        // Not get_forwarded because publicity has to be checked.
                        let symbol = match symboltable.get(*symbol_index) {
                            Some(symbol) => symbol,
                            None => continue,
                        };
                        if !symbol.kind.is_public() {
                            continue;
                        }
                        // Specially complete functions.
                        let symbol = symboltable.get_forwarded(*symbol_index).unwrap();
                        let symbol_index = symboltable.forward(*symbol_index);
                        if matches!(completion_type, DotCompletionType::Full) {
                            if let SemanticSymbolKind::Function { params, .. } = &symbol.kind {
                                let label = symbol.name.clone();
                                let insert_text =
                                    Some(self.generate_function_completion(&symbol.name, params));
                                let documentation =
                                    Some(generate_documentation(&writer, &symbol_index, symbol));
                                completions.push(CompletionItem {
                                    label,
                                    kind: Some(CompletionItemKind::FUNCTION),
                                    documentation,
                                    insert_text,
                                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                                    ..Default::default()
                                });
                                continue;
                            }
                        }
                        if let Some(completionitem) =
                            self.create_completion_item(&writer, symbol, symbol_index)
                        {
                            completions.push(completionitem);
                        }
                    }
                }
            }
            // EvaluatedType::OpaqueTypeInstance {
            //     methods,
            //     properties,
            //     implementations,
            //     collaborators,
            // } => todo!(),
            // EvaluatedType::Void => todo!(),
            // EvaluatedType::Never => todo!(),
            // EvaluatedType::Unknown => todo!(),
            // EvaluatedType::Generic { base } => todo!(),
            // EvaluatedType::Borrowed { base } => todo!(),
            _ => return None,
        }
        completions.sort_by(|a, b| {
            a.label
                .chars()
                .next()
                .map(|ch| ch.cmp(&b.label.chars().next().unwrap_or('z')))
                .unwrap_or(std::cmp::Ordering::Less)
        });
        return Some(CompletionResponse::Array(completions));
    }

    /// Checks that the trigger character matches a string.
    pub fn trigger_character_is(&self, arg: &str) -> bool {
        self.completion_context.as_ref().is_some_and(|context| {
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
        let symboltable = &self.standpoint.symbol_table;
        for (index, param) in params.iter().enumerate() {
            let param_symbol = match symboltable.get_forwarded(*param) {
                Some(symbol) => symbol,
                None => return name.to_string(),
            };
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
    fn create_completion_item(
        &self,
        writer: &SymbolWriter<'_>,
        symbol: &analyzer::SemanticSymbol,
        symbol_idx: SymbolIndex,
    ) -> Option<CompletionItem> {
        let label = symbol.name.clone();
        let documentation = Some(generate_documentation(&writer, &symbol_idx, symbol));
        let symbol = self.standpoint.symbol_table.get_forwarded(symbol_idx)?;
        let kind = Some(match &symbol.kind {
            SemanticSymbolKind::Module { .. } => CompletionItemKind::MODULE,
            SemanticSymbolKind::Trait { .. } => CompletionItemKind::INTERFACE,
            SemanticSymbolKind::Model { .. } => CompletionItemKind::CLASS,
            SemanticSymbolKind::Enum { .. } => CompletionItemKind::ENUM,
            SemanticSymbolKind::Variable { .. } => CompletionItemKind::VARIABLE,
            SemanticSymbolKind::Constant { .. } => CompletionItemKind::CONSTANT,
            SemanticSymbolKind::Function { .. } => CompletionItemKind::FUNCTION,
            SemanticSymbolKind::TypeName { .. } => CompletionItemKind::TYPE_PARAMETER,
            _ => return None,
        });
        Some(CompletionItem {
            label,
            kind,
            documentation,
            ..Default::default()
        })
    }
}

fn generate_documentation(
    writer: &SymbolWriter<'_>,
    symbol_idx: &analyzer::SymbolIndex,
    symbol: &analyzer::SemanticSymbol,
) -> Documentation {
    let header = writer.print_symbol_with_idx(*symbol_idx);
    let mut value = format!("```wrl\n{header}\n```");
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
            &self.standpoint.symbol_table,
            &self.standpoint.literals,
        );
        let next_statement_span = self.next_statement_span.borrow().clone();
        // None;
        let single_span = Span::at(self.pos);
        if span.contains(self.pos)
            || (span.is_before(single_span)
                && (next_statement_span.is_none()
                    || next_statement_span.is_some_and(|span| span.is_after(single_span))))
        {
            return match statement {
                TypedStmnt::FunctionDeclaration(f) => self.function(f),
                TypedStmnt::TypeDeclaration(t) => self.type_decl(t),
                TypedStmnt::EnumDeclaration(e) => self.enum_decl(e),
                TypedStmnt::ModelDeclaration(m) => self.model_decl(m),
                TypedStmnt::ShorthandVariableDeclaration(v) => self.shorthand_var_decl(v),
                TypedStmnt::ExpressionStatement(e) => self.expr_statement(e),
                TypedStmnt::FreeExpression(e) => self.free_expr(e),
                TypedStmnt::TraitDeclaration(t) => self.trait_declaration(t),
                TypedStmnt::ModuleDeclaration(m) => self.module_declaration(m),
                TypedStmnt::ConstantDeclaration(c) => self.constant(c),
                TypedStmnt::TestDeclaration(t) => self.test_declaration(t),
                TypedStmnt::ReturnStatement(rettye) => self.return_statement(rettye),
                TypedStmnt::BreakStatement(brk) => self.break_statement(brk),
                TypedStmnt::ForStatement(for_stat) => self.for_statement(for_stat),
                TypedStmnt::WhileStatement(whilestat) => self.while_statement(whilestat),
                TypedStmnt::ContinueStatement(continue_) => self.continue_statement(continue_),
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

    fn trait_declaration(
        &self,
        _trait: &'a analyzer::TypedTraitDeclaration,
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
        self.message_store
            .borrow_mut()
            .inform(format!("Checking for completion in expression. {exp:?}"));
        let symboltable = &self.standpoint.symbol_table;
        let literals = &self.standpoint.literals;
        let expression_span = span_of_typed_expression(exp, symboltable, literals);
        // Position is prior, expecting an actual completion.
        // Todo: spacing?
        if expression_span.is_before(Span::at(self.pos)) {
            if self.trigger_character_is(".") && expression_span.is_adjacent_to(self.pos) {
                let inferred_type = symboltable.get_expression_type(exp, literals)?;
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
                &self.standpoint.symbol_table,
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
            &self.standpoint.symbol_table,
            &self.standpoint.literals,
        );
        self.message_store
            .borrow_mut()
            .inform("Checking for mistakes...");
        if property_span.is_adjacent_to(self.pos) || property_span.contains(self.pos) {
            let object_type = self
                .standpoint
                .symbol_table
                .get_expression_type(&acces_expr.object, &self.standpoint.literals)?;
            let property_name = &match &acces_expr.property {
                TypedExpression::Identifier(i) => self.standpoint.symbol_table.get(i.value),
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
                    .filter(|item| item.label.starts_with(property_name))
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
        self.expr(&function_expr.body)
    }

    fn call_expr(&self, call: &'a analyzer::TypedCallExpr) -> Option<CompletionResponse> {
        maybe!(self.expr(&call.caller));
        for arg in &call.arguments {
            maybe!(self.expr(arg));
        }
        <Option<CompletionResponse>>::default()
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
        <Option<CompletionResponse>>::default()
    }

    fn test_declaration(
        &self,
        test: &'a analyzer::TypedTestDeclaration,
    ) -> Option<CompletionResponse> {
        let body = &test.body;
        for (index, statement) in body.statements.iter().enumerate() {
            self.next_statement_is(index + 1, &body.statements);
            maybe!(self.statement(statement))
        }
        None
    }

    fn break_statement(&self, brk: &analyzer::TypedBreakStatement) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }

    fn for_statement(&self, forstat: &analyzer::TypedForStatement) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }

    fn while_statement(
        &self,
        _while: &analyzer::TypedWhileStatement,
    ) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }

    fn continue_statement(
        &self,
        cont: &analyzer::TypedContinueStatement,
    ) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }

    fn return_statement(
        &self,
        rettye: &analyzer::TypedReturnStatement,
    ) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }

    fn function(
        &self,
        function: &'a analyzer::TypedFunctionDeclaration,
    ) -> Option<CompletionResponse> {
        self.message_store
            .borrow_mut()
            .inform("Searching for completion in function...");
        let body = &function.body;
        for (index, statement) in body.statements.iter().enumerate() {
            self.next_statement_is(index, &body.statements);
            maybe!(self.statement(statement));
        }
        None
    }

    fn enum_decl(&self, enum_decl: &analyzer::TypedEnumDeclaration) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }

    fn model_decl(&self, model: &analyzer::TypedModelDeclaration) -> Option<CompletionResponse> {
        <Option<CompletionResponse>>::default()
    }
}
