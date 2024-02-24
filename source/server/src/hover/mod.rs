use crate::message_store::MessageStore;
use analyzer::{
    IntermediateType, IntermediateTypeClause, SemanticSymbol, SemanticSymbolKind, Standpoint,
    SymbolIndex, TypedCallExpr, TypedIdent, TypedModule, TypedThisExpr, TypedTypeEquation,
    TypedVisitorNoArgs,
};
use ast::{maybe, within, Span};
use pretty::SymbolWriter;
use std::cell::RefCell;
use tower_lsp::lsp_types::{Hover, HoverContents, LanguageString, MarkedString};

/// Information shown during hovering.
pub struct HoverInfo {
    pub contents: HoverContents,
}

impl HoverInfo {
    fn from_str(value: &str) -> HoverInfo {
        HoverInfo {
            contents: HoverContents::Array(vec![MarkedString::LanguageString(LanguageString {
                language: format!("wrl"),
                value: value.to_owned(),
            })]),
        }
    }
}

impl From<(&Standpoint, SymbolIndex)> for HoverInfo {
    fn from(tuple: (&Standpoint, SymbolIndex)) -> Self {
        let mut contents = vec![];
        let symbol_library = &tuple.0.symbol_library;
        let symbol = symbol_library.get(tuple.1).unwrap();
        let writer = SymbolWriter::new(tuple.0);
        let string = writer.print_symbol_with_idx(tuple.1);

        // Import and property redirection.
        let (symbol, is_opaque) = match &symbol.kind {
            SemanticSymbolKind::Import {
                source: Some(source),
                ..
            } => {
                let mut parent = symbol_library.get(*source);
                while let Some(SemanticSymbol {
                    kind:
                        SemanticSymbolKind::Import {
                            source: Some(source),
                            ..
                        },
                    ..
                }) = parent
                {
                    parent = symbol_library.get(*source);
                }
                (parent.unwrap(), false)
            }
            SemanticSymbolKind::Property {
                resolved: Some(origin),
                is_opaque,
            } => (tuple.0.symbol_library.get(*origin).unwrap(), *is_opaque),
            _ => (symbol, false),
        };

        contents.push(MarkedString::LanguageString(LanguageString {
            language: String::from("wrl"),
            value: string,
        }));
        // Documentation?
        if let Some(ref docs) = symbol.doc_info {
            if !is_opaque {
                let mut documentation = String::new();
                for line in docs.iter() {
                    documentation.push_str(line);
                    documentation.push('\n')
                }
                contents.push(MarkedString::String(documentation))
            }
        }
        // Reference symbols.

        return HoverInfo {
            contents: HoverContents::Array(contents),
        };
    }
}

impl From<HoverInfo> for Hover {
    fn from(value: HoverInfo) -> Self {
        Hover {
            contents: value.contents,
            range: None,
        }
    }
}

pub struct HoverFinder<'a> {
    module: &'a TypedModule,
    standpoint: &'a Standpoint,
    pos: [u32; 2],
    pub message_store: RefCell<MessageStore>,
}

impl<'a> HoverFinder<'a> {
    pub fn new(
        module: &'a TypedModule,
        context: &'a Standpoint,
        pos: [u32; 2],
        messages: MessageStore,
    ) -> Self {
        Self {
            standpoint: context,
            module,
            pos,
            message_store: RefCell::new(messages),
        }
    }
}

impl<'a> TypedVisitorNoArgs<Option<HoverInfo>> for HoverFinder<'a> {
    /// Hover over a use import.
    fn use_declaration(&self, use_decl: &analyzer::TypedUseDeclaration) -> Option<HoverInfo> {
        within!(use_decl.span, self);
        for import in &use_decl.imports {
            let symbol = self.standpoint.symbol_library.get(*import)?;
            if symbol.ident_span().contains(self.pos) {
                return Some(HoverInfo::from((self.standpoint, *import)));
            }
        }
        None
    }
    fn import_decl(&self, import: &analyzer::TypedImportDeclaration) -> Option<HoverInfo> {
        within!(import.span, self);
        for (_, idx) in &import.imports {
            let symbol = self.standpoint.symbol_library.get(*idx)?;
            // Hovering over imported function name.
            if symbol.ident_span().contains(self.pos) {
                return Some(HoverInfo::from((self.standpoint, *idx)));
            };
            if let SemanticSymbolKind::Function {
                params,
                generic_params,
                return_type,
                ..
            } = &symbol.kind
            {
                maybe!(self.fn_parts_hover(generic_params, params, return_type, None, None))
            }
        }
        return None;
    }
    /// Hovering over a variable declaration.
    fn var_decl(&self, var_decl: &analyzer::TypedVariableDeclaration) -> Option<HoverInfo> {
        within!(var_decl.span, self);
        let mut type_already_checked = false;
        for name in &var_decl.names {
            let symbol = self.standpoint.symbol_library.get(*name)?;
            // Hovering over variable name.
            if symbol.ident_span().contains(self.pos) {
                return Some(HoverInfo::from((self.standpoint, *name)));
            };
            if !type_already_checked {
                let typ = match &symbol.kind {
                    SemanticSymbolKind::Variable { declared_type, .. } => declared_type,
                    _ => continue,
                };
                if let Some(intermediate_type) = typ {
                    maybe!(self.type_hover(intermediate_type))
                }
                type_already_checked = true;
            }
        }
        var_decl.value.as_ref().and_then(|value| self.expr(value))
    }

    /// Hovering over a type declaration.:
    fn type_decl(&self, type_decl: &TypedTypeEquation) -> Option<HoverInfo> {
        within!(type_decl.span, self);
        let symbol = self.standpoint.symbol_library.get(type_decl.name)?;
        // Hovering over the type name.
        if symbol.ident_span().contains(self.pos) {
            self.message_store
                .borrow_mut()
                .inform("Hovering over a type name...");
            return Some(HoverInfo::from((self.standpoint, type_decl.name)));
        };
        let (generic_params, value) = match &symbol.kind {
            SemanticSymbolKind::TypeName {
                generic_params,
                value,
                ..
            } => (generic_params, value),
            _ => return None,
        };
        // Hovering over a generic parameter.
        for generic_param in generic_params {
            maybe!(self.generic_parameter(*generic_param))
        }
        // Hovering over the type values.
        maybe!(self.type_hover(value));
        return None;
    }
    /// Hover over a shorthand variable declaration.
    fn shorthand_var_decl(
        &self,
        var_decl: &analyzer::TypedShorthandVariableDeclaration,
    ) -> Option<HoverInfo> {
        within!(var_decl.span, self);
        let symbol = self.standpoint.symbol_library.get(var_decl.name)?;
        // Hovering over a variable name.
        if symbol.ident_span().contains(self.pos) {
            return Some(HoverInfo::from((self.standpoint, var_decl.name)));
        }
        // Hovering over variable type.
        let declared_type = match &symbol.kind {
            SemanticSymbolKind::Variable { declared_type, .. } => declared_type,
            _ => return None,
        };
        if let Some(ref _type) = declared_type {
            maybe!(self.type_hover(_type))
        }
        // hovering over the variables' value.
        return self.expr(&var_decl.value);
    }
    /// Hovering over a function.
    fn function(&self, function: &analyzer::TypedFunctionDeclaration) -> Option<HoverInfo> {
        within!(function.span, self);
        let symbol = self.standpoint.symbol_library.get(function.name)?;
        // Hovering over the function name.
        if symbol.ident_span().contains(self.pos) {
            self.message_store
                .borrow_mut()
                .inform("Hovering over a function name...");
            return Some(HoverInfo::from((self.standpoint, function.name)));
        };
        let (params, generic_params, return_type) = match &symbol.kind {
            SemanticSymbolKind::Function {
                params,
                generic_params,
                return_type,
                ..
            } => (params, generic_params, return_type),
            _ => return None,
        };
        let body = &function.body;
        // hovering over function parts.
        return self.fn_parts_hover(generic_params, params, return_type, None, Some(body));
    }
    /// Hover over an enum declaration.
    fn enum_decl(&self, enum_decl: &analyzer::TypedEnumDeclaration) -> Option<HoverInfo> {
        within!(enum_decl.span, self);
        let symbol = self.standpoint.symbol_library.get(enum_decl.name)?;
        // Hovering over the enum name.
        if symbol.ident_span().contains(self.pos) {
            self.message_store
                .borrow_mut()
                .inform("Hovering over a enum name...");
            return Some(HoverInfo::from((self.standpoint, enum_decl.name)));
        };
        // Hovering over a generic param.
        let (generic_params, variants) = match &symbol.kind {
            SemanticSymbolKind::Enum {
                generic_params,
                variants,
                ..
            } => (generic_params, variants),
            _ => return None,
        };
        for generic_param in generic_params {
            maybe!(self.generic_parameter(*generic_param))
        }
        // Hovering over a variant.
        for variant in variants {
            let symbol = self.standpoint.symbol_library.get(*variant)?;
            // let references = symbol
            //     .references
            //     .iter()
            //     .find(|reflist| reflist.module_path == self.module.path_idx)?;
            // for span_start in references.starts.iter() {
            //     let span = Span::on_line(*span_start, symbol.name.len() as u32);
            if symbol.ident_span().contains(self.pos) {
                self.message_store
                    .borrow_mut()
                    .inform("Hovering over enumerated variant...");
                return Some(HoverInfo::from((self.standpoint, *variant)));
                // }
            }
        }
        return None;
    }
    /// Hovering over a call expression.
    fn call_expr(&self, call: &TypedCallExpr) -> Option<HoverInfo> {
        // Hovering over the caller.
        maybe!(self.expr(&call.caller));
        // Hovering over an argument.
        for argument in call.arguments.iter() {
            maybe!(self.expr(argument));
        }
        return None;
    }
    /// Hovering over `this`.
    fn this_expr(&self, _this: &TypedThisExpr) -> Option<HoverInfo> {
        let span_start = [_this.start_line, _this.start_character];
        let span = Span::on_line(span_start, 4);
        within!(span, self);
        match _this.model_or_interface {
            Some(meaning) => Some(HoverInfo::from((self.standpoint, meaning))),
            None => Some(HoverInfo::from_str("this: {unknown}")),
        }
    }
    /// Hovering over an identifier.
    fn identifier(&self, ident: &TypedIdent) -> Option<HoverInfo> {
        let symbol_idx = ident.value;
        let symbol = self.standpoint.symbol_library.get(symbol_idx)?;
        let references = symbol
            .references
            .iter()
            .find(|reflist| reflist.module_path == self.module.path_idx)?;
        for span_start in references.starts.iter() {
            let span = Span::on_line(*span_start, symbol.name.len() as u32);
            if span.contains(self.pos) {
                self.message_store
                    .borrow_mut()
                    .inform("Hovering over identifier...");
                return Some(HoverInfo::from((self.standpoint, symbol_idx)));
            }
        }
        None
    }

    fn module_declaration(&self, module: &analyzer::TypedModuleDeclaration) -> Option<HoverInfo> {
        within!(module.span, self);
        return Some(HoverInfo::from((self.standpoint, self.module.symbol_idx)));
    }

    fn if_expr(&self, ifexp: &analyzer::TypedIfExpr) -> Option<HoverInfo> {
        within!(ifexp.span, self);
        maybe!(self.expr(&ifexp.condition));
        maybe!(self.block(&ifexp.consequent));
        ifexp
            .alternate
            .as_ref()
            .map(|els| self.expr(&els.expression))
            .flatten()
    }

    fn block(&self, block: &analyzer::TypedBlock) -> Option<HoverInfo> {
        within!(block.span, self);
        for stat in &block.statements {
            maybe!(self.statement(stat));
        }
        None
    }

    fn log_exp(&self, logexp: &analyzer::TypedLogicExpr) -> Option<HoverInfo> {
        within!(logexp.span, self);
        maybe!(self.expr(&logexp.left));
        self.expr(&logexp.right)
    }

    fn un_exp(&self, unexp: &analyzer::TypedUnaryExpr) -> Option<HoverInfo> {
        within!(unexp.span, self);
        self.expr(&unexp.operand)
    }

    fn update(&self, update: &analyzer::TypedUpdateExpr) -> Option<HoverInfo> {
        within!(update.span, self);
        self.expr(&update.operand)
    }

    fn ass_exp(&self, assexp: &analyzer::TypedAssignmentExpr) -> Option<HoverInfo> {
        within!(assexp.span, self);
        maybe!(self.expr(&assexp.left));
        self.expr(&assexp.right)
    }

    fn bin_exp(&self, binexp: &analyzer::TypedBinExpr) -> Option<HoverInfo> {
        within!(binexp.span, self);
        maybe!(self.expr(&binexp.left));
        self.expr(&binexp.right)
    }

    fn index(&self, index_expr: &analyzer::TypedIndexExpr) -> Option<HoverInfo> {
        within!(index_expr.span, self);
        maybe!(self.expr(&index_expr.object));
        self.expr(&index_expr.index)
    }

    fn access(&self, acces_expr: &analyzer::TypedAccessExpr) -> Option<HoverInfo> {
        within!(acces_expr.span, self);
        maybe!(self.expr(&acces_expr.object));
        self.expr(&acces_expr.property)
    }

    fn array(&self, arr: &analyzer::TypedArrayExpr) -> Option<HoverInfo> {
        for elem in &arr.elements {
            maybe!(self.expr(elem));
        }
        None
    }

    fn function_expr(&self, function_expr: &analyzer::TypedFnExpr) -> Option<HoverInfo> {
        maybe!(self.fn_parts_hover(
            &function_expr.generic_params,
            &function_expr.params,
            &function_expr.return_type,
            None,
            None
        ));
        self.expr(&function_expr.body)
    }

    fn literal(&self, _literal: &analyzer::LiteralIndex) -> Option<HoverInfo> {
        None
    }

    fn constant(&self, constant: &analyzer::TypedConstantDeclaration) -> Option<HoverInfo> {
        within!(constant.span, self);
        let symbol = self.standpoint.symbol_library.get(constant.name)?;
        // Hovering over a variable name.
        if symbol.ident_span().contains(self.pos) {
            return Some(HoverInfo::from((self.standpoint, constant.name)));
        }
        // Hovering over variable type.
        let declared_type = match &symbol.kind {
            SemanticSymbolKind::Constant { declared_type, .. } => declared_type,
            _ => return None,
        };
        maybe!(self.type_hover(declared_type));
        // hovering over the variables' value.
        self.expr(&constant.value)
    }

    fn model_decl(&self, model: &analyzer::TypedModelDeclaration) -> Option<HoverInfo> {
        within!(model.span, self);
        // hovering over a model name.
        let model_symbol = self.standpoint.symbol_library.get(model.name)?;
        if model_symbol.ident_span().contains(self.pos) {
            return Some(HoverInfo::from((self.standpoint, model.name)));
        }
        // hovering over a generic parameter.
        let (generic_params, impls, params) = match &model_symbol.kind {
            SemanticSymbolKind::Model {
                generic_params,
                interfaces: implementations,
                constructor_parameters,
                ..
            } => (generic_params, implementations, constructor_parameters),
            _ => return None,
        };
        for generic_param in generic_params {
            maybe!(self.generic_parameter(*generic_param))
        }
        // hovering over an implementation.
        for _impl in impls {
            maybe!(self.type_hover(_impl))
        }
        // hovering over the model body.
        within!(model.body.span, self);
        // hovering inside the model constructor.
        maybe!(model.body.constructor.as_ref().and_then(|constructor| {
            params
                .as_ref()
                .and_then(|params| params.iter().find_map(|param| self.parameter(*param)))
                .or_else(|| self.block(&constructor))
        }));
        // hovering in a property.
        for property in model.body.properties.iter() {
            if !property.span.contains(self.pos) {
                continue;
            }
            let symbol = self.standpoint.symbol_library.get(property.name)?;
            // Hovering over the property name.
            if symbol.ident_span().contains(self.pos) {
                return Some(HoverInfo::from((self.standpoint, property.name)));
            }
            match &property._type {
                analyzer::TypedModelPropertyType::TypedAttribute => {
                    // Hovering over the attribute type.
                    let _type = match &symbol.kind {
                        SemanticSymbolKind::Attribute { declared_type, .. } => declared_type,
                        _ => return None,
                    };
                    return self.type_hover(_type);
                }
                analyzer::TypedModelPropertyType::TypedMethod { body } => match &symbol.kind {
                    SemanticSymbolKind::Method {
                        params,
                        generic_params,
                        return_type,
                        constraint,
                        ..
                    } => {
                        return self.fn_parts_hover(
                            generic_params,
                            params,
                            return_type,
                            constraint.as_ref().map(|(c, _)| c),
                            Some(body),
                        )
                    }
                    _ => return None,
                },
                analyzer::TypedModelPropertyType::InterfaceImpl {
                    interface_target,
                    body,
                } => {
                    for interface_target_unit in interface_target {
                        maybe!(self.type_hover(interface_target_unit))
                    }
                    match &symbol.kind {
                        SemanticSymbolKind::Method {
                            params,
                            generic_params,
                            return_type,
                            constraint,
                            ..
                        } => {
                            return self.fn_parts_hover(
                                generic_params,
                                params,
                                return_type,
                                constraint.as_ref().map(|(c, _)| c),
                                Some(body),
                            )
                        }
                        _ => return None,
                    }
                }
            }
        }
        None
    }

    fn interface_declaration(
        &self,
        interface_: &analyzer::TypedInterfaceDeclaration,
    ) -> Option<HoverInfo> {
        within!(interface_.span, self);
        // hovering over a interface name.
        let model_symbol = self.standpoint.symbol_library.get(interface_.name)?;
        if model_symbol.ident_span().contains(self.pos) {
            return Some(HoverInfo::from((self.standpoint, interface_.name)));
        }
        // hovering over a generic parameter.
        let (generic_params, impls) = match &model_symbol.kind {
            SemanticSymbolKind::Interface {
                generic_params,
                interfaces: implementations,
                ..
            } => (generic_params, implementations),
            _ => return None,
        };
        for generic_param in generic_params {
            maybe!(self.generic_parameter(*generic_param))
        }
        // hovering over an implementation.
        for _impl in impls {
            maybe!(self.type_hover(_impl))
        }
        // hovering over the interface body.
        within!(interface_.body.span, self);
        // hovering in a property.
        for property in interface_.body.properties.iter() {
            if !property.span.contains(self.pos) {
                continue;
            }
            let symbol = self.standpoint.symbol_library.get(property.name)?;
            // Hovering over the property name.
            if symbol.ident_span().contains(self.pos) {
                return Some(HoverInfo::from((self.standpoint, property.name)));
            }
            match &property._type {
                analyzer::TypedInterfacePropertyType::Method { body } => match &symbol.kind {
                    SemanticSymbolKind::Method {
                        params,
                        generic_params,
                        return_type,
                        constraint,
                        ..
                    } => {
                        return self.fn_parts_hover(
                            generic_params,
                            params,
                            return_type,
                            constraint.as_ref().map(|(c, _)| c),
                            Some(body),
                        )
                    }
                    _ => return None,
                },
                analyzer::TypedInterfacePropertyType::Signature => match &symbol.kind {
                    SemanticSymbolKind::Method {
                        params,
                        generic_params,
                        return_type,
                        constraint,
                        ..
                    } => {
                        return self.fn_parts_hover(
                            generic_params,
                            params,
                            return_type,
                            constraint.as_ref().map(|(c, _)| c),
                            None,
                        )
                    }
                    _ => panic!("{symbol:?}"),
                },
            }
        }
        None
    }

    fn test_declaration(&self, test: &analyzer::TypedTestDeclaration) -> Option<HoverInfo> {
        within!(test.span, self);
        self.block(&test.body)
    }

    fn break_statement(&self, brk: &analyzer::TypedBreakStatement) -> Option<HoverInfo> {
        within!(brk.span, self);
        brk.label.as_ref().and_then(|ident| self.identifier(ident))
    }

    fn for_statement(&self, forstat: &analyzer::TypedForStatement) -> Option<HoverInfo> {
        within!(forstat.span, self);
        // hovering over the loop variables.
        for name in &forstat.items {
            let symbol = self.standpoint.symbol_library.get(*name)?;
            // Hovering over variable name.
            if symbol.ident_span().contains(self.pos) {
                self.message_store
                    .borrow_mut()
                    .inform("Hovering over loop variable name...");
                return Some(HoverInfo::from((self.standpoint, *name)));
            };
        }
        // hovering over label.
        maybe!(forstat
            .label
            .as_ref()
            .and_then(|ident| Some((ident, self.standpoint.symbol_library.get(*ident)?)))
            .and_then(|(idx, symbol)| {
                symbol
                    .ident_span()
                    .contains(self.pos)
                    .then(|| HoverInfo::from((self.standpoint, *idx)))
            }));
        // hovering over loop iterator.
        maybe!(self.expr(&forstat.iterator));
        self.block(&forstat.body)
    }

    fn while_statement(&self, _while: &analyzer::TypedWhileStatement) -> Option<HoverInfo> {
        within!(_while.span, self);
        maybe!(self.expr(&_while.condition));
        self.block(&_while.body)
    }

    fn continue_statement(&self, cont: &analyzer::TypedContinueStatement) -> Option<HoverInfo> {
        within!(cont.span, self);
        cont.label.as_ref().and_then(|ident| self.identifier(ident))
    }

    fn return_statement(&self, rettye: &analyzer::TypedReturnStatement) -> Option<HoverInfo> {
        within!(rettye.span, self);
        rettye.value.as_ref().and_then(|expr| self.expr(expr))
    }
}

impl HoverFinder<'_> {
    /// Recursively pinpoint when hovering over types.
    fn type_hover(&self, _type: &IntermediateType) -> Option<HoverInfo> {
        match _type {
            IntermediateType::FunctionType {
                params,
                return_type,
                span,
            } => {
                within!(span, self);
                for param in params {
                    maybe!(param
                        .type_label
                        .as_ref()
                        .and_then(|param_type| self.type_hover(param_type)))
                }

                maybe!(return_type
                    .as_ref()
                    .and_then(|param_type| self.type_hover(param_type)))
            }
            IntermediateType::SimpleType {
                value,
                generic_args,
                span,
            } => {
                within!(span, self);
                let symbol = self.standpoint.symbol_library.get(*value)?;
                // Hovering over type name.
                // Try to get this reference.
                // There should be a better way to do this, but for now, abeg.
                let references = symbol
                    .references
                    .iter()
                    .find(|reflist| reflist.module_path == self.module.path_idx)?;
                for span_start in references.starts.iter() {
                    let span = Span::on_line(*span_start, symbol.name.len() as u32);
                    if span.contains(self.pos) {
                        self.message_store
                            .borrow_mut()
                            .inform("Hovering over a type.");
                        return Some(HoverInfo::from((self.standpoint, *value)));
                    }
                }
                // Hovering over a discrete type generic argument.
                for generic_arg in generic_args {
                    maybe!(self.type_hover(generic_arg))
                }
            }
            IntermediateType::UnionType { types, span } => {
                within!(span, self);
                for itmd_type in types {
                    maybe!(self.type_hover(itmd_type))
                }
            }
            IntermediateType::This { meaning, span } => {
                within!(span, self);
                self.message_store
                    .borrow_mut()
                    .inform("Hovering over This.");
                if let Some(meaning) = meaning {
                    return Some(HoverInfo::from((self.standpoint, *meaning)));
                } else {
                    return Some(HoverInfo::from_str("This: {unknown}"));
                }
            }
            IntermediateType::MaybeType { value, span } => {
                within!(span, self);
                maybe!(self.type_hover(value));
            }
            IntermediateType::Placeholder => {
                unreachable!("Attempted a hover over a placeholder intermediate type. How???")
            }
            IntermediateType::MemberType {
                object,
                property,
                span,
            } => {
                within!(span, self);
                // Hovering over the namespace.
                maybe!(self.type_hover(&object));
                // Hovering over the property.
                if property.span.contains(self.pos) {
                    if let Some(origin) = property.actual {
                        self.message_store
                            .borrow_mut()
                            .inform("Hovering over a type (property).");
                        return Some(HoverInfo::from((self.standpoint, origin)));
                    }
                    return Some(HoverInfo::from_str(&format!(
                        "(property) {}: {{unknown}}",
                        property.name
                    )));
                }
                for typ in &property.generic_args {
                    maybe!(self.type_hover(typ))
                }
            }
            IntermediateType::ArrayType { element_type, span } => {
                within!(span, self);
                maybe!(self.type_hover(&element_type))
            }
            IntermediateType::TernaryType {
                clause,
                consequent,
                alternate,
                span,
            } => {
                within!(span, self);
                // Hover over the condition.
                maybe!(self.type_clause(clause));
                // Hover over the consequent or alternate.
                maybe!(self.type_hover(consequent.as_ref()));
                maybe!(self.type_hover(alternate.as_ref()));
            }
            IntermediateType::BoundConstraintType {
                consequent,
                clause,
                span,
            } => {
                within!(span, self);
                maybe!(self.type_hover(&consequent));
                // Hover over the condition.
                maybe!(self.type_clause(clause));
            }
        }
        return None;
    }

    fn type_clause(&self, condition: &IntermediateTypeClause) -> Option<HoverInfo> {
        match condition {
            IntermediateTypeClause::Binary { left, right, .. } => {
                maybe!(self.type_clause(left));
                self.type_clause(right)
            }
            IntermediateTypeClause::Implements { base, interfaces } => {
                let symbol = self.standpoint.symbol_library.get(*base)?;
                // Hovering over type name.
                let references = symbol
                    .references
                    .iter()
                    .find(|reflist| reflist.module_path == self.module.path_idx)?;
                for span_start in references.starts.iter() {
                    let span = Span::on_line(*span_start, symbol.name.len() as u32);
                    if span.contains(self.pos) {
                        self.message_store
                            .borrow_mut()
                            .inform("Hovering over a type.");
                        return Some(HoverInfo::from((self.standpoint, *base)));
                    }
                }
                for interface in interfaces {
                    maybe!(self.type_hover(interface))
                }
                return None;
            }
        }
    }

    fn fn_parts_hover(
        &self,
        generic_params: &Vec<analyzer::SymbolIndex>,
        params: &Vec<analyzer::SymbolIndex>,
        return_type: &Option<analyzer::IntermediateType>,
        constraints: Option<&IntermediateTypeClause>,
        body: Option<&analyzer::TypedBlock>,
    ) -> Option<HoverInfo> {
        // GENERICS.
        // Hovering over a generic parameter.
        for generic_param in generic_params {
            maybe!(self.generic_parameter(*generic_param))
        }
        // PARAMETERS.
        // Hovering over a parameter.
        for param in params {
            maybe!(self.parameter(*param));
        }
        // CONSTRAINTS
        maybe!(constraints.and_then(|c| self.type_clause(c)));
        // RETURN TYPES.
        // Hovering over return type.
        maybe!(return_type
            .as_ref()
            .and_then(|return_type| self.type_hover(return_type)));
        let body = body?;
        // BODY.
        // Hovering over something in the function's body.
        return self.block(body);
    }
    /// Hovering over a parameter.
    fn parameter(&self, param: SymbolIndex) -> Option<HoverInfo> {
        let symbol = self.standpoint.symbol_library.get(param)?;
        let references = symbol
            .references
            .iter()
            .find(|reflist| reflist.module_path == self.module.path_idx)?;
        for span_start in references.starts.iter() {
            let span = Span::on_line(*span_start, symbol.name.len() as u32);
            if span.contains(self.pos) {
                self.message_store
                    .borrow_mut()
                    .inform("Hovering over parameter.");
                return Some(HoverInfo::from((self.standpoint, param)));
            }
        }
        let type_label = match &symbol.kind {
            SemanticSymbolKind::Parameter { param_type, .. } => param_type,
            _ => return None,
        };
        type_label.as_ref().and_then(|_type| self.type_hover(_type))
    }

    /// Hover over a generic parameter.
    fn generic_parameter(&self, generic_param: SymbolIndex) -> Option<HoverInfo> {
        let symbol = self.standpoint.symbol_library.get(generic_param)?;
        // let references = symbol
        //     .references
        //     .iter()
        //     .find(|reflist| reflist.module_path == self.module.path_idx)?;
        // for span_start in references.starts.iter() {
        //     let span = Span::on_line(*span_start, symbol.name.len() as u32);
        if symbol.ident_span().contains(self.pos) {
            self.message_store
                .borrow_mut()
                .inform("Hovering over generic parameter...");
            return Some(HoverInfo::from((self.standpoint, generic_param)));
            // }
        }
        let (interfaces, default) = match &symbol.kind {
            SemanticSymbolKind::GenericParameter {
                interfaces,
                default_value,
                ..
            } => (interfaces, default_value),
            _ => return None,
        };
        for _interface in interfaces {
            if let Some(hvinfo) = self.type_hover(_interface) {
                return Some(hvinfo);
            }
        }
        if let Some(hvinfo) = default.as_ref().and_then(|_type| self.type_hover(_type)) {
            return Some(hvinfo);
        }
        return None;
    }
}
