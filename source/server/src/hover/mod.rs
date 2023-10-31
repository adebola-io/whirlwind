#![allow(unused)]
use crate::message_store::MessageStore;
use analyzer::{
    IntermediateType, SemanticSymbol, SemanticSymbolKind, Standpoint, SymbolIndex, TypedCallExpr,
    TypedIdent, TypedModule, TypedThisExpr, TypedTypeDeclaration, TypedVisitorNoArgs,
};
use ast::Span;
use printer::SymbolWriter;
use std::cell::RefCell;
use tower_lsp::lsp_types::{Hover, HoverContents, LanguageString, MarkedString};
// use utils::get_parent_dir;

/// Exits a function and returns an option value if it is Some.
/// Basically the direct opposite of `impl Try for Option`.
macro_rules! maybe {
    ($exp: expr) => {
        if let Some(exp) = $exp {
            return Some(exp);
        }
    };
}

/// Exits a visition function if the hover position is not contained within a span.
macro_rules! within {
    ($span: expr, $self: expr) => {
        if !$span.contains($self.pos) {
            return None;
        }
    };
}

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
        let symbol_table = &tuple.0.symbol_table;
        let symbol = symbol_table.get(tuple.1).unwrap();
        let writer = SymbolWriter::new(tuple.0);
        let string = writer.print_symbol_with_idx(tuple.1);

        // Import and property redirection.
        let symbol = match &symbol.kind {
            SemanticSymbolKind::Import {
                source: Some(source),
                ..
            } => {
                let mut origin = source;
                let mut parent = symbol_table.get(*source);
                while let Some(SemanticSymbol {
                    kind:
                        SemanticSymbolKind::Import {
                            source: Some(source),
                            ..
                        },
                    ..
                }) = parent
                {
                    origin = source;
                    parent = symbol_table.get(*source);
                }
                parent.unwrap()
            }
            SemanticSymbolKind::Property {
                resolved: Some(origin),
            } => tuple.0.symbol_table.get(*origin).unwrap(),
            _ => symbol,
        };

        contents.push(MarkedString::LanguageString(LanguageString {
            language: String::from("wrl"),
            value: string,
        }));
        // Documentation?
        if let Some(ref docs) = symbol.doc_info {
            let mut documentation = String::new();
            for line in docs.iter() {
                documentation.push_str(line);
                documentation.push('\n')
            }
            contents.push(MarkedString::String(documentation))
        }
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

    // /// Create a hover over this module itself.
    // fn create_module_self_hover(&self) -> Option<HoverInfo> {
    //     let mut contents = vec![];
    //     let mut string = String::new();
    //     string.push_str("module ");
    //     // connect dots.
    //     let mut pathway = vec![];
    //     self.context.draw_line_to(self.module, &mut pathway);
    //     for (idx, module_id) in pathway.iter().enumerate() {
    //         string.push_str(self.context.get_module_with_id(*module_id)?.name.as_ref()?);
    //         if idx + 1 != pathway.len() {
    //             string.push('.');
    //         }
    //     }
    //     contents.push(MarkedString::LanguageString(LanguageString {
    //         language: String::from("wrl"),
    //         value: string,
    //     }));
    //     // Documentation?
    //     if let Some(ref docs) = self.module.ambience.info() {
    //         let mut documentation = String::new();
    //         for line in docs.iter() {
    //             documentation.push_str(line);
    //             documentation.push('\n')
    //         }
    //         contents.push(MarkedString::String(documentation))
    //     }
    //     return Some(HoverInfo {
    //         contents: HoverContents::Array(contents),
    //     });
    // }

    // fn use_target_hover(&self, target: &ast::UseTarget) -> Option<HoverInfo> {
    //     let parent_folder = get_parent_dir(self.module.module_path.as_ref()?)?;
    //     let target_module = self
    //         .context
    //         .get_module_in_dir(parent_folder, &target.name.name)?;
    //     // hover over target name.
    //     if target.name.span.contains(self.pos) {
    //         let hvfinder = HoverFinder::new(target_module, self.context, self.pos);
    //         return hvfinder.create_module_self_hover();
    //     }
    //     // hover over any other.
    //     match &target.path {
    //         // same as hover over target name.
    //         ast::UsePath::Me => return None, // technically unreachable.
    //         ast::UsePath::Item(sub_target) => {
    //             return self.use_sub_target_hover(target_module, sub_target)
    //         }
    //         ast::UsePath::List(list) => {
    //             for sub_target in list.iter() {
    //                 if let Some(hover) = self.use_sub_target_hover(target_module, sub_target) {
    //                     return Some(hover);
    //                 }
    //             }
    //         }
    //     }

    //     return None;
    // }

    // /// Hovering over an atom of the target.
    // fn use_sub_target_hover(
    //     &self,
    //     target_module: &Module,
    //     sub_target: &ast::UseTarget,
    // ) -> Option<HoverInfo> {
    // Create mock hover over the contents of the actual declaration.
    //     let ambience = target_module.ambience.create_shadow(0);
    //     let target_decl = ambience.lookaround(&sub_target.name.name)?;
    //     let target_decl_ident = target_decl.entry.ident()?;
    //     let span = target_decl_ident.span;
    //     let original_position = [span.start[0], span.start[1]];
    //     let hover_finder = HoverFinder::new(target_module, self.context, original_position);
    //     let scope = target_decl.scope;

    //     // hover over name.
    //     if sub_target.name.span.contains(self.pos) {
    //         match target_decl.entry {
    //             ast::ScopeEntry::Function(f) => name_hover!(f, scope, hover_finder),
    //             ast::ScopeEntry::Type(t) => name_hover!(t, scope, hover_finder),
    //             ast::ScopeEntry::Model(m) => name_hover!(m, scope, hover_finder),
    //             ast::ScopeEntry::Enum(e) => name_hover!(e, scope, hover_finder),
    //             ast::ScopeEntry::ShorthandVariable(v) => {
    //                 return Some(HoverInfo::from(&(&target_module.ambience, v)))
    //             }
    //             ast::ScopeEntry::Trait(t) => name_hover!(t, scope, hover_finder),
    //             ast::ScopeEntry::Parameter(_) | ast::ScopeEntry::ReservedSpace => return None,
    //             ast::ScopeEntry::UseImport(u) => {
    //                 let declaration = target_module
    //                     .statements()
    //                     .map(|statement| statement.closest_nodes_to(u.name.span))
    //                     .flatten()
    //                     .next()?;
    //                 return hover_finder.statement(declaration);
    //             }
    //             ast::ScopeEntry::Constant(c) => name_hover!(c, scope, hover_finder),
    //             ast::ScopeEntry::Variable(_) => {}
    //             _ => {} // technically unreachable
    //         }
    //     }

    //     let parent_folder = get_parent_dir(target_module.module_path.as_ref()?)?;
    //     let sub_target_module = self
    //         .context
    //         .get_module_in_dir(parent_folder, &sub_target.name.name)?;

    //     // hover over any other.
    //     match &sub_target.path {
    //         // same as hover over target name.
    //         ast::UsePath::Me => return None, // technically unreachable.
    //         ast::UsePath::Item(sub_sub_target) => {
    //             return self.use_sub_target_hover(sub_target_module, sub_sub_target);
    //         }
    //         ast::UsePath::List(sub_list) => {
    //             for sub_sub_target in sub_list.iter() {
    //                 if let Some(hover) =
    //                     self.use_sub_target_hover(sub_target_module, sub_sub_target)
    //                 {
    //                     return Some(hover);
    //                 }
    //             }
    //         }
    //     }

    //     return None;
    // }
}

impl<'a> TypedVisitorNoArgs<Option<HoverInfo>> for HoverFinder<'a> {
    /// Hover over a use import.
    fn use_declaration(&self, use_decl: &analyzer::TypedUseDeclaration) -> Option<HoverInfo> {
        within!(use_decl.span, self);
        for import in &use_decl.imports {
            let symbol = self.standpoint.symbol_table.get(*import)?;
            if symbol.ident_span().contains(self.pos) {
                return Some(HoverInfo::from((self.standpoint, *import)));
            }
        }
        None
    }
    /// Hovering over a variable declaration.
    fn var_decl(&self, var_decl: &analyzer::TypedVariableDeclaration) -> Option<HoverInfo> {
        within!(var_decl.span, self);
        let mut type_already_checked = false;
        for name in &var_decl.names {
            let symbol = self.standpoint.symbol_table.get(*name)?;
            // Hovering over variable name.
            if symbol.ident_span().contains(self.pos) {
                self.message_store
                    .borrow_mut()
                    .inform("Hovering over variable name...");
                return Some(HoverInfo::from((self.standpoint, *name)));
            };
            if !type_already_checked {
                let typ = match &symbol.kind {
                    SemanticSymbolKind::Variable {
                        declared_type,
                        inferred_type,
                        ..
                    } => declared_type,
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
    fn type_decl(&self, type_decl: &TypedTypeDeclaration) -> Option<HoverInfo> {
        within!(type_decl.span, self);
        let symbol = self.standpoint.symbol_table.get(type_decl.name)?;
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
        let symbol = self.standpoint.symbol_table.get(var_decl.name)?;
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
        let symbol = self.standpoint.symbol_table.get(function.name)?;
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
        return self.fn_parts_hover(generic_params, params, return_type, Some(body));
    }
    /// Hover over an enum declaration.
    fn enum_decl(&self, enum_decl: &analyzer::TypedEnumDeclaration) -> Option<HoverInfo> {
        within!(enum_decl.span, self);
        let symbol = self.standpoint.symbol_table.get(enum_decl.name)?;
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
            let symbol = self.standpoint.symbol_table.get(*variant)?;
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
        match _this.model_or_trait {
            Some(meaning) => Some(HoverInfo::from((self.standpoint, meaning))),
            None => Some(HoverInfo::from_str("this: {nknown}")),
        }
    }
    /// Hovering over an identifier.
    fn identifier(&self, ident: &TypedIdent) -> Option<HoverInfo> {
        let symbol_idx = ident.value;
        let symbol = self.standpoint.symbol_table.get(symbol_idx)?;
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
            None
        ));
        self.expr(&function_expr.body)
    }

    fn literal(&self, literal: &analyzer::LiteralIndex) -> Option<HoverInfo> {
        None
    }

    fn new_expr(&self, new_exp: &analyzer::TypedNewExpr) -> Option<HoverInfo> {
        within!(new_exp.span, self);
        self.expr(&new_exp.value)
    }

    fn constant(&self, constant: &analyzer::TypedConstantDeclaration) -> Option<HoverInfo> {
        within!(constant.span, self);
        let symbol = self.standpoint.symbol_table.get(constant.name)?;
        // Hovering over a variable name.
        if symbol.ident_span().contains(self.pos) {
            return Some(HoverInfo::from((self.standpoint, constant.name)));
        }
        // Hovering over variable type.
        let declared_type = match &symbol.kind {
            SemanticSymbolKind::Constant {
                is_public,
                declared_type,
                ..
            } => declared_type,
            _ => return None,
        };
        maybe!(self.type_hover(declared_type));
        // hovering over the variables' value.
        self.expr(&constant.value)
    }

    fn model_decl(&self, model: &analyzer::TypedModelDeclaration) -> Option<HoverInfo> {
        within!(model.span, self);
        // hovering over a model name.
        let model_symbol = self.standpoint.symbol_table.get(model.name)?;
        if model_symbol.ident_span().contains(self.pos) {
            return Some(HoverInfo::from((self.standpoint, model.name)));
        }
        // hovering over a generic parameter.
        let (generic_params, impls) = match &model_symbol.kind {
            SemanticSymbolKind::Model {
                generic_params,
                implementations,
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
        // hovering over the model body.
        within!(model.body.span, self);
        // hovering inside the model constructor.
        maybe!(model.body.constructor.as_ref().and_then(|constructor| {
            constructor
                .parameters
                .iter()
                .find_map(|param| self.parameter(*param))
                .or_else(|| self.block(&constructor.block))
        }));
        // hovering in a property.
        for property in model.body.properties.iter() {
            if !property.span.contains(self.pos) {
                continue;
            }
            let symbol = self.standpoint.symbol_table.get(property.name)?;
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
                        ..
                    } => {
                        return self.fn_parts_hover(generic_params, params, return_type, Some(body))
                    }
                    _ => return None,
                },
                analyzer::TypedModelPropertyType::TraitImpl { trait_target, body } => {
                    for trait_target_unit in trait_target {
                        maybe!(self.type_hover(trait_target_unit))
                    }
                    match &symbol.kind {
                        SemanticSymbolKind::Method {
                            params,
                            generic_params,
                            return_type,
                            ..
                        } => {
                            return self.fn_parts_hover(
                                generic_params,
                                params,
                                return_type,
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

    fn trait_declaration(&self, trait_: &analyzer::TypedTraitDeclaration) -> Option<HoverInfo> {
        within!(trait_.span, self);
        // hovering over a trait name.
        let model_symbol = self.standpoint.symbol_table.get(trait_.name)?;
        if model_symbol.ident_span().contains(self.pos) {
            return Some(HoverInfo::from((self.standpoint, trait_.name)));
        }
        // hovering over a generic parameter.
        let (generic_params, impls) = match &model_symbol.kind {
            SemanticSymbolKind::Trait {
                generic_params,
                implementations,
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
        // hovering over the trait body.
        within!(trait_.body.span, self);
        // hovering in a property.
        for property in trait_.body.properties.iter() {
            if !property.span.contains(self.pos) {
                continue;
            }
            let symbol = self.standpoint.symbol_table.get(property.name)?;
            // Hovering over the property name.
            if symbol.ident_span().contains(self.pos) {
                return Some(HoverInfo::from((self.standpoint, property.name)));
            }
            match &property._type {
                analyzer::TypedTraitPropertyType::Method { body } => match &symbol.kind {
                    SemanticSymbolKind::Method {
                        params,
                        generic_params,
                        return_type,
                        ..
                    } => {
                        return self.fn_parts_hover(generic_params, params, return_type, Some(body))
                    }
                    _ => return None,
                },
                analyzer::TypedTraitPropertyType::Signature => match &symbol.kind {
                    SemanticSymbolKind::Method {
                        params,
                        generic_params,
                        return_type,
                        ..
                    } => return self.fn_parts_hover(generic_params, params, return_type, None),
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
        for i in &forstat.items {
            // todo:
        }
        // hovering over label.
        maybe!(forstat
            .label
            .as_ref()
            .and_then(|ident| self.identifier(ident)));
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
                let symbol = self.standpoint.symbol_table.get(*value)?;
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
            IntermediateType::BorrowedType { value, span } => {
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
                maybe!(self.type_hover(&property));
            }
        }
        return None;
    }

    fn fn_parts_hover(
        &self,
        generic_params: &Vec<analyzer::SymbolIndex>,
        params: &Vec<analyzer::SymbolIndex>,
        return_type: &Option<analyzer::IntermediateType>,
        body: Option<&analyzer::TypedBlock>,
    ) -> Option<HoverInfo> {
        let body = body?;
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
        // RETURN TYPES.
        // Hovering over return type.
        maybe!(return_type
            .as_ref()
            .and_then(|return_type| self.type_hover(return_type)));
        // BODY.
        // Hovering over something in the function's body.
        return self.block(body);
    }
    /// Hovering over a parameter.
    fn parameter(&self, param: SymbolIndex) -> Option<HoverInfo> {
        let symbol = self.standpoint.symbol_table.get(param)?;
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
        let symbol = self.standpoint.symbol_table.get(generic_param)?;
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
        let (traits, default) = match &symbol.kind {
            SemanticSymbolKind::GenericParameter {
                traits,
                default_value,
                ..
            } => (traits, default_value),
            _ => return None,
        };
        for _trait in traits {
            if let Some(hvinfo) = self.type_hover(_trait) {
                return Some(hvinfo);
            }
        }
        if let Some(hvinfo) = default.as_ref().and_then(|_type| self.type_hover(_type)) {
            return Some(hvinfo);
        }
        return None;
    }
}
