#![allow(unused)]
use crate::message_store::MessageStore;
use analyzer::{
    IntermediateType, SemanticSymbolKind, Standpoint, SymbolIndex, TypedCallExpr, TypedIdent,
    TypedModule, TypedThisExpr, TypedTypeDeclaration, TypedVisitorNoArgs,
};
use ast::Span;
use printer::SymbolWriter;
use std::cell::RefCell;
use tower_lsp::lsp_types::{Hover, HoverContents, LanguageString, MarkedString};
// use utils::get_parent_dir;

// /// I can only write it so many times.
// /// Generates a hover over signatures.
// macro_rules! name_hover {
//     ($signature: expr, $scope: expr, $hvfinder: expr) => {{
//         if $signature.name.span.contains($hvfinder.pos) {
//             let mut contents = vec![];
//             let mut string = String::new();
//             if $scope.is_global() && $signature.is_public {
//                 string.push_str("module ");
//                 // connect dots.
//                 let mut pathway = vec![];
//                 $hvfinder.graph.draw_line_to($hvfinder.module, &mut pathway);
//                 for (idx, module_id) in pathway.iter().enumerate() {
//                     string.push_str(
//                         $hvfinder
//                             .graph
//                             .get_module_with_id(*module_id)?
//                             .name
//                             .as_ref()?,
//                     );
//                     if idx + 1 != pathway.len() {
//                         string.push('.');
//                     }
//                 }
//                 string.push('\n');
//             }
//             string.push_str(&$signature.to_formatted());
//             contents.push(MarkedString::LanguageString(LanguageString {
//                 language: String::from("wrl"),
//                 value: string,
//             }));
//             // Documentation?
//             if let Some(ref docs) = $signature.info() {
//                 let mut documentation = String::new();
//                 for line in docs.iter() {
//                     documentation.push_str(line);
//                     documentation.push('\n')
//                 }
//                 contents.push(MarkedString::String(documentation))
//             }
//             return Some(HoverInfo {
//                 contents: HoverContents::Array(contents),
//             });
//         }
//     }};
// }

// /// Generates hover for types.
// macro_rules! type_hover {
//     ($typ: expr, $scope: expr, $self: expr) => {
//         if let Some(hover) = $self.type_hover(&$typ, $scope.id) {
//             return Some(hover);
//         }
//     };
// }

// /// Generate hover for sub-atoms.
// macro_rules! sub_name_hover {
//     ($sgn1: expr, $parent: expr, $scope: expr, $sgn2: expr, $self: expr) => {{
//         // hover over attribute name.
//         if $sgn1.name.span.contains($self.pos) {
//             let hover_over = ThreeTierContext {
//                 signature: $sgn2,
//                 parent: $parent,
//                 module_ambience: &$self.module.ambience,
//             };
//             if $scope.is_global() && $parent.is_public && $sgn1.is_public {
//                 let _global_hover = PublicSignatureContext {
//                     module_ambience: &$self.module.ambience,
//                     signature: &hover_over,
//                 };
//                 return None;
//             }
//             return None;
//         }
//     };};
// }

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

        let symbol = tuple.0.symbol_table.get(tuple.1).unwrap();
        let writer = SymbolWriter::new(tuple.0);
        let string = writer.print_symbol_with_idx(tuple.1);

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
    context: &'a Standpoint,
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
            context,
            module,
            pos,
            message_store: RefCell::new(messages),
        }
    }

    /// Hover over a generic parameter.
    fn generic_parameter(&self, generic_param: SymbolIndex) -> Option<HoverInfo> {
        let symbol = self.context.symbol_table.get(generic_param)?;
        let references = symbol
            .references
            .iter()
            .find(|reflist| reflist.module_path == self.module.path_idx)?;
        for span_start in references.starts.iter() {
            let span = Span::on_line(*span_start, symbol.name.len() as u32);
            if span.contains(self.pos) {
                self.message_store
                    .borrow_mut()
                    .inform("Hovering over generic parameter...");
                return Some(HoverInfo::from((self.context, generic_param)));
            }
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
    //     /// Hover over a use import.
    //     fn use_declaration(&self, use_decl: &ast::UseDeclaration) -> Option<HoverInfo> {
    //         return self.use_target_hover(&use_decl.target);
    //     }
    //     /// Hover over the module declaration.
    //     fn module_declaration(&self, _module: &ast::ModuleDeclaration) -> Option<HoverInfo> {
    //         if _module.span.contains(self.pos) {
    //             return self.create_module_self_hover();
    //         }
    //         return None;
    //     }
    //     /// Hover over a trait declaration.
    //     fn trait_declaraion(&self, trait_decl: &ast::TraitDeclaration) -> Option<HoverInfo> {
    //         let module_ambience = &self.module.ambience;
    //         let scope = module_ambience.get_scope(trait_decl.address.scope_id)?;
    //         let trait_ = scope.get_trait(trait_decl.address.entry_no)?;
    //         // hover over model name.
    //         name_hover!(trait_, scope, self);
    //         // hover over trait impl.
    //         for implentation in &trait_.implementations {
    //             type_hover!(implentation, scope, self);
    //         }
    //         // hover over an attribute, method or implementation.
    //         for property in &trait_decl.body.properties {
    //             if property.span.contains(self.pos) {
    //                 match &property._type {
    //                     TraitPropertyType::Signature => {
    //                         let method = trait_.methods.get(property.index)?;
    //                         // Hovering over a method name.
    //                         sub_name_hover!(method, trait_, scope, method, self);
    //                         let body = Block::empty(scope.id, Span::default());
    //                         // Other hovers.
    //                         return self.fn_parts_hover(
    //                             &method.params,
    //                             &method.return_type,
    //                             &body,
    //                             scope.id,
    //                         );
    //                     }
    //                     TraitPropertyType::Method { body } => {
    //                         let method = trait_.methods.get(property.index)?;
    //                         // Hovering over a method name.
    //                         sub_name_hover!(method, trait_, scope, method, self);
    //                         // Other hovers.
    //                         return self.fn_parts_hover(
    //                             &method.params,
    //                             &method.return_type,
    //                             &body,
    //                             scope.id,
    //                         );
    //                     }
    //                 }
    //             }
    //         }
    //         return None;
    //     }
    /// Hovering over a type declaration.:
    fn type_decl(&self, type_decl: &TypedTypeDeclaration) -> Option<HoverInfo> {
        if !type_decl.span.contains(self.pos) {
            return None;
        }
        let symbol = self.context.symbol_table.get(type_decl.name.symbol_idx)?;
        // Hovering over the type name.
        if symbol.ident_span().contains(self.pos) {
            self.message_store
                .borrow_mut()
                .inform("Hovering over a type name...");
            return Some(HoverInfo::from((self.context, type_decl.name.symbol_idx)));
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
            if let Some(hvinfo) = self.generic_parameter(*generic_param) {
                return Some(hvinfo);
            }
        }
        // Hovering over the type values.
        if let Some(hvinfo) = self.type_hover(value) {
            return Some(hvinfo);
        }
        return None;
    }
    /// Hover over a shorthand variable declaration.
    fn shorthand_var_decl(
        &self,
        var_decl: &analyzer::TypedShorthandVariableDeclaration,
    ) -> Option<HoverInfo> {
        if !var_decl.span.contains(self.pos) {
            return None;
        }
        let symbol = self.context.symbol_table.get(var_decl.name.symbol_idx)?;
        // Hovering over a variable name.
        if symbol.ident_span().contains(self.pos) {
            return Some(HoverInfo::from((self.context, var_decl.name.symbol_idx)));
        }
        // Hovering over variable type.
        let declared_type = match &symbol.kind {
            SemanticSymbolKind::Variable { declared_type, .. } => declared_type,
            _ => return None,
        };
        if let Some(ref _type) = declared_type {
            if let Some(hvinfo) = self.type_hover(_type) {
                return Some(hvinfo);
            }
        }
        // hovering over the variables' value.
        return self.expr(&var_decl.value);
    }
    /// Hovering over a function.
    fn function(&self, function: &analyzer::TypedFunctionDeclaration) -> Option<HoverInfo> {
        if !function.span.contains(self.pos) {
            return None;
        }
        let symbol = self.context.symbol_table.get(function.name.symbol_idx)?;
        // Hovering over the function name.
        if symbol.ident_span().contains(self.pos) {
            self.message_store
                .borrow_mut()
                .inform("Hovering over a function name...");
            return Some(HoverInfo::from((self.context, function.name.symbol_idx)));
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
        return self.fn_parts_hover(generic_params, params, return_type, body);
    }
    /// Hover over an enum declaration.
    fn enum_decl(&self, enum_decl: &analyzer::TypedEnumDeclaration) -> Option<HoverInfo> {
        if !enum_decl.span.contains(self.pos) {
            return None;
        }
        let symbol = self.context.symbol_table.get(enum_decl.name.symbol_idx)?;
        // Hovering over the enum name.
        if symbol.ident_span().contains(self.pos) {
            self.message_store
                .borrow_mut()
                .inform("Hovering over a enum name...");
            return Some(HoverInfo::from((self.context, enum_decl.name.symbol_idx)));
        };

        // Hovering over a generic param.
        let (generic_params) = match &symbol.kind {
            SemanticSymbolKind::Enum { generic_params, .. } => (generic_params),
            _ => return None,
        };
        return None;
    }
    //     /// Hover over a model.
    //     fn model_decl(&self, model_decl: &ast::ModelDeclaration) -> Option<HoverInfo> {
    //         let module_ambience = &self.module.ambience;
    //         let scope = module_ambience.get_scope(model_decl.address.scope_id)?;
    //         let model = scope.get_model(model_decl.address.entry_no)?;
    //         // hover over model name.
    //         name_hover!(model, scope, self);
    //         // hover over trait impl.
    //         for implentation in &model.implementations {
    //             type_hover!(implentation, scope, self);
    //         }
    //         // hover over an attribute, method or implementation.
    //         for property in &model_decl.body.properties {
    //             if property.span.contains(self.pos) {
    //                 match &property._type {
    //                     ModelPropertyType::Attribute => {
    //                         let attribute = model.attributes.get(property.index)?;
    //                         // hover over attribute name.
    //                         sub_name_hover!(attribute, model, scope, attribute, self);
    //                         // hover over attribute type.
    //                         type_hover!(&attribute.var_type, scope, self);
    //                     }
    //                     ModelPropertyType::Method { body } => {
    //                         let method = model.methods.get(property.index)?;
    //                         // Hovering over a method name.
    //                         sub_name_hover!(method, model, scope, method, self);
    //                         // Other hovers.
    //                         return self.fn_parts_hover(
    //                             &method.params,
    //                             &method.return_type,
    //                             &body,
    //                             scope.id,
    //                         );
    //                     }
    //                     ModelPropertyType::TraitImpl { body, .. } => {
    //                         let method = model.methods.get(property.index)?;
    //                         sub_name_hover!(method, model, scope, method, self);

    //                         // Hover over name.
    //                         // Other hovers
    //                         return self.fn_parts_hover(
    //                             &method.params,
    //                             &method.return_type,
    //                             &body,
    //                             scope.id,
    //                         );
    //                     }
    //                 }
    //             }
    //         }
    //         return None;
    //     }
    /// Hovering over a call expression.
    fn call_expr(&self, call: &TypedCallExpr) -> Option<HoverInfo> {
        // Hovering over the caller.
        if let Some(hvinfo) = self.expr(&call.caller) {
            return Some(hvinfo);
        }
        // Hovering over an argument.
        for argument in call.arguments.iter() {
            if let Some(hvinfo) = self.expr(argument) {
                return Some(hvinfo);
            }
        }
        return None;
    }
    /// Hovering over `this`.
    fn this_expr(&self, _this: &TypedThisExpr) -> Option<HoverInfo> {
        let span_start = [_this.start_line, _this.start_character];
        let span = Span::on_line(span_start, 4);
        if !span.contains(self.pos) {
            return None;
        }
        match _this.model_or_trait {
            Some(meaning) => Some(HoverInfo::from((self.context, meaning))),
            None => Some(HoverInfo::from_str("this: {{unknown}}")),
        }
    }
    /// Hovering over an identifier.
    fn identifier(&self, ident: &TypedIdent) -> Option<HoverInfo> {
        let symbol_idx = ident.value.symbol_idx;
        let symbol = self.context.symbol_table.get(symbol_idx)?;
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
                return Some(HoverInfo::from((self.context, symbol_idx)));
            }
        }
        return None;
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
            } => todo!(),
            IntermediateType::SimpleType {
                value,
                generic_args,
                span,
            } => {
                if !span.contains(self.pos) {
                    return None;
                }
                let symbol = self.context.symbol_table.get(*value)?;
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
                        return Some(HoverInfo::from((self.context, *value)));
                    }
                }
                // Hovering over a discrete type generic argument.
                for generic_arg in generic_args {
                    if let Some(hvinfo) = self.type_hover(generic_arg) {
                        return Some(hvinfo);
                    }
                }
            }
            IntermediateType::UnionType { types, span } => {
                if !span.contains(self.pos) {
                    return None;
                }
                for itmd_type in types {
                    if let Some(hvinfo) = self.type_hover(itmd_type) {
                        return Some(hvinfo);
                    }
                }
            }
            IntermediateType::This { meaning, span } => {
                if !span.contains(self.pos) {
                    return None;
                }
                self.message_store
                    .borrow_mut()
                    .inform("Hovering over This.");
                if let Some(meaning) = meaning {
                    return Some(HoverInfo::from((self.context, *meaning)));
                } else {
                    return Some(HoverInfo::from_str("This : {{unknown}}"));
                }
            }
            IntermediateType::BorrowedType { value, span } => {
                if !span.contains(self.pos) {
                    return None;
                }
                if let Some(hvinfo) = self.type_hover(value) {
                    return Some(hvinfo);
                }
            }
            IntermediateType::Placeholder => {
                unreachable!("Attempted a hover over a placeholder intermediate type. How???")
            }
            IntermediateType::MemberType {
                object,
                property,
                span,
            } => {
                if !span.contains(self.pos) {
                    return None;
                }
                // Hovering over the namespace.
                if let Some(hvinfo) = self.type_hover(&object) {
                    return Some(hvinfo);
                }
                // Hovering over the property.
                if let Some(hvinfo) = self.type_hover(&property) {
                    return Some(hvinfo);
                }
            }
        }
        return None;
    }

    fn fn_parts_hover(
        &self,
        generic_params: &Vec<analyzer::SymbolIndex>,
        params: &Vec<analyzer::SymbolIndex>,
        return_type: &Option<analyzer::IntermediateType>,
        body: &analyzer::TypedBlock,
    ) -> Option<HoverInfo> {
        // GENERICS.
        // Hovering over a generic parameter.
        for generic_param in generic_params {
            // Hovering over a generic parameter name.
            let symbol = self.context.symbol_table.get(*generic_param)?;
            let references = symbol
                .references
                .iter()
                .find(|reflist| reflist.module_path == self.module.path_idx)?;
            for span_start in references.starts.iter() {
                let span = Span::on_line(*span_start, symbol.name.len() as u32);
                if span.contains(self.pos) {
                    self.message_store
                        .borrow_mut()
                        .inform("Hovering over generic parameter...");
                    return Some(HoverInfo::from((self.context, *generic_param)));
                }
            }
            let (traits, default) = match &symbol.kind {
                SemanticSymbolKind::GenericParameter {
                    traits,
                    default_value,
                    ..
                } => (traits, default_value),
                _ => return None,
            };
            // Hovering over a generic parameter trait.
            for _trait in traits {
                if let Some(hvinfo) = self.type_hover(_trait) {
                    return Some(hvinfo);
                }
            }
            // Hovering over the default value.
            if let Some(hvinfo) = default.as_ref().and_then(|_type| self.type_hover(_type)) {
                return Some(hvinfo);
            }
        }
        // PARAMETERS.
        // Hovering over a parameter.
        for param in params {
            // Hovering over a parameter name.
            let symbol = self.context.symbol_table.get(*param)?;
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
                    return Some(HoverInfo::from((self.context, *param)));
                }
            }
            let type_label = match &symbol.kind {
                SemanticSymbolKind::Parameter { param_type, .. } => param_type,
                _ => return None,
            };
            // Hovering over a parameter type label.
            if let Some(hvinfo) = type_label.as_ref().and_then(|_type| self.type_hover(_type)) {
                return Some(hvinfo);
            }
        }
        // RETURN TYPES.
        // Hovering over return type.
        if let Some(return_type) = return_type {
            if let Some(hvinfo) = self.type_hover(return_type) {
                return Some(hvinfo);
            }
        }
        // BODY.
        // Hovering over something in the function's body.
        if !body.span.contains(self.pos) {
            return None;
        }
        for statement in &body.statements {
            let hover_info = self.statement(statement);
            if hover_info.is_some() {
                return hover_info;
            }
        }
        return None;
    }

    //     /// Create a hover with its position set over an identifier.
    //     fn create_mock_hover_over(&self, ident: &Identifier, current_scope: usize) -> HoverFinder {
    //         let span = ident.span;
    //         let pos = span.start;
    //         HoverFinder {
    //             module: self.module,
    //             context: self.context,
    //             pos,
    //             current_scope: RefCell::new(current_scope),
    //         }
    //     }
}
