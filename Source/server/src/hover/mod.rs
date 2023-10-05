use std::cell::RefCell;

use tower_lsp::lsp_types::{Hover, HoverContents, LanguageString, MarkedString};

use analyzer::{Module, ModuleGraph};
use ast::{
    ASTVisitorNoArgs, Block, Identifier, ModelPropertyType, Parameter, Positioning,
    PublicSignatureContext, Signature, Span, Spannable, ThreeTierContext, TraitPropertyType,
    TypeExpression,
};
use printer::HoverFormatter;
use utils::get_parent_dir;

/// I can only write it so many times.
/// Generates a hover over signatures.
macro_rules! name_hover {
    ($signature: expr, $scope: expr, $hvfinder: expr) => {{
        if $signature.name.span.contains($hvfinder.pos) {
            let mut contents = vec![];
            let mut string = String::new();
            if $scope.is_global() && $signature.is_public {
                string.push_str("module ");
                // connect dots.
                let mut pathway = vec![];
                $hvfinder.graph.draw_line_to($hvfinder.module, &mut pathway);
                for (idx, module_id) in pathway.iter().enumerate() {
                    string.push_str(
                        $hvfinder
                            .graph
                            .get_module_with_id(*module_id)?
                            .name
                            .as_ref()?,
                    );
                    if idx + 1 != pathway.len() {
                        string.push('.');
                    }
                }
                string.push('\n');
            }
            string.push_str(&$signature.to_formatted());
            contents.push(MarkedString::LanguageString(LanguageString {
                language: String::from("wrl"),
                value: string,
            }));
            // Documentation?
            if let Some(ref docs) = $signature.info() {
                let mut documentation = String::new();
                for line in docs.iter() {
                    documentation.push_str(line);
                    documentation.push('\n')
                }
                contents.push(MarkedString::String(documentation))
            }
            return Some(HoverInfo {
                contents: HoverContents::Array(contents),
            });
        }
    }};
}

/// Generates hover for types.
macro_rules! type_hover {
    ($typ: expr, $scope: expr, $self: expr) => {
        if let Some(hover) = $self.type_hover(&$typ, $scope.id) {
            return Some(hover);
        }
    };
}

/// Generate hover for sub-atoms.
macro_rules! sub_name_hover {
    ($sgn1: expr, $parent: expr, $scope: expr, $sgn2: expr, $self: expr) => {{
        // hover over attribute name.
        if $sgn1.name.span.contains($self.pos) {
            let hover_over = ThreeTierContext {
                signature: $sgn2,
                parent: $parent,
                module_ambience: &$self.module.ambience,
            };
            if $scope.is_global() && $parent.is_public && $sgn1.is_public {
                let _global_hover = PublicSignatureContext {
                    module_ambience: &$self.module.ambience,
                    signature: &hover_over,
                };
                return None;
            }
            return None;
        }
    };};
}

/// Information shown during hovering.
pub struct HoverInfo {
    pub contents: HoverContents,
}

// impl HoverInfo {
//     fn from_str(value: &str) -> HoverInfo {
//         HoverInfo {
//             contents: HoverContents::Array(vec![MarkedString::LanguageString(LanguageString {
//                 language: format!("wrl"),
//                 value: value.to_owned(),
//             })]),
//         }
//     }
// }

impl<T: Signature + HoverFormatter> From<&T> for HoverInfo {
    fn from(value: &T) -> Self {
        let mut info = vec![];
        let string = value.to_formatted();
        info.push(MarkedString::LanguageString(LanguageString {
            language: String::from("wrl"),
            value: string,
        }));

        // Documentation?
        if let Some(ref docs) = value.info() {
            let mut documentation = String::new();
            for line in docs.iter() {
                documentation.push_str(line);
                documentation.push('\n')
            }
            info.push(MarkedString::String(documentation))
        }

        HoverInfo {
            contents: HoverContents::Array(info),
        }
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
    module: &'a Module,
    graph: &'a ModuleGraph,
    pos: [u32; 2],
    current_scope: RefCell<usize>,
}

impl<'a> HoverFinder<'a> {
    pub fn new(module: &'a Module, graph: &'a ModuleGraph, pos: [u32; 2]) -> Self {
        Self {
            graph,
            module,
            pos,
            current_scope: RefCell::new(0),
        }
    }

    /// Create a hover over this module itself.
    fn create_module_self_hover(&self) -> Option<HoverInfo> {
        let mut contents = vec![];
        let mut string = String::new();
        string.push_str("module ");
        // connect dots.
        let mut pathway = vec![];
        self.graph.draw_line_to(self.module, &mut pathway);
        for (idx, module_id) in pathway.iter().enumerate() {
            string.push_str(self.graph.get_module_with_id(*module_id)?.name.as_ref()?);
            if idx + 1 != pathway.len() {
                string.push('.');
            }
        }
        contents.push(MarkedString::LanguageString(LanguageString {
            language: String::from("wrl"),
            value: string,
        }));
        // Documentation?
        if let Some(ref docs) = self.module.ambience.info() {
            let mut documentation = String::new();
            for line in docs.iter() {
                documentation.push_str(line);
                documentation.push('\n')
            }
            contents.push(MarkedString::String(documentation))
        }
        return Some(HoverInfo {
            contents: HoverContents::Array(contents),
        });
    }

    fn use_target_hover(&self, target: &ast::UseTarget) -> Option<HoverInfo> {
        let parent_folder = get_parent_dir(self.module.module_path.as_ref()?)?;
        let target_module = self
            .graph
            .get_module_in_dir(parent_folder, &target.name.name)?;
        // hover over target name.
        if target.name.span.contains(self.pos) {
            let hvfinder = HoverFinder::new(target_module, self.graph, self.pos);
            return hvfinder.create_module_self_hover();
        }
        // hover over any other.
        match &target.path {
            // same as hover over target name.
            ast::UsePath::Me => return None, // technically unreachable.
            ast::UsePath::Item(sub_target) => {
                return self.use_sub_target_hover(target_module, sub_target)
            }
            ast::UsePath::List(list) => {
                for sub_target in list.iter() {
                    if let Some(hover) = self.use_sub_target_hover(target_module, sub_target) {
                        return Some(hover);
                    }
                }
            }
        }

        return None;
    }

    /// Hovering over an atom of the target.
    fn use_sub_target_hover(
        &self,
        target_module: &Module,
        sub_target: &ast::UseTarget,
    ) -> Option<HoverInfo> {
        // Create mock hover over the contents of the actual declaration.
        let ambience = target_module.ambience.create_shadow(0);
        let target_decl = ambience.lookaround(&sub_target.name.name)?;
        let target_decl_ident = target_decl.entry.ident()?;
        let span = target_decl_ident.span;
        let original_position = [span.start[0], span.start[1]];
        let hover_finder = HoverFinder::new(target_module, self.graph, original_position);
        let scope = target_decl.scope;

        // hover over name.
        if sub_target.name.span.contains(self.pos) {
            match target_decl.entry {
                ast::ScopeEntry::Function(f) => name_hover!(f, scope, hover_finder),
                ast::ScopeEntry::Type(t) => name_hover!(t, scope, hover_finder),
                ast::ScopeEntry::Model(m) => name_hover!(m, scope, hover_finder),
                ast::ScopeEntry::Enum(e) => name_hover!(e, scope, hover_finder),
                ast::ScopeEntry::ShorthandVariable(v) => {
                    return Some(HoverInfo::from(&(&target_module.ambience, v)))
                }
                ast::ScopeEntry::Trait(t) => name_hover!(t, scope, hover_finder),
                ast::ScopeEntry::Parameter(_) | ast::ScopeEntry::ReservedSpace => return None,
                ast::ScopeEntry::UseImport(u) => {
                    let declaration = target_module
                        .statements()
                        .map(|statement| statement.closest_nodes_to(u.name.span))
                        .flatten()
                        .next()?;
                    return hover_finder.statement(declaration);
                }
                ast::ScopeEntry::Constant(c) => name_hover!(c, scope, hover_finder),
                ast::ScopeEntry::Variable(_) => todo!(),
                // technically unreachable
            }
        }

        let parent_folder = get_parent_dir(target_module.module_path.as_ref()?)?;
        let sub_target_module = self
            .graph
            .get_module_in_dir(parent_folder, &sub_target.name.name)?;

        // hover over any other.
        match &sub_target.path {
            // same as hover over target name.
            ast::UsePath::Me => return None, // technically unreachable.
            ast::UsePath::Item(sub_sub_target) => {
                return self.use_sub_target_hover(sub_target_module, sub_sub_target);
            }
            ast::UsePath::List(sub_list) => {
                for sub_sub_target in sub_list.iter() {
                    if let Some(hover) =
                        self.use_sub_target_hover(sub_target_module, sub_sub_target)
                    {
                        return Some(hover);
                    }
                }
            }
        }

        return None;
    }
}

impl<'a> ASTVisitorNoArgs<Option<HoverInfo>> for HoverFinder<'a> {
    /// Hover over a use import.
    fn use_declaration(&self, use_decl: &ast::UseDeclaration) -> Option<HoverInfo> {
        return self.use_target_hover(&use_decl.target);
    }
    /// Hover over the module declaration.
    fn module_declaration(&self, _module: &ast::ModuleDeclaration) -> Option<HoverInfo> {
        if _module.span.contains(self.pos) {
            return self.create_module_self_hover();
        }
        return None;
    }
    /// Hover over a trait declaration.
    fn trait_declaraion(&self, trait_decl: &ast::TraitDeclaration) -> Option<HoverInfo> {
        let module_ambience = &self.module.ambience;
        let scope = module_ambience.get_scope(trait_decl.address.scope_id)?;
        let trait_ = scope.get_trait(trait_decl.address.entry_no)?;
        // hover over model name.
        name_hover!(trait_, scope, self);
        // hover over trait impl.
        for implentation in &trait_.implementations {
            type_hover!(implentation, scope, self);
        }
        // hover over an attribute, method or implementation.
        for property in &trait_decl.body.properties {
            if property.span.contains(self.pos) {
                match &property._type {
                    TraitPropertyType::Signature => {
                        let method = trait_.methods.get(property.index)?;
                        // Hovering over a method name.
                        sub_name_hover!(method, trait_, scope, method, self);
                        let body = Block::empty(scope.id, Span::default());
                        // Other hovers.
                        return self.fn_parts_hover(
                            &method.params,
                            &method.return_type,
                            &body,
                            scope.id,
                        );
                    }
                    TraitPropertyType::Method { body } => {
                        let method = trait_.methods.get(property.index)?;
                        // Hovering over a method name.
                        sub_name_hover!(method, trait_, scope, method, self);
                        // Other hovers.
                        return self.fn_parts_hover(
                            &method.params,
                            &method.return_type,
                            &body,
                            scope.id,
                        );
                    }
                }
            }
        }
        return None;
    }
    /// Hover over a type declaration.:
    fn type_decl(&self, type_decl: &ast::TypeDeclaration) -> Option<HoverInfo> {
        let scope = self.module.ambience.get_scope(type_decl.address.scope_id)?;
        let signature = scope.get_type(type_decl.address.entry_no)?;
        // hover over type name.
        name_hover!(signature, scope, self);
        // hover over type value.
        return self.type_hover(&signature.value, scope.id);
    }
    /// Hover over a shorthand variable declaration.
    fn shorthand_var_decl(
        &self,
        var_decl: &ast::ShorthandVariableDeclaration,
    ) -> Option<HoverInfo> {
        let scope = self.module.ambience.get_scope(var_decl.address.scope_id)?;
        let signature = scope.get_shorthand_variable(var_decl.address.entry_no)?;
        // hover over variable name.
        if signature.name.span.contains(self.pos) {
            return Some(HoverInfo::from(&(&self.module.ambience, signature)));
        }
        // hover over type.
        if let Some(ref _type) = signature.var_type {
            type_hover!(_type, scope, self);
        }
        // hover over value.
        return self.expr(&var_decl.value);
    }
    /// Hover over a function.
    fn function(&self, function: &ast::FunctionDeclaration) -> Option<HoverInfo> {
        let scope = self.module.ambience.get_scope(function.address.scope_id)?;
        let signature = scope.get_function(function.address.entry_no)?;
        let body = &function.body;
        // hover over function name.
        name_hover!(signature, scope, self);
        // hover over function parts.
        return self.fn_parts_hover(&signature.params, &signature.return_type, body, scope.id);
    }
    /// Hover over an enum declaration.
    fn enum_decl(&self, enum_decl: &ast::EnumDeclaration) -> Option<HoverInfo> {
        let scope = self.module.ambience.get_scope(enum_decl.address.scope_id)?;
        let signature = scope.get_enum(enum_decl.address.entry_no)?;
        // hover over enum name.
        name_hover!(signature, scope, self);
        // hover over variant.
        for variant in &signature.variants {
            if variant.span.contains(self.pos) {
                if scope.is_global() && signature.is_public {
                    let global_hover = PublicSignatureContext {
                        module_ambience: &self.module.ambience,
                        signature: &(&signature.name, variant),
                    };
                    return Some((&global_hover).into());
                }
                return Some(HoverInfo::from(&(&signature.name, variant)));
            }
        }
        return None;
    }
    /// Hover over a model.
    fn model_decl(&self, model_decl: &ast::ModelDeclaration) -> Option<HoverInfo> {
        let module_ambience = &self.module.ambience;
        let scope = module_ambience.get_scope(model_decl.address.scope_id)?;
        let model = scope.get_model(model_decl.address.entry_no)?;
        // hover over model name.
        name_hover!(model, scope, self);
        // hover over trait impl.
        for implentation in &model.implementations {
            type_hover!(implentation, scope, self);
        }
        // hover over an attribute, method or implementation.
        for property in &model_decl.body.properties {
            if property.span.contains(self.pos) {
                match &property._type {
                    ModelPropertyType::Attribute => {
                        let attribute = model.attributes.get(property.index)?;
                        // hover over attribute name.
                        sub_name_hover!(attribute, model, scope, attribute, self);
                        // hover over attribute type.
                        type_hover!(&attribute.var_type, scope, self);
                    }
                    ModelPropertyType::Method { body } => {
                        let method = model.methods.get(property.index)?;
                        // Hovering over a method name.
                        sub_name_hover!(method, model, scope, method, self);
                        // Other hovers.
                        return self.fn_parts_hover(
                            &method.params,
                            &method.return_type,
                            &body,
                            scope.id,
                        );
                    }
                    ModelPropertyType::TraitImpl { body, .. } => {
                        let method = model.methods.get(property.index)?;
                        sub_name_hover!(method, model, scope, method, self);

                        // Hover over name.
                        // Other hovers
                        return self.fn_parts_hover(
                            &method.params,
                            &method.return_type,
                            &body,
                            scope.id,
                        );
                    }
                }
            }
        }
        return None;
    }
    /// Hover over a call expression.
    fn call_expr(&self, call: &ast::CallExpr) -> Option<HoverInfo> {
        if call.caller.span().contains(self.pos) {
            return self.expr(&call.caller);
        }
        for argument in call.arguments.iter() {
            if argument.span().contains(self.pos) {
                return self.expr(argument);
            }
        }
        return None;
    }
    /// Hover over `this`.
    fn this_expr(&self, _this: &ast::ThisExpr) -> Option<HoverInfo> {
        let current_scope = self.current_scope.borrow().clone();
        let shadow = self.module.ambience.create_shadow(current_scope);
        let method_parent = shadow.get_method_context()?;

        let hvfinder =
            self.create_mock_hover_over(method_parent.entry.ident()?, method_parent.scope.id);

        match method_parent.entry {
            ast::ScopeEntry::Model(model) => {
                name_hover!(model, method_parent.scope, hvfinder)
            }
            ast::ScopeEntry::Trait(trait_) => {
                name_hover!(trait_, method_parent.scope, hvfinder)
            }
            _ => unreachable!(),
        }
        return None;
    }
    /// Hover over an identifier.
    fn identifier(&self, ident: &ast::Identifier) -> Option<HoverInfo> {
        let current_scope = self.current_scope.borrow().clone();
        let shadow = self.module.ambience.create_shadow(current_scope);
        let search = shadow.lookup(&ident.name)?;
        let scope = search.scope;

        // Mock hover over the actual atom.
        let ident = search.entry.ident()?;
        let span = ident.span;
        let pos = span.start;
        let hvfinder = HoverFinder {
            module: self.module,
            graph: self.graph,
            pos,
            current_scope: RefCell::new(scope.id),
        };

        match &search.entry {
            ast::ScopeEntry::Function(f) => name_hover!(f, scope, hvfinder),
            ast::ScopeEntry::Type(t) => name_hover!(t, scope, hvfinder),
            ast::ScopeEntry::Model(m) => name_hover!(m, scope, hvfinder),
            ast::ScopeEntry::Enum(e) => name_hover!(e, scope, hvfinder),
            ast::ScopeEntry::Variable(_) => {}
            ast::ScopeEntry::Parameter(param) => return Some(param.into()),
            ast::ScopeEntry::Trait(t) => name_hover!(t, scope, hvfinder),
            ast::ScopeEntry::UseImport(u) => {
                let declaration = self
                    .module
                    .statements()
                    .map(|statement| statement.closest_nodes_to(u.name.span))
                    .flatten()
                    .next()?;
                return hvfinder.statement(declaration);
            }
            ast::ScopeEntry::ReservedSpace => {}
            ast::ScopeEntry::Constant(c) => name_hover!(c, scope, hvfinder),
            ast::ScopeEntry::ShorthandVariable(_) => {}
        };
        return None;
    }
}

impl HoverFinder<'_> {
    /// Recursively pinpoint types.
    fn type_hover(&self, expression: &TypeExpression, scope_id: usize) -> Option<HoverInfo> {
        if expression.span().contains(self.pos) {
            match expression {
                TypeExpression::Discrete(discrete_type) => {
                    // Hovering over a discrete type name.
                    if discrete_type.name.span.contains(self.pos) {
                        // let type_eval =
                        //     evaluate_discrete_type(&self.module.ambience, discrete_type, scope_id);
                        // if let Ok(eval) = type_eval {
                        //     return Some(HoverInfo::from(&(self.module.ambience, eval)));
                        // }
                    } else {
                        // Hovering over a discrete type generic argument.
                        if let Some(ref generic_args) = discrete_type.generic_args {
                            for expression in generic_args {
                                if let Some(hvinfo) = self.type_hover(expression, scope_id) {
                                    return Some(hvinfo);
                                }
                            }
                        }
                    }
                }
                TypeExpression::Union(union) => {
                    for expression in &union.types {
                        if let Some(hvinfo) = self.type_hover(expression, scope_id) {
                            return Some(hvinfo);
                        }
                    }
                }
                TypeExpression::This { span } => {
                    if span.contains(self.pos) {
                        // Todo: This.
                        return None;
                    }
                }
                _ => return None,
            }
        }
        None
    }

    fn fn_parts_hover(
        &self,
        params: &Vec<Parameter>,
        return_type: &Option<TypeExpression>,
        body: &Block,

        scope_id: usize,
    ) -> Option<HoverInfo> {
        // Hovering over a parameter.
        for parameter in params {
            // Hovering over parameter name.
            if parameter.name.span.contains(self.pos) {
                return Some(parameter.into());
            }
            // Hovering over parameter type.
            if let Some(ref expression) = parameter.type_label {
                if let Some(hover) = self.type_hover(expression, scope_id) {
                    return Some(hover);
                }
            }
        }
        // Hovering over return type.
        if let Some(ref expression) = return_type {
            if let Some(hover) = self.type_hover(expression, scope_id) {
                return Some(hover);
            }
        }
        // Hovering over something in the function's body.
        if !body.span.contains(self.pos) {
            return None;
        }
        let current_scope = *self.current_scope.borrow();
        *self.current_scope.borrow_mut() = body.scope_id;
        for statement in &body.statements {
            let hover_info = self.statement(statement);
            if hover_info.is_some() {
                return hover_info;
            }
        }
        *self.current_scope.borrow_mut() = current_scope;
        return None;
    }

    /// Create a hover with its position set over an identifier.
    fn create_mock_hover_over(&self, ident: &Identifier, current_scope: usize) -> HoverFinder {
        let span = ident.span;
        let pos = span.start;
        HoverFinder {
            module: self.module,
            graph: self.graph,
            pos,
            current_scope: RefCell::new(current_scope),
        }
    }
}
