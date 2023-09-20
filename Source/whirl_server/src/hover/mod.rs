use tower_lsp::lsp_types::{Hover, HoverContents, LanguageString, MarkedString};
use whirl_analyzer::type_utils::evaluate_discrete_type;
use whirl_ast::{
    ASTVisitorNoArgs, Block, ModelPropertyType, ModuleAmbience, Parameter, PublicSignatureContext,
    Signature, Span, ThreeTierContext, TraitPropertyType, Type, TypeExpression, TypedValueContext,
};
use whirl_printer::HoverFormatter;

/// I can only write it so many times.
/// Generates a hover over signatures.
macro_rules! name_hover {
    ($signature: expr, $scope: expr, $hvfinder: expr) => {{
        if $signature.name.span.contains($hvfinder.pos) {
            if $scope.is_global() && $signature.is_public {
                let global_model_hover = PublicSignatureContext {
                    signature: $signature,
                    module_ambience: $hvfinder.module_ambience,
                };
                return Some((&global_model_hover).into());
            }
            return Some($signature.into());
        }
    }};
}

/// Generates hover for types.
macro_rules! type_hover {
    ($typ: expr, $scope: expr, $self: expr) => {
        if let Some(ref expression) = $typ.declared {
            if let Some(hover) = $self.type_hover(expression, $scope.id) {
                return Some(hover);
            }
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
                module_ambience: $self.module_ambience,
            };
            if $scope.is_global() && $parent.is_public && $sgn1.is_public {
                let global_hover = PublicSignatureContext {
                    module_ambience: $self.module_ambience,
                    signature: &hover_over,
                };
                return Some((&global_hover).into());
            }
            return Some((&hover_over).into());
        }
    };};
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
    module_ambience: &'a ModuleAmbience,
    pos: [u32; 2],
}

impl<'a> HoverFinder<'a> {
    pub fn new(module_ambience: &'a ModuleAmbience, pos: [u32; 2]) -> Self {
        Self {
            module_ambience,
            pos,
        }
    }
}

impl<'a> ASTVisitorNoArgs<Option<HoverInfo>> for HoverFinder<'a> {
    /// Hover over the module declaration.
    fn module_declaration(&self, _module: &whirl_ast::ModuleDeclaration) -> Option<HoverInfo> {
        if _module.span.contains(self.pos) {
            return Some(HoverInfo::from_str(
                format!("module {}", self.module_ambience.get_module_name()?).as_str(),
            ));
        }
        return None;
    }

    /// Hover over a shorthand variable declaration.
    fn shorthand_var_decl(
        &self,
        var_decl: &whirl_ast::ShorthandVariableDeclaration,
    ) -> Option<HoverInfo> {
        let scope = self.module_ambience.get_scope(var_decl.address.scope_id)?;
        let signature = scope.get_variable(var_decl.address.entry_no)?;
        // hover over variable name.
        if signature.name.span.contains(self.pos) {
            return Some(HoverInfo::from(&(self.module_ambience, signature)));
        }
        // hover over type.
        type_hover!(signature.var_type, scope, self);
        // hover over value.
        return self.expr(&var_decl.value);
    }

    /// Hover over a model.
    fn model_decl(&self, model_decl: &whirl_ast::ModelDeclaration) -> Option<HoverInfo> {
        let module_ambience = self.module_ambience;
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
                        sub_name_hover!(
                            attribute,
                            model,
                            scope,
                            &TypedValueContext {
                                module_ambience,
                                atom: attribute
                            },
                            self
                        );
                        // hover over attribute type.
                        type_hover!(attribute.var_type, scope, self);
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
                    ModelPropertyType::TraitImpl { .. } => return None,
                }
            }
        }
        return None;
    }

    fn trait_declaraion(&self, trait_decl: &whirl_ast::TraitDeclaration) -> Option<HoverInfo> {
        let module_ambience = self.module_ambience;
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

    /// Hover over a function.
    fn function(&self, function: &whirl_ast::FunctionDeclaration) -> Option<HoverInfo> {
        let scope = self.module_ambience.get_scope(function.address.scope_id)?;
        let signature = scope.get_function(function.address.entry_no)?;
        let body = &function.body;
        // hover over function name.
        name_hover!(signature, scope, self);
        // hover over function parts.
        return self.fn_parts_hover(&signature.params, &signature.return_type, body, scope.id);
    }

    /// Hover over a type declaration.:
    fn type_decl(&self, type_decl: &whirl_ast::TypeDeclaration) -> Option<HoverInfo> {
        let scope = self.module_ambience.get_scope(type_decl.address.scope_id)?;
        let signature = scope.get_type(type_decl.address.entry_no)?;
        // hover over type name.
        name_hover!(signature, scope, self);
        // hover over type value.
        return self.type_hover(&signature.value, scope.id);
    }

    /// Hover over an enum declaration.
    fn enum_decl(&self, enum_decl: &whirl_ast::EnumDeclaration) -> Option<HoverInfo> {
        let scope = self.module_ambience.get_scope(enum_decl.address.scope_id)?;
        let signature = scope.get_enum(enum_decl.address.entry_no)?;
        // hover over enum name.
        name_hover!(signature, scope, self);
        // hover over variant.
        for variant in &signature.variants {
            if variant.span.contains(self.pos) {
                if scope.is_global() && signature.is_public {
                    let global_hover = PublicSignatureContext {
                        module_ambience: self.module_ambience,
                        signature: &(&signature.name, variant),
                    };
                    return Some((&global_hover).into());
                }
                return Some(HoverInfo::from(&(&signature.name, variant)));
            }
        }
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
                        let type_eval =
                            evaluate_discrete_type(&self.module_ambience, discrete_type, scope_id);
                        if let Ok(eval) = type_eval {
                            return Some(HoverInfo::from(&(self.module_ambience, eval)));
                        }
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
        return_type: &Type,
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
            if let Some(ref expression) = parameter.type_label.declared {
                if let Some(hover) = self.type_hover(expression, scope_id) {
                    return Some(hover);
                }
            }
        }
        // Hovering over return type.
        if let Some(ref expression) = return_type.declared {
            if let Some(hover) = self.type_hover(expression, scope_id) {
                return Some(hover);
            }
        }
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
}
