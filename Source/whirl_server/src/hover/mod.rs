use tower_lsp::lsp_types::{Hover, HoverContents, LanguageString, MarkedString};
use whirl_analyzer::type_utils::evaluate_discrete_type;
use whirl_ast::{
    ASTVisitor, AttributeContext, MethodContext, ModelPropertyType, ModuleAmbience,
    PublicSignatureContext, Signature, TypeExpression,
};
use whirl_printer::HoverFormatter;

/// Information shown during hovering.
pub struct HoverInfo {
    pub contents: HoverContents,
}

// impl HoverInfo {
//     fn from_string(value: &str) -> Self {
//         HoverInfo {
//             contents: HoverContents::Scalar(MarkedString::String(value.to_owned())),
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
    module_ambience: &'a ModuleAmbience,
}

impl<'a> HoverFinder<'a> {
    pub fn with_module_ambience(module_ambience: &'a ModuleAmbience) -> Self {
        Self { module_ambience }
    }
}

impl<'a> ASTVisitor<[u32; 2], Option<HoverInfo>> for HoverFinder<'a> {
    /// Hover over a shorthand variable declaration.
    fn shorthand_var_decl(
        &self,
        var_decl: &whirl_ast::ShorthandVariableDeclaration,
        args: &[u32; 2],
    ) -> Option<HoverInfo> {
        let scope = self.module_ambience.get_scope(var_decl.address.scope_id)?;
        let signature = scope.get_variable(var_decl.address.entry_no)?;

        // Hovering over a variable name.
        if signature.name.span.contains(*args) {
            return Some(HoverInfo::from(&(self.module_ambience, signature)));
        }

        // Hovering over a variable type.
        if let Some(ref expression) = signature.var_type.declared {
            if let Some(hover) = type_hover(self.module_ambience, expression, scope.id, args) {
                return Some(hover);
            }
        }

        return None;
    }

    /// Hover over a model.
    fn model_decl(
        &self,
        model_decl: &whirl_ast::ModelDeclaration,
        args: &[u32; 2],
    ) -> Option<HoverInfo> {
        let module_ambience = self.module_ambience;
        let scope = module_ambience.get_scope(model_decl.address.scope_id)?;
        let model = scope.get_model(model_decl.address.entry_no)?;
        // Hovering over a variable name.
        if model.name.span.contains(*args) {
            // Global model functions.
            if scope.is_global() && model.is_public {
                let global_model_hover = PublicSignatureContext {
                    signature: model,
                    module_ambience,
                };
                return Some((&global_model_hover).into());
            }
            return Some(model.into());
        }
        // Hovering over a trait.
        for implentation in &model.implementations {
            // Hovering over parameter type.
            if let Some(ref expression) = implentation.declared {
                if let Some(hover) = type_hover(module_ambience, expression, scope.id, args) {
                    return Some(hover);
                }
            }
        }
        // Hovering over an attribute, method or implementation.
        for property in &model_decl.body.properties {
            if property.span.contains(*args) {
                match &property._type {
                    ModelPropertyType::Attribute => {
                        let attribute = model.attributes.get(property.index)?;
                        // Hovering over an attribute name.
                        if attribute.name.span.contains(*args) {
                            let hover_over_attrib = AttributeContext {
                                attribute,
                                model,
                                module_ambience,
                            };
                            if scope.is_global() && model.is_public && attribute.is_public {
                                let global_attrib_hover = PublicSignatureContext {
                                    module_ambience,
                                    signature: &hover_over_attrib,
                                };
                                return Some((&global_attrib_hover).into());
                            }

                            return Some((&hover_over_attrib).into());
                        }
                        // Hovering over an attribute type.
                        if let Some(ref expression) = attribute.var_type.declared {
                            if let Some(hover) =
                                type_hover(module_ambience, expression, scope.id, args)
                            {
                                return Some(hover);
                            }
                        }
                    }
                    ModelPropertyType::Method { body } => {
                        let method = model.methods.get(property.index)?;
                        // Hovering over a method name.
                        if method.name.span.contains(*args) {
                            let hover_over_function = MethodContext {
                                method,
                                model,
                                module_ambience,
                            };
                            if scope.is_global() && model.is_public && method.is_public {
                                let global_hover = PublicSignatureContext {
                                    module_ambience,
                                    signature: &hover_over_function,
                                };
                                return Some((&global_hover).into());
                            }
                            return Some((&hover_over_function).into());
                        }
                        // Hovering over a parameter.
                        for parameter in &method.params {
                            // Hovering over parameter name.
                            if parameter.name.span.contains(*args) {
                                return Some(parameter.into());
                            }
                            // Hovering over parameter type.
                            if let Some(ref expression) = parameter.type_label.declared {
                                if let Some(hover) =
                                    type_hover(self.module_ambience, expression, scope.id, args)
                                {
                                    return Some(hover);
                                }
                            }
                        }
                        // Hovering over return type.
                        if let Some(ref expression) = method.return_type.declared {
                            if let Some(hover) =
                                type_hover(self.module_ambience, expression, scope.id, args)
                            {
                                return Some(hover);
                            }
                        }
                        // Hovering over something in the function's body.
                        if !body.span.contains(*args) {
                            return None;
                        }
                        for statement in &body.statements {
                            let hover_info = self.statement(statement, args);
                            if hover_info.is_some() {
                                return hover_info;
                            }
                        }
                    }
                    ModelPropertyType::TraitImpl { .. } => return None,
                }
            }
        }
        return None;
    }

    /// Hover over a function.
    fn function(
        &self,
        function: &whirl_ast::FunctionDeclaration,
        args: &[u32; 2],
    ) -> Option<HoverInfo> {
        let module_ambience = &self.module_ambience;
        let scope = self.module_ambience.get_scope(function.address.scope_id)?;
        let signature = scope.get_function(function.address.entry_no)?;
        let body = &function.body;
        // Hovering over the function name.
        if signature.name.span.contains(*args) {
            // Global public functions.
            if scope.is_global() && signature.is_public {
                let global_function_hover = PublicSignatureContext {
                    signature,
                    module_ambience,
                };
                return Some((&global_function_hover).into());
            }
            return Some(signature.into());
        }
        // Hovering over a parameter.
        for parameter in &signature.params {
            // Hovering over parameter name.
            if parameter.name.span.contains(*args) {
                return Some(parameter.into());
            }
            // Hovering over parameter type.
            if let Some(ref expression) = parameter.type_label.declared {
                if let Some(hover) = type_hover(self.module_ambience, expression, scope.id, args) {
                    return Some(hover);
                }
            }
        }
        // Hovering over return type.
        if let Some(ref expression) = signature.return_type.declared {
            if let Some(hover) = type_hover(self.module_ambience, expression, scope.id, args) {
                return Some(hover);
            }
        }
        // Hovering over something in the function's body.
        if !body.span.contains(*args) {
            return None;
        }
        for statement in &body.statements {
            let hover_info = self.statement(statement, args);
            if hover_info.is_some() {
                return hover_info;
            }
        }
        return None;
    }

    /// Hover over a type declaration.:
    fn type_decl(
        &self,
        type_decl: &whirl_ast::TypeDeclaration,
        args: &[u32; 2],
    ) -> Option<HoverInfo> {
        let scope = self.module_ambience.get_scope(type_decl.address.scope_id)?;
        let signature = scope.get_type(type_decl.address.entry_no)?;
        // Hovering over the type name.
        if signature.name.span.contains(*args) {
            if scope.is_global() && signature.is_public {
                let global_hover = PublicSignatureContext {
                    module_ambience: self.module_ambience,
                    signature,
                };
                return Some((&global_hover).into());
            }
            return Some(signature.into());
        }
        return None;
    }

    /// Hover over an enum declaration.
    fn enum_decl(
        &self,
        enum_decl: &whirl_ast::EnumDeclaration,
        args: &[u32; 2],
    ) -> Option<HoverInfo> {
        let scope = self.module_ambience.get_scope(enum_decl.address.scope_id)?;
        let signature = scope.get_enum(enum_decl.address.entry_no)?;

        // Hovering over enum name.
        if signature.name.span.contains(*args) {
            if scope.is_global() && signature.is_public {
                let global_hover = PublicSignatureContext {
                    module_ambience: self.module_ambience,
                    signature,
                };
                return Some((&global_hover).into());
            }
            return Some(signature.into());
        }

        for variant in &signature.variants {
            if variant.span.contains(*args) {
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

/// Recursively pinpoint types.
fn type_hover(
    module_ambience: &ModuleAmbience,
    expression: &TypeExpression,
    scope: usize,
    args: &[u32; 2],
) -> Option<HoverInfo> {
    if expression.span().contains(*args) {
        match expression {
            TypeExpression::Discrete(discrete_type) => {
                // Hovering over a discrete type name.
                if discrete_type.name.span.contains(*args) {
                    let type_eval = evaluate_discrete_type(module_ambience, discrete_type, scope);
                    if let Ok(eval) = type_eval {
                        return Some(HoverInfo::from(&(module_ambience, eval)));
                    }
                } else {
                    // Hovering over a discrete type generic argument.
                    if let Some(ref generic_args) = discrete_type.generic_args {
                        for expression in generic_args {
                            if let Some(hvinfo) =
                                type_hover(module_ambience, expression, scope, args)
                            {
                                return Some(hvinfo);
                            }
                        }
                    }
                }
            }
            TypeExpression::Union(union) => {
                for expression in &union.types {
                    if let Some(hvinfo) = type_hover(module_ambience, expression, scope, args) {
                        return Some(hvinfo);
                    }
                }
            }
            TypeExpression::This { span } => {
                if span.contains(*args) {
                    // Todo: This.
                    return None;
                }
            }
            _ => return None,
        }
    }
    None
}
