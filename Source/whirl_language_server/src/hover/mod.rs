use tower_lsp::lsp_types::{Hover, HoverContents, LanguageString, MarkedString};
use whirl_ast::{ASTVisitor, HoverFormatter, Parameter, ScopeManager, SignatureFormatter};

/// Information shown during hovering.
pub struct HoverInfo {
    pub contents: HoverContents,
}

impl From<&str> for HoverInfo {
    fn from(value: &str) -> Self {
        HoverInfo {
            contents: HoverContents::Scalar(MarkedString::String(value.to_owned())),
        }
    }
}

impl<T: SignatureFormatter> From<&T> for HoverInfo {
    fn from(value: &T) -> Self {
        let mut info = vec![];
        info.push(MarkedString::LanguageString(LanguageString {
            language: String::from("wrl"),
            value: value.to_formatted(),
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
    scope_manager: &'a ScopeManager,
}

impl<'a> HoverFinder<'a> {
    pub fn with_scope_manager(scope_manager: &'a ScopeManager) -> Self {
        Self { scope_manager }
    }
}

impl<'a> ASTVisitor<[u32; 2], Option<HoverInfo>> for HoverFinder<'a> {
    fn visit_function(
        &self,
        function: &whirl_ast::FunctionDeclaration,
        args: &[u32; 2],
    ) -> Option<HoverInfo> {
        let scope = self.scope_manager.get_scope(function.address.scope_id)?;
        let signature = scope.get_function(function.address.entry_no)?;
        let body = &function.body;

        // Hovering over the function name.
        if signature.name.span.contains(*args) {
            return Some(signature.into());
        }
        // Hovering over a parameter.
        for parameter in &signature.params {
            let visit = self.visit_parameter(parameter, args);
            if visit.is_some() {
                return visit;
            }
        }
        // Hovering over something in the function's body.
        if !body.span.contains(*args) {
            return None;
        }
        for statement in &body.statements {
            let hover_info = self.visit_statement(statement, args);
            if hover_info.is_some() {
                return hover_info;
            }
        }
        return None;
    }

    fn visit_parameter(&self, parameter: &Parameter, args: &[u32; 2]) -> Option<HoverInfo> {
        // Hovering over parameter name.
        if parameter.name.span.contains(*args) {
            return Some(HoverInfo {
                contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                    language: format!("wrl"),
                    value: parameter.to_formatted(),
                })),
            });
        }
        // todo: Hovering over the parameter type.
        return None;
    }

    fn visit_type_declaration(
        &self,
        type_decl: &whirl_ast::TypeDeclaration,
        args: &[u32; 2],
    ) -> Option<HoverInfo> {
        let scope = self.scope_manager.get_scope(type_decl.address.scope_id)?;
        let signature = scope.get_type(type_decl.address.entry_no)?;
        // Hovering over the type name.

        if signature.name.span.contains(*args) {
            return Some(signature.into());
        }
        return None;
    }

    fn visit_enum_declaration(
        &self,
        enum_decl: &whirl_ast::EnumDeclaration,
        args: &[u32; 2],
    ) -> Option<HoverInfo> {
        let scope = self.scope_manager.get_scope(enum_decl.address.scope_id)?;
        let signature = scope.get_enum(enum_decl.address.entry_no)?;

        // Hovering over enum name.
        if signature.name.span.contains(*args) {
            return Some(signature.into());
        }

        for variant in &signature.variants {
            if variant.span.contains(*args) {
                return Some(HoverInfo::from(&(&signature.name, variant)));
            }
        }
        return None;
    }
}
