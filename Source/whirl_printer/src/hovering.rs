use whirl_ast::{
    AttributeSignature, EnumSignature, EnumVariant, FunctionSignature, GenericParameter,
    Identifier, MethodSignature, ModelSignature, Parameter, ScopeManager, TypeEval, TypeExpression,
    TypeSignature, VariableSignature,
};

use crate::stringify_type_eval;

/// Generator trait for how symbols are illustrated in a hover card.
pub trait HoverFormatter {
    fn to_formatted(&self) -> String;
}

/// Generator trait for how declarations in particular are illustrated.
pub trait SignatureFormatter: HoverFormatter {
    /// Returns documentation info about the declaration.
    fn info(&self) -> Option<&Vec<String>>;
}

pub struct PublicAtomHover<'a, T: SignatureFormatter> {
    pub signature: &'a T,
    pub scope_manager: &'a ScopeManager,
}

impl<'a, T: SignatureFormatter> HoverFormatter for PublicAtomHover<'a, T> {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        // Add module name.
        if let Some(name) = self.scope_manager.get_module_name() {
            string.push_str("module ");
            string.push_str(name);
            string.push('\n');
        }
        string.push_str(&self.signature.to_formatted());
        string
    }
}

impl<'a, T: SignatureFormatter> SignatureFormatter for PublicAtomHover<'a, T> {
    fn info(&self) -> Option<&Vec<String>> {
        self.signature.info()
    }
}

impl HoverFormatter for FunctionSignature {
    fn to_formatted(&self) -> String {
        // Construct function signature.
        let mut string = String::new();
        if self.is_public {
            string.push_str("public ");
        }
        if self.is_async {
            string.push_str("async ");
        }
        string.push_str("function ");
        string.push_str(&self.name.name);
        maybe_print_generic_params(&mut string, self.generic_params.as_ref());
        print_parameters(&mut string, &self.params);
        maybe_print_return_type(&mut string, &self.return_type);
        string
    }
}

impl SignatureFormatter for FunctionSignature {
    fn info(&self) -> Option<&Vec<String>> {
        self.info.as_ref()
    }
}

impl HoverFormatter for TypeSignature {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        if self.is_public {
            string.push_str("public ");
        }
        string.push_str("type ");
        string.push_str(&self.name.name);
        // Todo: Generic params.
        string.push_str(" = ");
        string.push_str(&self.value.to_formatted());
        string
    }
}

impl SignatureFormatter for TypeSignature {
    fn info(&self) -> Option<&Vec<String>> {
        self.info.as_ref()
    }
}

impl HoverFormatter for EnumSignature {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        if self.is_public {
            string.push_str("public ");
        }
        string.push_str("enum ");
        string.push_str(&self.name.name);
        // Todo: Generic params.
        string
    }
}

impl SignatureFormatter for EnumSignature {
    fn info(&self) -> Option<&Vec<String>> {
        self.info.as_ref()
    }
}

impl HoverFormatter for ModelSignature {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        if self.is_public {
            string.push_str("public ");
        }
        string.push_str("model ");
        string.push_str(&self.name.name);
        maybe_print_generic_params(&mut string, self.generic_params.as_ref());
        for _implementation in &self.implementations {
            // todo: implementations
        }
        string
    }
}

impl SignatureFormatter for ModelSignature {
    fn info(&self) -> Option<&Vec<String>> {
        self.info.as_ref()
    }
}

impl HoverFormatter for Parameter {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        string.push_str(&self.name.name);
        if self.is_optional {
            string.push_str("?")
        }
        // Display given or inferred type.
        string.push_str(": ");

        let param_type_str = match self.type_label.declared {
            Some(ref declared) => declared.to_formatted(),
            None => match self.type_label.inferred {
                Some(_) => todo!(),
                None => format!("unknown"),
            },
        };
        string.push_str(&param_type_str);
        string
    }
}

impl SignatureFormatter for Parameter {
    fn info(&self) -> Option<&Vec<String>> {
        self.info.as_ref()
    }
}

impl HoverFormatter for GenericParameter {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        string.push_str(&self.name.name);
        if self.traits.len() > 0 {
            string.push_str(": ");
            // TODO: Traits.
        }
        if let Some(ref default) = self.default {
            string.push_str(" = ");
            string.push_str(&default.to_formatted());
        }
        string
    }
}

impl HoverFormatter for TypeExpression {
    fn to_formatted(&self) -> String {
        match self {
            TypeExpression::Union(union) => {
                let types = &union.types;

                let mut string = String::new();

                for (index, typeexp) in types.iter().enumerate() {
                    string.push_str(&typeexp.to_formatted());
                    if index + 1 < types.len() {
                        string.push_str(" | ");
                    }
                    // Show at most 5 types + the last one.
                    if index == 4 && types.len() > 6 {
                        let len = types.len();
                        string.push_str("... ");
                        string.push_str(&(len - 6).to_string());
                        string.push_str(" more ... | ");
                        string.push_str(&types.last().unwrap().to_formatted());
                        break;
                    }
                }

                string
            }
            TypeExpression::Functional(function) => {
                let mut string = String::from("fn(");

                for (i, param) in function.params.iter().enumerate() {
                    string.push_str(&param.to_formatted());
                    if i + 1 < function.params.len() {
                        string.push_str(", ")
                    }
                }

                string.push(')');

                if let Some(ref rettype) = function.return_type {
                    string.push_str(": ");
                    string.push_str(&rettype.to_formatted())
                }

                string
            }
            TypeExpression::Member(member) => {
                let mut string = member.namespace.to_formatted();
                string.push_str(&member.property.to_formatted());
                string
            }
            TypeExpression::Discrete(discrete) => {
                let mut string = discrete.name.name.to_owned();

                if let Some(ref generic_args) = discrete.generic_args {
                    string.push('<');
                    for (index, genarg) in generic_args.iter().enumerate() {
                        string.push_str(&genarg.to_formatted());
                        if index + 1 < generic_args.len() {
                            string.push_str(", ")
                        }
                    }
                    string.push('>');
                }

                string
            }
            TypeExpression::This { .. } => format!("This"),
            TypeExpression::Invalid => format!("invalid"),
        }
    }
}

impl HoverFormatter for (&Identifier, &EnumVariant) {
    fn to_formatted(&self) -> String {
        let mut string = String::new();

        string.push_str("(variant) ");

        string.push_str(&self.0.name);
        string.push('.');
        string.push_str(&self.1.name.name);

        if let Some(ref s) = self.1.tagged_type {
            string.push('(');
            string.push_str(&s.to_formatted());
            string.push(')');
        }

        string
    }
}

impl SignatureFormatter for (&Identifier, &EnumVariant) {
    fn info(&self) -> Option<&Vec<String>> {
        self.1.info.as_ref()
    }
}

impl HoverFormatter for (&ScopeManager, &VariableSignature) {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        let signature = self.1;
        let scope_manager = self.0;
        if signature.is_public {
            string.push_str("public ");
        }
        string.push_str("var ");
        string.push_str(&signature.name.name);
        string.push_str(": ");
        let var_type = match signature.var_type.inferred {
            Some(ref type_eval) => stringify_type_eval(scope_manager, type_eval),
            None => match signature.var_type.declared {
                Some(ref t) => t.to_formatted(),
                None => format!("unknown"),
            },
        };
        string.push_str(&var_type);
        string
    }
}

impl SignatureFormatter for (&ScopeManager, &VariableSignature) {
    fn info(&self) -> Option<&Vec<String>> {
        self.1.info.as_ref()
    }
}

impl HoverFormatter for (&ScopeManager, TypeEval) {
    fn to_formatted(&self) -> String {
        match self.1 {
            TypeEval::Pointer { address, .. } => match self.0.get_entry_unguarded(address) {
                whirl_ast::ScopeEntry::Type(typ) => typ.to_formatted(),
                whirl_ast::ScopeEntry::Enum(e) => e.to_formatted(),
                whirl_ast::ScopeEntry::Model(c) => c.to_formatted(),
                _ => String::new(),
            },
            TypeEval::Invalid => String::new(),
        }
    }
}

impl SignatureFormatter for (&ScopeManager, TypeEval) {
    fn info(&self) -> Option<&Vec<String>> {
        match self.1 {
            TypeEval::Pointer { address, .. } => match self.0.get_entry_unguarded(address) {
                whirl_ast::ScopeEntry::Type(typ) => typ.info(),
                whirl_ast::ScopeEntry::Enum(e) => e.info(),
                whirl_ast::ScopeEntry::Model(c) => c.info(),
                _ => None,
            },
            TypeEval::Invalid => None,
        }
    }
}

pub struct AttributeHover<'a> {
    pub scope_manager: &'a ScopeManager,
    pub model: &'a ModelSignature,
    pub attribute: &'a AttributeSignature,
}

impl<'a> HoverFormatter for AttributeHover<'a> {
    fn to_formatted(&self) -> String {
        let mut string = self.model.to_formatted();
        string.push('\n');
        let signature = self.attribute;
        if signature.is_public {
            string.push_str("public ");
        }
        string.push_str("var ");
        string.push_str(&signature.name.name);
        string.push_str(": ");
        let var_type = match signature.var_type.inferred {
            Some(ref type_eval) => stringify_type_eval(self.scope_manager, type_eval),
            None => match signature.var_type.declared {
                Some(ref t) => t.to_formatted(),
                None => format!("unknown"),
            },
        };
        string.push_str(&var_type);
        string
    }
}

impl<'a> SignatureFormatter for AttributeHover<'a> {
    fn info(&self) -> Option<&Vec<String>> {
        self.attribute.info.as_ref()
    }
}

pub struct MethodHover<'a> {
    pub scope_manager: &'a ScopeManager,
    pub model: &'a ModelSignature,
    pub method: &'a MethodSignature,
}

impl<'a> HoverFormatter for MethodHover<'a> {
    fn to_formatted(&self) -> String {
        let mut string = self.model.to_formatted();
        string.push('\n');
        let signature = self.method;
        if signature.is_public {
            string.push_str("public ");
        }
        if signature.is_static {
            string.push_str("static ");
        }
        if signature.is_async {
            string.push_str("async ");
        }
        string.push_str("function ");
        string.push_str(&signature.name.name);
        maybe_print_generic_params(&mut string, signature.generic_params.as_ref());
        print_parameters(&mut string, &signature.params);
        maybe_print_return_type(&mut string, &signature.return_type);
        string
    }
}

impl<'a> SignatureFormatter for MethodHover<'a> {
    fn info(&self) -> Option<&Vec<String>> {
        self.method.info.as_ref()
    }
}

/// Print parameters into a string.
fn print_parameters(string: &mut String, params: &Vec<Parameter>) {
    string.push('(');
    for (index, parameter) in params.iter().enumerate() {
        string.push_str(&parameter.to_formatted());
        if index < params.len() - 1 {
            string.push_str(", ");
        }
    }
    string.push(')');
}

/// Print a function's return type if it is available.
fn maybe_print_return_type(string: &mut String, return_type: &whirl_ast::Type) {
    if let Some(ref rettype) = return_type.declared {
        string.push_str(": ");
        string.push_str(&rettype.to_formatted())
    }
}

/// Print generic parameters to a string.
fn maybe_print_generic_params(string: &mut String, generic_params: Option<&Vec<GenericParameter>>) {
    if let Some(ref params) = generic_params {
        string.push('<');
        for (index, param) in params.iter().enumerate() {
            string.push_str(&param.to_formatted());
            if index + 1 != params.len() {
                string.push_str(", ");
            }
        }
        string.push('>');
    }
}
#[cfg(test)]
mod tests {
    use whirl_ast::{
        DiscreteType, FunctionSignature, Identifier, Span, Type, TypeExpression, TypeSignature,
    };

    use crate::HoverFormatter;

    #[test]
    fn formatting_functions() {
        let function = FunctionSignature {
            name: Identifier {
                name: String::from("DoStuff"),
                span: Span::default(),
            },
            info: Some(vec![]),
            is_async: true,
            is_public: true,
            generic_params: None,
            params: vec![],
            return_type: Type {
                declared: Some(TypeExpression::Discrete(DiscreteType {
                    name: Identifier {
                        name: String::from("Deferred"),
                        span: Span::default(),
                    },
                    generic_args: None,
                    span: Span::default(),
                })),
                inferred: None,
            },
        };
        assert_eq!(
            function.to_formatted(),
            "public async function DoStuff(): Deferred"
        );
    }

    #[test]
    fn formatting_types() {
        let type_ = TypeSignature {
            name: Identifier {
                name: String::from("DoStuff"),
                span: Span::default(),
            },
            info: Some(vec![]),
            is_public: true,
            generic_params: None,
            value: TypeExpression::Discrete(DiscreteType {
                name: Identifier {
                    name: String::from("Deferred"),
                    span: Span::default(),
                },
                generic_args: None,
                span: Span::default(),
            }),
        };
        assert_eq!(type_.to_formatted(), "public type DoStuff = Deferred");
    }
}
