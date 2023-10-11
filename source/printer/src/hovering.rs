use ast::{
    ConstantSignature, EnumSignature, EnumVariant, FunctionSignature, GenericParameter, Identifier,
    MethodSignature, ModelSignature, ModuleAmbience, Parameter, PublicSignatureContext,
    ShorthandVariableSignature, Signature, ThreeTierContext, TraitSignature, TypeExpression,
    TypeSignature,
};

/// Generator trait for how symbols are illustrated in a hover card.
pub trait HoverFormatter {
    fn to_formatted(&self) -> String;
}

impl HoverFormatter for ModuleAmbience {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        string.push_str("module ");
        match self.get_module_name() {
            Some(name) => string.push_str(name),
            None => string.push_str("{unknown}"),
        }
        string
    }
}

impl<'a, T: Signature + HoverFormatter> HoverFormatter for PublicSignatureContext<'a, T> {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        // Add module name.
        if let Some(name) = self.module_ambience.get_module_name() {
            string.push_str("module ");
            string.push_str(name);
            string.push('\n');
        }
        string.push_str(&self.signature.to_formatted());
        string
    }
}

impl HoverFormatter for FunctionSignature {
    fn to_formatted(&self) -> String {
        // Construct function signature.
        let mut string = String::new();
        maybe_print_public(&mut string, self);
        if self.is_async {
            string.push_str("async ");
        }
        string.push_str("function ");
        string.push_str(&self.name.name);
        maybe_print_generic_params(&mut string, self.generic_params.as_ref());
        print_parameters(&mut string, &self.params);
        maybe_print_return_type(&mut string, self.return_type.as_ref());
        string
    }
}

impl HoverFormatter for TypeSignature {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        maybe_print_public(&mut string, self);
        string.push_str("type ");
        string.push_str(&self.name.name);
        // Todo: Generic params.
        string.push_str(" = ");
        string.push_str(&self.value.to_formatted());
        string
    }
}

impl HoverFormatter for EnumSignature {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        maybe_print_public(&mut string, self);
        string.push_str("enum ");
        string.push_str(&self.name.name);
        // Todo: Generic params.
        string
    }
}

impl HoverFormatter for ModelSignature {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        maybe_print_public(&mut string, self);
        string.push_str("model ");
        string.push_str(&self.name.name);
        maybe_print_generic_params(&mut string, self.generic_params.as_ref());
        for _implementation in &self.implementations {
            // todo: implementations
        }
        string
    }
}

impl HoverFormatter for MethodSignature {
    fn to_formatted(&self) -> String {
        // Construct function signature.
        let mut string = String::new();
        maybe_print_public(&mut string, self);
        if self.is_static {
            string.push_str("static ")
        }
        if self.is_async {
            string.push_str("async ");
        }
        string.push_str("function ");
        string.push_str(&self.name.name);
        maybe_print_generic_params(&mut string, self.generic_params.as_ref());
        print_parameters(&mut string, &self.params);
        maybe_print_return_type(&mut string, self.return_type.as_ref());
        string
    }
}

impl HoverFormatter for TraitSignature {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        maybe_print_public(&mut string, self);
        string.push_str("trait ");
        string.push_str(&self.name.name);
        maybe_print_generic_params(&mut string, self.generic_params.as_ref());
        for _implementation in &self.implementations {
            // todo: implementations
        }
        string
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

        let param_type_str = match self.type_label {
            Some(ref declared) => declared.to_formatted(),
            None => format!("invalid"),
        };
        string.push_str(&param_type_str);
        string
    }
}

impl HoverFormatter for GenericParameter {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        string.push_str(&self.name.name);
        if self.traits.len() > 0 {
            string.push_str(": ");
            for (idx, trait_) in self.traits.iter().enumerate() {
                string.push_str(&trait_.to_formatted());
                if idx + 1 != self.traits.len() {
                    string.push_str(" + ");
                }
            }
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
            TypeExpression::BorrowedType(borrowedtype) => {
                format!("&{}", borrowedtype.value.to_formatted())
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

        // if let Some(ref s) = self.1.tagged_type {
        //     string.push('(');
        //     string.push_str(&s.to_formatted());
        //     string.push(')');
        // }

        string
    }
}

impl HoverFormatter for (&ModuleAmbience, &ShorthandVariableSignature) {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        let signature = self.1;
        let _module_ambience = self.0;
        maybe_print_public(&mut string, signature);
        string.push_str("var ");
        string.push_str(&signature.name.name);
        string.push_str(": ");
        let var_type = match signature.var_type {
            Some(ref _type_eval) => todo!(),
            None => format!("unknown"),
        };
        string.push_str(&var_type);
        string
    }
}

impl HoverFormatter for ConstantSignature {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        maybe_print_public(&mut string, self);
        string.push_str("const ");
        string.push_str(&self.name.name);
        string.push_str(": ");
        string.push_str(&self.var_type.to_formatted());
        string
    }
}

impl<'a, T: Signature + HoverFormatter, U: Signature + HoverFormatter> HoverFormatter
    for ThreeTierContext<'a, T, U>
{
    fn to_formatted(&self) -> String {
        let mut string = self.parent.to_formatted();
        string.push('\n');
        string.push_str(&self.signature.to_formatted());
        string
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
fn maybe_print_return_type(string: &mut String, return_type: Option<&ast::TypeExpression>) {
    if let Some(ref rettype) = return_type {
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

fn maybe_print_public<T: Signature>(string: &mut String, signature: &T) {
    if signature.is_public() {
        string.push_str("public ");
    }
}

#[cfg(test)]
mod tests {
    use ast::{DiscreteType, FunctionSignature, Identifier, Span, TypeExpression, TypeSignature};

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
            return_type: Some(TypeExpression::Discrete(DiscreteType {
                name: Identifier {
                    name: String::from("Deferred"),
                    span: Span::default(),
                },
                generic_args: None,
                span: Span::default(),
            })),
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
