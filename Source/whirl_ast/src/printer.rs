use crate::{
    EnumSignature, EnumVariant, FunctionSignature, Identifier, Parameter, TypeExpression,
    TypeSignature,
};

/// Generator trait for how symbols are illustrated in a hover card.
pub trait HoverFormatter {
    fn to_formatted(&self) -> String;
}

/// Generator trait for how declarations in particular are illustrated.
pub trait SignatureFormatter: HoverFormatter {
    /// Returns documentation info about the declaration.
    fn info(&self) -> Option<&Vec<String>>;
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

        // TODO: Generic Parameters.

        string.push('(');

        for (index, parameter) in self.params.iter().enumerate() {
            string.push_str(&parameter.to_formatted());
            if index < self.params.len() - 1 {
                string.push_str(", ");
            }
        }
        string.push(')');

        if let Some(ref rettype) = self.return_type.declared {
            string.push_str(": ");
            string.push_str(&rettype.to_formatted())
        }

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

#[cfg(test)]
mod tests {
    use crate::{FunctionSignature, HoverFormatter, Identifier, Span, TypeSignature};

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
            return_type: crate::Type {
                declared: Some(crate::TypeExpression::Discrete(crate::DiscreteType {
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
            value: crate::TypeExpression::Discrete(crate::DiscreteType {
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
