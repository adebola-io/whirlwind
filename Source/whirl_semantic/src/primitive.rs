use whirl_ast::{
    GenericParameter, Identifier, ModelSignature, Span, TypeExpression, TypeSignature,
};

/// Inbuilt values in the language.
pub struct Primitives {
    pub models: [ModelSignature; 4],
    pub types: [TypeSignature; 0],
}

impl Primitives {
    pub fn create() -> Self {
        Self {
            types: [],
            models: [
                create_model_primitive(
                    "String",
                    "Model for creating, manipulating and formatting of text sequences.",
                    None,
                    vec![],
                ),
                create_model_primitive(
                    "Integer",
                    "Allows for managing of 64-bit whole numbers.",
                    None,
                    vec![],
                ),
                create_model_primitive(
                    "Boolean",
                    "An item that can only have `true` or `false` as its values.",
                    None,
                    vec![],
                ),
                create_model_primitive(
                    "ArrayOf",
                    "A continuous growable group of items.",
                    Some(vec![GenericParameter {
                        name: Identifier {
                            name: format!("T"),
                            span: Span::default(),
                        },
                        traits: vec![],
                        default: None,
                    }]),
                    vec![],
                ),
            ],
        }
    }
}

fn create_model_primitive(
    name: &str,
    info: &str,
    generic_params: Option<Vec<GenericParameter>>,
    implementations: Vec<TypeExpression>,
) -> ModelSignature {
    ModelSignature {
        name: Identifier {
            name: format!("{}", name),
            span: Span::default(),
        },
        info: Some(vec![format!("{}", info)]),
        is_public: false,
        generic_params,
        implementations,
    }
}
