use whirl_ast::{ClassSignature, Identifier, Span, Type, TypeSignature};

/// Inbuilt values in the language.
pub struct Primitives {
    pub classes: [ClassSignature; 3],
    pub types: [TypeSignature; 0],
}

impl Primitives {
    pub fn create() -> Self {
        Self {
            types: [],
            classes: [
                create_class_primitive(
                    "String",
                    "Class for creating, manipulating and formatting of text sequences.",
                    vec![],
                ),
                create_class_primitive(
                    "Integer",
                    "Allows for managing of 64-bit whole numbers.",
                    vec![],
                ),
                create_class_primitive(
                    "Boolean",
                    "An item that can only have 'true' or 'false' as its values.",
                    vec![],
                ),
            ],
        }
    }
}

fn create_class_primitive(name: &str, info: &str, implementations: Vec<Type>) -> ClassSignature {
    ClassSignature {
        name: Identifier {
            name: format!("{}", name),
            span: Span::default(),
        },
        info: Some(vec![format!("{}", info)]),
        is_public: false,
        generic_params: None,
        extensions: vec![],
        implementations,
    }
}
