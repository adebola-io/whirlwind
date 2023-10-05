use crate::{EnumVariant, GenericParameter, Identifier, ModuleAmbience, Span, TypeExpression};

use macros::Signature;

/// An atom or declaration in a scope.
pub trait Signature {
    /// Returns the name of the atom.
    fn name(&self) -> &str {
        ""
    }
    /// Returns the information about the atom.
    fn info(&self) -> Option<&Vec<String>>;
    /// Returns true if the signature is denoted as public.
    fn is_public(&self) -> bool {
        false
    }
}

#[derive(Debug, Signature, Hash)]
pub struct ModelSignature {
    /// Name of the model.
    pub name: Identifier,
    /// Doc comments annotating the function, if any.
    pub info: Option<Vec<String>>,
    /// Whether it was denoted as public.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The constructor parameters, if there is a constructor.
    pub parameters: Option<Vec<Parameter>>,
    /// Implemented Traits.
    pub implementations: Vec<TypeExpression>,
    /// The properties of the model.
    pub attributes: Vec<AttributeSignature>,
    /// The methods of the model.
    pub methods: Vec<MethodSignature>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Parameter {
    /// Name of the parameter.
    pub name: Identifier,
    /// Parameter type label.
    pub type_label: Option<TypeExpression>,
    /// Whether or not the parameter is optional.
    pub is_optional: bool,
    /// Doc comments annotating the parameter, if any.
    pub info: Option<Vec<String>>,
    /// Span of the parameter.
    pub span: Span,
}

impl Signature for Parameter {
    fn info(&self) -> Option<&Vec<String>> {
        self.info.as_ref()
    }
}

/// Entry to mark an attribute.
#[derive(Debug, Signature, Hash)]
pub struct AttributeSignature {
    /// Name of the attribute.
    pub name: Identifier,
    /// Documentation about the attribute.
    pub info: Option<Vec<String>>,
    /// Whether or not it is denoted by public.
    pub is_public: bool,
    /// The variable type.
    pub var_type: TypeExpression,
}

/// Entry to mark a method.
#[derive(Debug, Signature, Hash)]
pub struct MethodSignature {
    /// Name of the method.
    pub name: Identifier,
    /// Doc comments annotating the method, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the method is declared as static.
    pub is_static: bool,
    /// Whether or not the method is denoted by `async`.
    pub is_async: bool,
    /// Whether or not the method is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The parameters of the function, if any.
    pub params: Vec<Parameter>,
    /// Optional return type.
    pub return_type: Option<TypeExpression>,
}

/// Entry to mark a variable.
#[derive(Debug, Signature, Hash)]
pub struct VariableSignature {
    /// Name of the variable.
    pub name: Identifier,
    /// Documentation about the variable, if any.
    pub info: Option<Vec<String>>,
    /// Whether it was declared with shorthand syntax or not.
    pub is_shorthand: bool,
    /// Whether or not the variable is denoted by `public`.
    pub is_public: bool,
    /// The variable's assigned type.
    pub var_type: Option<TypeExpression>,
}

/// Entry to mark a constant.
#[derive(Debug, Signature, Hash)]
pub struct ConstantSignature {
    /// Name of the constant.
    pub name: Identifier,
    /// Documentation about the constant, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the constant is denoted by `public`.
    pub is_public: bool,
    /// The constant's assigned type.
    pub var_type: TypeExpression,
}

/// An entry to mark a function.
#[derive(Debug, Signature, Hash)]
pub struct FunctionSignature {
    /// Name of the function.
    pub name: Identifier,
    /// Doc comments annotating the function, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the function is denoted by `async`.
    pub is_async: bool,
    /// Whether or not the function is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The parameters of the function, if any.
    pub params: Vec<Parameter>,
    /// Optional return type.
    pub return_type: Option<TypeExpression>,
}

/// An entry to mark a trait declaration.
#[derive(Debug, Signature, Hash)]
pub struct TraitSignature {
    /// Name of the trait.
    pub name: Identifier,
    /// Doc comments annotating the function, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the function is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// Methods on the trait.
    pub methods: Vec<MethodSignature>,
    /// Implemented Traits.
    pub implementations: Vec<TypeExpression>,
}

/// Entry to mark a type.
#[derive(Debug, Signature, Hash)]
pub struct TypeSignature {
    /// Type name.
    pub name: Identifier,
    /// Doc comments annotating the type, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the function is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    pub value: TypeExpression,
}

/// Entry to mark an enum.
#[derive(Debug, Signature, Hash)]
pub struct EnumSignature {
    /// enum name.
    pub name: Identifier,
    /// Doc comments annotating the enum, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the function is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The enum variants.
    pub variants: Vec<EnumVariant>,
}

impl Signature for (&Identifier, &EnumVariant) {
    fn info(&self) -> Option<&Vec<String>> {
        self.1.info.as_ref()
    }
}

impl Signature for ModuleAmbience {
    fn info(&self) -> Option<&Vec<String>> {
        self.module_info.as_ref()
    }
}

impl Signature for (&ModuleAmbience, &VariableSignature) {
    fn info(&self) -> Option<&Vec<String>> {
        self.1.info.as_ref()
    }
}
