use crate::{
    EnumVariant, GenericParameter, Identifier, ModuleAmbience, Span, TypeClause, TypeExpression,
};

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
    /// Implemented Interfaces.
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

#[derive(Debug, Hash)]
pub struct LoopVariable {
    pub name: VariablePattern,
}
#[derive(Debug, Hash)]
pub struct LoopLabel(pub Identifier);

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
    /// The constraint of the method (The type clause that must be satisfied for it to exist.)
    pub constraint: Option<(TypeClause, Span)>,
    /// The parameters of the function, if any.
    pub params: Vec<Parameter>,
    /// Optional return type.
    pub return_type: Option<TypeExpression>,
}

/// Entry to mark a shorthand variable.
#[derive(Debug, Hash)]
pub struct ShorthandVariableSignature {
    /// Name of the variable.
    pub name: Identifier,
    /// Documentation about the variable, if any.
    pub info: Option<Vec<String>>,
    /// The variable's assigned type.
    pub var_type: Option<TypeExpression>,
}

#[derive(Debug, Hash)]
pub enum VariablePattern {
    Identifier(Identifier),
    ObjectPattern {
        real_name: Identifier,
        alias: Option<Identifier>,
        span: Span,
    },
    ArrayPattern(Identifier),
}

/// Entry to mark a variable.
#[derive(Debug, Hash)]
pub struct VariableSignature {
    /// Name of the variable.
    pub name: VariablePattern,
    /// Documentation about the constant, if any.
    pub info: Option<Vec<String>>,
    pub is_public: bool,
    /// The variable type.
    pub var_type: Option<TypeExpression>,
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
    /// Whether or not the function is imported from an external library.
    /// It either contains the name of the library or `None` if it is not imported.
    pub extern_import_source: Option<String>,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The parameters of the function, if any.
    pub params: Vec<Parameter>,
    /// Optional return type.
    pub return_type: Option<TypeExpression>,
}

/// An entry to mark a interface declaration.
#[derive(Debug, Signature, Hash)]
pub struct InterfaceSignature {
    /// Name of the interface.
    pub name: Identifier,
    /// Doc comments annotating the function, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the function is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// Methods on the interface.
    pub methods: Vec<MethodSignature>,
    /// Implemented Interfaces.
    pub implementations: Vec<TypeExpression>,
}

/// Entry to mark a type.
#[derive(Debug, Signature, Hash)]
pub struct TypeEquationSignature {
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

impl Signature for ShorthandVariableSignature {
    fn name(&self) -> &str {
        &self.name.name
    }
    fn info(&self) -> Option<&Vec<String>> {
        self.info.as_ref()
    }
}

impl Signature for ModuleAmbience {
    fn info(&self) -> Option<&Vec<String>> {
        self.module_info.as_ref()
    }
}

impl Signature for (&ModuleAmbience, &ShorthandVariableSignature) {
    fn info(&self) -> Option<&Vec<String>> {
        self.1.info.as_ref()
    }
}
