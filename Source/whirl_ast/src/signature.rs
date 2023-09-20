use crate::{
    EnumVariant, GenericParameter, Identifier, ModuleAmbience, ScopeEntry, Type, TypeEval,
    TypeExpression, TypedValue,
};

use whirl_macros::Signature;

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

#[derive(Debug, Signature)]
pub struct ModelSignature {
    /// Name of the model.
    pub name: Identifier,
    /// Doc comments annotating the function, if any.
    pub info: Option<Vec<String>>,
    /// Whether it was denoted as public.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The constructor parameters.
    pub parameters: Vec<Parameter>,
    /// Implemented Traits.
    pub implementations: Vec<Type>,
    /// The properties of the model.
    pub attributes: Vec<AttributeSignature>,
    /// The methods of the model.
    pub methods: Vec<MethodSignature>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    /// Name of the parameter.
    pub name: Identifier,
    /// Parameter type label.
    pub type_label: Type,
    /// Whether or not the parameter is optional.
    pub is_optional: bool,
    /// Doc comments annotating the parameter, if any.
    pub info: Option<Vec<String>>,
}

impl Signature for Parameter {
    fn info(&self) -> Option<&Vec<String>> {
        self.info.as_ref()
    }
}

/// Entry to mark an attribute.
#[derive(Debug, Signature)]
pub struct AttributeSignature {
    /// Name of the attribute.
    pub name: Identifier,
    /// Documentation about the attribute.
    pub info: Option<Vec<String>>,
    /// Whether or not it is denoted by public.
    pub is_public: bool,
    /// The variable type.
    pub var_type: Type,
}

/// Entry to mark a method.
#[derive(Debug, Signature)]
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
    pub return_type: Type,
}

/// Entry to mark a variable.
#[derive(Debug, Signature)]
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
    pub var_type: Type,
}

/// An entry to mark a function.
#[derive(Debug, Signature)]
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
    pub return_type: Type,
}

/// An entry to mark a trait declaration.
#[derive(Debug, Signature)]
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
    pub implementations: Vec<Type>,
}

/// Entry to mark a type.
#[derive(Debug, Signature)]
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
#[derive(Debug, Signature)]
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

impl TypedValue for AttributeSignature {
    fn evaluated_type(&self) -> Option<&TypeEval> {
        self.var_type.inferred.as_ref()
    }

    fn declared_type(&self) -> Option<&TypeExpression> {
        self.var_type.declared.as_ref()
    }
}

impl Signature for (&Identifier, &EnumVariant) {
    fn info(&self) -> Option<&Vec<String>> {
        self.1.info.as_ref()
    }
}

impl Signature for (&ModuleAmbience, &VariableSignature) {
    fn info(&self) -> Option<&Vec<String>> {
        self.1.info.as_ref()
    }
}

impl Signature for (&ModuleAmbience, TypeEval) {
    fn info(&self) -> Option<&Vec<String>> {
        match self.1 {
            TypeEval::TypeWithinModule { address, .. } => {
                match self.0.get_entry_unguarded(address) {
                    ScopeEntry::Type(typ) => typ.info(),
                    ScopeEntry::Enum(e) => e.info(),
                    ScopeEntry::Model(c) => c.info(),
                    _ => None,
                }
            }
            TypeEval::Unknown | TypeEval::Invalid => None,
        }
    }
}
