use crate::{
    AttributeSignature, MethodSignature, ModelSignature, ModuleScope, Signature, TypedValue,
};

/// A compact representation of a typed value and the module scope.
pub struct TypedValueContext<'a, T: TypedValue> {
    pub module_scope: &'a ModuleScope,
    pub atom: T,
}

/// A signature that is exposed to other modules in the program.
pub struct PublicSignatureContext<'a, T: Signature> {
    pub signature: &'a T,
    pub module_scope: &'a ModuleScope,
}

pub struct AttributeContext<'a> {
    pub module_scope: &'a ModuleScope,
    pub model: &'a ModelSignature,
    pub attribute: &'a AttributeSignature,
}

pub struct MethodContext<'a> {
    pub module_scope: &'a ModuleScope,
    pub model: &'a ModelSignature,
    pub method: &'a MethodSignature,
}

impl<'a> Signature for AttributeContext<'a> {
    fn info(&self) -> Option<&Vec<String>> {
        self.attribute.info.as_ref()
    }
}

impl<'a> Signature for MethodContext<'a> {
    fn info(&self) -> Option<&Vec<String>> {
        self.method.info.as_ref()
    }
}

impl<'a, T: Signature> Signature for PublicSignatureContext<'a, T> {
    fn info(&self) -> Option<&Vec<String>> {
        self.signature.info()
    }
}
