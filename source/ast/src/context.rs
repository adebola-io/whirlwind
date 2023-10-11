use crate::{ModuleAmbience, Signature};

/// A signature that is exposed to other modules in the program.
pub struct PublicSignatureContext<'a, T: Signature> {
    pub signature: &'a T,
    pub module_ambience: &'a ModuleAmbience,
}

pub struct ThreeTierContext<'a, T: Signature, U: Signature> {
    pub module_ambience: &'a ModuleAmbience,
    pub parent: &'a T,
    pub signature: &'a U,
}

impl<'a, T: Signature, U: Signature> Signature for ThreeTierContext<'a, T, U> {
    fn info(&self) -> Option<&Vec<String>> {
        self.signature.info()
    }
}

impl<'a, T: Signature> Signature for PublicSignatureContext<'a, T> {
    fn info(&self) -> Option<&Vec<String>> {
        self.signature.info()
    }
}
