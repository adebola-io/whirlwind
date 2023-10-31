use crate::{ParameterType, SymbolIndex};

/// A type expression, as is.
#[derive(Debug, PartialEq, Clone)]
pub enum EvaluatedType {
    /// An instance created with `new A()`, or by labelling a value with `: A`.
    ModelInstance {
        model: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    TraitInstance {
        trait_: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// An instance of an enum created by assigning a variant.
    EnumInstance {
        enum_: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A named or anonymous function.
    FunctionInstance {
        function: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    FunctionExpressionInstance {
        is_async: bool,
        params: Vec<ParameterType>,
        return_type: Box<EvaluatedType>,
        generic_args: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A method.
    MethodInstance {
        method: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A model value.
    Model(SymbolIndex),
    Trait(SymbolIndex),
    Enum(SymbolIndex),
    Module(SymbolIndex),
    OpaqueTypeInstance {
        methods: Vec<SymbolIndex>,
        properties: Vec<SymbolIndex>,
        implementations: Vec<SymbolIndex>,
        collaborators: Vec<SymbolIndex>,
    },
    Void,
    Never,
    Unknown,
    Generic {
        base: SymbolIndex,
    },
    Borrowed {
        base: Box<EvaluatedType>,
    },
}

impl EvaluatedType {
    /// Returns `true` if the evaluated type is [`Model`].
    ///
    /// [`Model`]: EvaluatedType::Model
    #[must_use]
    pub fn is_model(&self) -> bool {
        matches!(self, Self::Model(..))
    }

    /// Returns `true` if the evaluated type is [`Trait`].
    ///
    /// [`Trait`]: EvaluatedType::Trait
    #[must_use]
    pub fn is_trait(&self) -> bool {
        matches!(self, Self::Trait(..))
    }

    /// Returns `true` if the evaluated type is [`Enum`].
    ///
    /// [`Enum`]: EvaluatedType::Enum
    #[must_use]
    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum(..))
    }

    /// Returns `true` if the evaluated type is [`Module`].
    ///
    /// [`Module`]: EvaluatedType::Module
    #[must_use]
    pub fn is_module(&self) -> bool {
        matches!(self, Self::Module(..))
    }

    /// Returns `true` if the evaluated type is [`ModelInstance`].
    ///
    /// [`ModelInstance`]: EvaluatedType::ModelInstance
    #[must_use]
    pub fn is_model_instance(&self) -> bool {
        matches!(self, Self::ModelInstance { .. })
    }
    /// Returns `true` if the evaluated type is [`Void`].
    ///
    /// [`Void`]: EvaluatedType::Void
    #[must_use]
    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    /// Returns `true` if the evaluated type is [`Unknown`].
    ///
    /// [`Unknown`]: EvaluatedType::Unknown
    #[must_use]
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    /// Returns `true` if the evaluated type is [`TraitInstance`].
    ///
    /// [`TraitInstance`]: EvaluatedType::TraitInstance
    #[must_use]
    pub fn is_trait_instance(&self) -> bool {
        matches!(self, Self::TraitInstance { .. })
    }

    /// Returns `true` if the evaluated type is [`Generic`].
    ///
    /// [`Generic`]: EvaluatedType::Generic
    #[must_use]
    pub fn is_generic(&self) -> bool {
        matches!(self, Self::Generic { .. })
    }
}
