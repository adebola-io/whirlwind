use crate::{Identifier, Parameter, ScopeAddress, Span};

/// Stores the address of a symbol in relation to an entire workspace.
#[derive(Debug, PartialEq)]
pub struct ModuleAddress {
    pub module_id: usize,
    pub scope_address: ScopeAddress,
}

/// The first representation of a type.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Type {
    pub declared: Option<TypeExpression>,
    pub inferred: Option<TypeEval>,
}

impl Type {
    /// Create a nothing type.
    pub fn empty() -> Self {
        Type {
            declared: None,
            inferred: None,
        }
    }
    pub fn from_expression(expression: TypeExpression) -> Self {
        Type {
            declared: Some(expression),
            inferred: None,
        }
    }
}

/// Result of a type evaluation.
#[derive(Default, PartialEq, Debug, Clone)]
pub enum TypeEval {
    /// An address of a scope entry.
    Pointer {
        address: ScopeAddress,
        args: Option<Vec<TypeEval>>,
    },
    /// A type error.
    #[default]
    Invalid,
    Unknown,
}
impl TypeEval {
    pub fn is_invalid(&self) -> bool {
        matches!(self, TypeEval::Invalid)
    }
}

pub trait TypedValue {
    /// Returns the type evaluated for the value.
    fn evaluated_type(&self) -> Option<&TypeEval>;
    /// Returns the type declared for the value.
    fn declared_type(&self) -> Option<&TypeExpression>;
}

#[derive(PartialEq, Clone)]
pub enum TypeExpression {
    Union(UnionType),
    Functional(FunctionalType),
    Member(MemberType),
    Discrete(DiscreteType),
    This { span: Span },
    Invalid,
}

/// A complex union type e.g. `type Animal = Cat | Dog | Parrot; `
#[derive(Debug, PartialEq, Clone)]
pub struct UnionType {
    pub types: Vec<TypeExpression>,
    pub span: Span,
}

/// A function type e.g. `type Predicate<T> = fn(value: T): Boolean;`
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionalType {
    pub params: Vec<Parameter>,
    pub generic_params: Option<Vec<GenericParameter>>,
    pub return_type: Option<Box<TypeExpression>>,
    pub span: Span,
}

/// A type that is part of an member expression. e.g. `type Error = Core.Io.Error`;
#[derive(Debug, PartialEq, Clone)]
pub struct MemberType {
    pub namespace: Box<TypeExpression>,
    pub property: Box<TypeExpression>,
    pub span: Span,
}

/// A simple type. e.g. `type SignalValue = Number;`
#[derive(Debug, PartialEq, Clone)]
pub struct DiscreteType {
    pub name: Identifier,
    pub generic_args: Option<Vec<TypeExpression>>,
    pub span: Span,
}

impl std::fmt::Display for TypeExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// A type parameter on a function, model or type.
#[derive(Debug, PartialEq, Clone)]
pub struct GenericParameter {
    pub name: Identifier,
    pub traits: Vec<TypeExpression>,
    pub default: Option<TypeExpression>,
}

impl TypeExpression {
    pub fn span(&self) -> Span {
        match self {
            TypeExpression::Union(u) => u.span,
            TypeExpression::Functional(f) => f.span,
            TypeExpression::Member(m) => m.span,
            TypeExpression::Discrete(d) => d.span,
            TypeExpression::This { span } => span.clone(),
            TypeExpression::Invalid => Span::default(),
        }
    }

    pub fn as_eval(&self) -> TypeEval {
        todo!()
    }
}

impl std::fmt::Debug for TypeExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Union(arg0) => f
                .debug_struct("UnionType")
                .field("types", &arg0.types)
                .field("span", &arg0.span)
                .finish(),
            Self::Functional(arg0) => f
                .debug_struct("FunctionalType")
                .field("generic_params", &arg0.generic_params)
                .field("params", &arg0.params)
                .field("return_type", &arg0.return_type)
                .field("span", &arg0.span)
                .finish(),
            Self::Member(arg0) => f
                .debug_struct("MemberType")
                .field("namespace", &arg0.namespace)
                .field("property", &arg0.property)
                .field("span", &arg0.span)
                .finish(),
            Self::Discrete(arg0) => f
                .debug_struct("DiscreteType")
                .field("name", &arg0.name)
                .field("generic_args", &arg0.generic_args)
                .field("span", &arg0.span)
                .finish(),
            Self::This { span } => f.debug_struct("ThisType").field("span", span).finish(),
            Self::Invalid => f.debug_struct("Invalid").finish(),
        }
    }
}
