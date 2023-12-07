use crate::{Identifier, Parameter, ScopeAddress, Span};

/// Stores the address of a symbol in relation to an entire workspace.
#[derive(Debug, PartialEq)]
pub struct ModuleAddress {
    pub module_id: usize,
    pub scope_address: ScopeAddress,
}

// /// Result of a type evaluation.
// #[derive(Default, PartialEq, Debug, Clone)]
// pub enum TypeEval {
//     /// An address of a scope entry within the module ambience.
//     ModelInstance {
//         model_address: SymbolAddress,
//         args: Option<Vec<TypeEval>>,
//     },
//     /// A type error.
//     #[default]
//     Invalid,
//     Unknown,
//     ModelConstructor {
//         address: SymbolAddress,
//     },
//     InterfaceConstructor {
//         address: SymbolAddress,
//     },
//     EnumConstructor {
//         address: SymbolAddress,
//     },
//     TypeAlias {
//         address: SymbolAddress,
//     },
//     MethodOfInstance {
//         model_address: SymbolAddress,
//         method_no: usize,
//         generic_args: Option<Vec<TypeEval>>,
//     },
// }
// impl TypeEval {
//     pub fn is_invalid(&self) -> bool {
//         matches!(self, TypeEval::Invalid)
//     }
//     /// returns true if one tyoe is an instance of the other.
//     pub fn is_instance_of(&self, other: TypeEval) -> bool {
//         matches!(self, TypeEval::ModelInstance { model_address: a1, .. }
//             if matches!(other, TypeEval::ModelConstructor { address: a2, .. }
//                 if *a1 == a2
//             )
//         )
//     }
// }

// pub trait TypedValue {
//     /// Returns the type evaluated for the value.
//     fn evaluated_type(&self) -> Option<&TypeEval>;
//     /// Returns the type declared for the value.
//     fn declared_type(&self) -> Option<&TypeExpression>;
// }

#[derive(PartialEq, Default, Clone, Hash)]
pub enum TypeExpression {
    Union(UnionType),
    Functional(FunctionalType),
    Member(MemberType),
    Discrete(DiscreteType),
    This {
        span: Span,
    },
    BorrowedType(BorrowedType),
    #[default]
    Invalid,
}

/// A union type e.g.
/// ```wrl
/// type Animal = Cat | Dog | Parrot;
/// ```
#[derive(Debug, PartialEq, Clone, Hash)]
pub struct UnionType {
    pub types: Vec<TypeExpression>,
    pub span: Span,
}

/// A function type e.g. `type Predicate<T> = fn(value: T): Boolean;`
#[derive(Debug, PartialEq, Clone, Hash)]
pub struct FunctionalType {
    pub params: Vec<Parameter>,
    pub generic_params: Option<Vec<GenericParameter>>,
    pub return_type: Option<Box<TypeExpression>>,
    pub span: Span,
}

/// A type that is part of an member expression. e.g. `type Error = Core.Io.Error`;
#[derive(Debug, PartialEq, Clone, Hash)]
pub struct MemberType {
    pub namespace: Box<TypeExpression>,
    pub property: Box<TypeExpression>,
    pub span: Span,
}

/// A simple type. e.g. `type SignalValue = Number;`
#[derive(Debug, PartialEq, Clone, Hash)]
pub struct DiscreteType {
    pub name: Identifier,
    pub generic_args: Option<Vec<TypeExpression>>,
    pub span: Span,
}

/// A reference to a value of a type. e.g. `type Ref = &SomeRef;`
#[derive(Debug, PartialEq, Clone, Hash)]
pub struct BorrowedType {
    pub value: Box<TypeExpression>,
    pub span: Span,
}

impl std::fmt::Display for TypeExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// A type parameter on a function, model or type.
#[derive(Debug, PartialEq, Clone, Hash)]
pub struct GenericParameter {
    pub name: Identifier,
    pub interfaces: Vec<TypeExpression>,
    pub default: Option<TypeExpression>,
    pub span: Span,
}

impl TypeExpression {
    pub fn span(&self) -> Span {
        match self {
            TypeExpression::Union(u) => u.span,
            TypeExpression::Functional(f) => f.span,
            TypeExpression::Member(m) => m.span,
            TypeExpression::Discrete(d) => d.span,
            TypeExpression::This { span } => span.clone(),
            TypeExpression::BorrowedType(b) => b.span,
            TypeExpression::Invalid => Span::default(),
        }
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
            Self::BorrowedType(arg0) => f
                .debug_struct("BorrowedType")
                .field("value", &arg0.value)
                .field("span", &arg0.span)
                .finish(),
        }
    }
}
