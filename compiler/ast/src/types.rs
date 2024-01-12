use crate::{Identifier, LogicOperator, Parameter, ScopeAddress, Span};

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

#[derive(PartialEq, Debug, Default, Clone, Hash)]
pub enum TypeExpression {
    Union(UnionType),
    Functional(FunctionalType),
    Member(MemberType),
    Discrete(DiscreteType),
    This {
        span: Span,
    },
    Array(ArrayType),
    #[default]
    Invalid,
    Optional(MaybeType),
    Ternary(TernaryType),
    Constraint(BoundConstraintType),
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

/// A type that is part of an member expression. e.g. `type Error = core.io.Error`;
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

/// A type that can only be used when a certain condition is true.
/// e.g. type Name<T> = Value<T>|=T implements Default;
#[derive(PartialEq, Debug, Clone, Hash)]
pub struct BoundConstraintType {
    pub consequent: DiscreteType,
    pub clause: Box<TypeClause>,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum TypeClause {
    Binary {
        left: Box<TypeClause>,
        operator: LogicOperator,
        right: Box<TypeClause>,
    },
    Implementations {
        base: Identifier,
        interfaces: Vec<TypeExpression>,
    },
    Is {
        base: Identifier,
        other: TypeExpression,
    },
}

impl TypeClause {
    /// Returns `true` if the type clause is [`Binary`].
    ///
    /// [`Binary`]: TypeClause::Binary
    #[must_use]
    pub fn is_binary(&self) -> bool {
        matches!(self, Self::Binary { .. })
    }
}

/// A shorthand for a list type. e.g. `type Bytes = []Uint8;`
#[derive(Debug, PartialEq, Clone, Hash)]
pub struct ArrayType {
    pub element_type: Box<TypeExpression>,
    pub span: Span,
}

/// A shorthand for a maybe type. e.g. `type MaybeString = ?String`
#[derive(Debug, PartialEq, Clone, Hash)]
pub struct MaybeType {
    pub value: Box<TypeExpression>,
    pub span: Span,
}

/// A type that depends on the value of other type parameters.
/// e.g. `type StringOrBool<T> = if T implements Default String else Bool;`
#[derive(Debug, PartialEq, Clone, Hash)]
pub struct TernaryType {
    pub clause: Box<TypeClause>,
    pub consequent: Box<TypeExpression>,
    pub alternate: Box<TypeExpression>,
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

            TypeExpression::Array(a) => a.span,
            TypeExpression::Optional(o) => o.span,
            TypeExpression::Ternary(c) => c.span,
            TypeExpression::Invalid => Span::default(),
            TypeExpression::Constraint(b) => b.span,
        }
    }
}
