use crate::{HoverFormatter, Identifier, Parameter, ScopeAddress, Span};

/// Stores the address of a symbol in relation to an entire workspace.
#[derive(Debug)]
pub struct ModuleAddress {
    pub module_id: usize,
    pub scope_address: ScopeAddress,
}

/// The first representation of a type.
#[derive(Debug)]
pub struct Type {
    pub declared: Option<TypeExpression>,
    pub inferred: Option<ModuleAddress>,
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

pub enum TypeExpression {
    Union(UnionType),
    Functional(FunctionalType),
    Member(MemberType),
    Discrete(DiscreteType),
    This { span: Span },
}

/// A complex union type e.g. `type Animal = Cat | Dog | Parrot; `
#[derive(Debug)]
pub struct UnionType {
    pub types: Vec<TypeExpression>,
    pub span: Span,
}

/// A function type e.g. `type Predicate<T> = fn(value: T): Boolean;`
#[derive(Debug)]
pub struct FunctionalType {
    pub params: Vec<Parameter>,
    pub generic_params: Option<Vec<GenericParameter>>,
    pub return_type: Option<Box<TypeExpression>>,
    pub span: Span,
}

/// A type that is part of an member expression. e.g. `type Error = Core.Io.Error`;
#[derive(Debug)]
pub struct MemberType {
    pub namespace: Box<TypeExpression>,
    pub property: Box<TypeExpression>,
    pub span: Span,
}

/// A simple type. e.g. `type SignalValue = Number;`
#[derive(Debug)]
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

/// A type parameter on a function, class or type.
#[derive(Debug)]
pub struct GenericParameter {
    pub name: Identifier,
    pub traits: Vec<TypeExpression>,
    pub default: TypeExpression,
}

impl TypeExpression {
    pub fn span(&self) -> Span {
        match self {
            TypeExpression::Union(u) => u.span,
            TypeExpression::Functional(f) => f.span,
            TypeExpression::Member(m) => m.span,
            TypeExpression::Discrete(d) => d.span,
            TypeExpression::This { span } => span.clone(),
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
        }
    }
}

impl HoverFormatter for TypeExpression {
    fn to_formatted(&self) -> String {
        match self {
            TypeExpression::Union(union) => {
                let types = &union.types;

                let mut string = String::new();

                for (index, typeexp) in types.iter().enumerate() {
                    string.push_str(&typeexp.to_formatted());
                    if index + 1 < types.len() {
                        string.push_str(" | ");
                    }
                    // Show at most 5 types + the last one.
                    if index == 4 && types.len() > 6 {
                        let len = types.len();
                        string.push_str("... ");
                        string.push_str(&(len - 6).to_string());
                        string.push_str(" more ... | ");
                        string.push_str(&types.last().unwrap().to_formatted())
                    }
                }

                string
            }
            TypeExpression::Functional(function) => {
                let mut string = String::from("fn(");

                for (i, param) in function.params.iter().enumerate() {
                    string.push_str(&param.to_formatted());
                    if i + 1 < function.params.len() {
                        string.push_str(", ")
                    }
                }

                string.push(')');

                if let Some(ref rettype) = function.return_type {
                    string.push_str(": ");
                    string.push_str(&rettype.to_formatted())
                }

                string
            }
            TypeExpression::Member(member) => {
                let mut string = member.namespace.to_formatted();
                string.push_str(&member.property.to_formatted());
                string
            }
            TypeExpression::Discrete(discrete) => {
                let mut string = discrete.name.name.to_owned();

                if let Some(ref generic_args) = discrete.generic_args {
                    string.push('<');
                    for (index, genarg) in generic_args.iter().enumerate() {
                        string.push_str(&genarg.to_formatted());
                        if index + 1 < generic_args.len() {
                            string.push_str(", ")
                        }
                    }
                    string.push('>');
                }

                string
            }
            TypeExpression::This { .. } => format!("This"),
        }
    }
}
