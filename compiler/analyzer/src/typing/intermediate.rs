use ast::{LogicOperator, Span};

use crate::{EvaluatedType, SymbolIndex};

#[derive(Debug, PartialEq, Clone)]
pub struct IntermediateTypeProperty {
    pub actual: Option<SymbolIndex>,
    pub name: String,
    pub generic_args: Vec<IntermediateType>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IntermediateType {
    FunctionType {
        params: Vec<ParameterType>,
        return_type: Option<Box<IntermediateType>>,
        span: Span,
    },
    MemberType {
        object: Box<IntermediateType>,
        property: IntermediateTypeProperty,
        span: Span,
    },
    SimpleType {
        value: SymbolIndex,
        generic_args: Vec<IntermediateType>,
        span: Span,
    },
    UnionType {
        types: Vec<IntermediateType>,
        span: Span,
    },
    This {
        meaning: Option<SymbolIndex>,
        span: Span,
    },
    Placeholder,
    ArrayType {
        element_type: Box<IntermediateType>,
        span: Span,
    },
    MaybeType {
        value: Box<IntermediateType>,
        span: Span,
    },
    TernaryType {
        clause: Box<IntermediateTypeClause>,
        consequent: Box<IntermediateType>,
        alternate: Box<IntermediateType>,
        span: Span,
    },
    BoundConstraintType {
        consequent: Box<IntermediateType>,
        clause: Box<IntermediateTypeClause>,
        span: Span,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntermediateTypeClause {
    Binary {
        left: Box<IntermediateTypeClause>,
        operator: LogicOperator,
        right: Box<IntermediateTypeClause>,
    },
    Is {
        base: SymbolIndex,
        other: IntermediateType,
    },
    Implements {
        base: SymbolIndex,
        interfaces: Vec<IntermediateType>,
    },
}

impl PartialEq for IntermediateType {
    /// Checks if two types are equal without checking their spans
    /// It is useful when I want to compare the constraint on an implementation and a
    /// constraint on a method it defines for a model.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::FunctionType {
                    params: l_params,
                    return_type: l_return_type,
                    ..
                },
                Self::FunctionType {
                    params: r_params,
                    return_type: r_return_type,
                    ..
                },
            ) => l_params == r_params && l_return_type == r_return_type,
            (
                Self::MemberType {
                    object: l_object,
                    property: l_property,
                    ..
                },
                Self::MemberType {
                    object: r_object,
                    property: r_property,
                    ..
                },
            ) => l_object == r_object && l_property == r_property,
            (
                Self::SimpleType {
                    value: l_value,
                    generic_args: l_generic_args,
                    ..
                },
                Self::SimpleType {
                    value: r_value,
                    generic_args: r_generic_args,
                    ..
                },
            ) => l_value == r_value && l_generic_args == r_generic_args,
            (Self::UnionType { types: l_types, .. }, Self::UnionType { types: r_types, .. }) => {
                l_types == r_types
            }
            (
                Self::This {
                    meaning: l_meaning, ..
                },
                Self::This {
                    meaning: r_meaning, ..
                },
            ) => l_meaning == r_meaning,
            (
                Self::ArrayType {
                    element_type: l_element_type,
                    ..
                },
                Self::ArrayType {
                    element_type: r_element_type,
                    ..
                },
            ) => l_element_type == r_element_type,
            (Self::MaybeType { value: l_value, .. }, Self::MaybeType { value: r_value, .. }) => {
                l_value == r_value
            }
            (
                Self::TernaryType {
                    clause: l_clause,
                    consequent: l_consequent,
                    alternate: l_alternate,
                    ..
                },
                Self::TernaryType {
                    clause: r_clause,
                    consequent: r_consequent,
                    alternate: r_alternate,
                    ..
                },
            ) => l_clause == r_clause && l_consequent == r_consequent && l_alternate == r_alternate,
            (
                Self::BoundConstraintType {
                    consequent: l_consequent,
                    clause: l_clause,
                    ..
                },
                Self::BoundConstraintType {
                    consequent: r_consequent,
                    clause: r_clause,
                    ..
                },
            ) => l_consequent == r_consequent && l_clause == r_clause,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl IntermediateType {
    pub fn span(&self) -> Span {
        match self {
            IntermediateType::FunctionType { span, .. }
            | IntermediateType::MemberType { span, .. }
            | IntermediateType::SimpleType { span, .. }
            | IntermediateType::UnionType { span, .. }
            | IntermediateType::This { span, .. }
            | IntermediateType::ArrayType { span, .. }
            | IntermediateType::MaybeType { span, .. }
            | IntermediateType::TernaryType { span, .. }
            | IntermediateType::BoundConstraintType { span, .. } => *span,
            IntermediateType::Placeholder => Span::default(),
        }
    }

    pub(crate) fn is_ternary(&self) -> bool {
        matches!(self, IntermediateType::TernaryType { .. })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParameterType {
    pub name: String,
    pub is_optional: bool,
    pub type_label: Option<IntermediateType>,
    pub inferred_type: EvaluatedType,
}
