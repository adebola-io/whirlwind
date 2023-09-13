use whirl_ast::{BinOperator, Span};

use super::TypeEval;

/// A type checking error.
#[derive(Debug, PartialEq)]
pub struct TypeError {
    pub _type: TypeErrorType,
    // Affected areas.
    pub spans: Vec<Span>,
}

#[derive(Debug, PartialEq)]
pub enum TypeErrorType {
    // Performing a binary operation on incompatible types.
    InvalidBinary {
        left: TypeEval,
        operator: BinOperator,
        right: TypeEval,
    },
    // Annotating with `: invalid`.
    AssignedInvalid,
    /// Using an undeclared type.
    UnknownType {
        name: String,
    },
    /// Using a variable name in a type annotation.
    ValueAsType {
        name: String,
    },
}

pub(crate) fn assigned_invalid(span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::AssignedInvalid,
        spans: vec![span],
    }
}

pub(crate) fn invalid_binary(
    left: TypeEval,
    operator: BinOperator,
    right: TypeEval,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidBinary {
            left,
            operator,
            right,
        },
        spans: vec![span],
    }
}

pub(crate) fn unknown_type(name: &str, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnknownType {
            name: name.to_owned(),
        },
        spans: vec![span],
    }
}

pub(crate) fn value_as_type(name: &str, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ValueAsType {
            name: name.to_owned(),
        },
        spans: vec![span],
    }
}
