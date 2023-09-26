use whirl_ast::{BinOperator, Span};

/// A type checking error.
#[derive(Debug, PartialEq)]
pub struct TypeError {
    pub _type: TypeErrorType,
    // Affected area.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum TypeErrorType {
    /// Performing a binary operation on incompatible types.
    InvalidBinary {
        left: String,
        operator: BinOperator,
        right: String,
    },
    /// Annotating with `: invalid`.
    AssignedInvalid,
    /// Using an undeclared type.
    UnknownType {
        name: String,
    },
    /// Using a variable name in a type annotation.
    ValueAsType,
    /// Giving generic arguments to a type that is not generic.
    UnexpectedGenericArgs {
        name: String,
    },
    /// Incompatible number of generic arguments to parameters.
    MismatchedGenericArgs {
        name: String,
        expected: usize,
        assigned: usize,
    },
    /// Assigning two unassignable types.
    MismatchedAssignment {
        left: String,
        right: String,
    },
    /// Using a trait in a type expression.
    TraitAsType {
        name: String,
    },

    EnumInModelPlace {
        name: String,
    },
    TypeInModelPlace,
    InvalidNewExpression,
    ExpectedModelGotAbstract(String),
    UnconstructableModel(String),
    MismatchedModelArgs {
        name: String,
        expected: usize,
        assigned: usize,
    },
    UninferrableParameter(String),
    ConstructorAssigntoInstance(String),
    AttributeAccessOnConstructor {
        model: String,
        attribute_name: String,
    },
    ConstructorNonStaticMethodAccess {
        model_name: String,
        method_name: String,
    },
    PrivatePropertyLeak {
        model_name: String,
        property_name: String,
    },
    AccessingOnTrait {
        trait_: String,
    },
    TypeAsValue {
        type_: String,
    },
    InstanceStaticMethodAccess {
        model_name: String,
        method_name: String,
    },
}

pub fn assigned_invalid(span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::AssignedInvalid,
        span,
    }
}

pub fn invalid_binary(left: String, operator: BinOperator, right: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidBinary {
            left,
            operator,
            right,
        },
        span,
    }
}

pub fn unknown_type(name: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnknownType { name },
        span,
    }
}

pub fn value_as_type(span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ValueAsType,
        span,
    }
}

pub fn unexpected_generic_args(name: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnexpectedGenericArgs { name },
        span,
    }
}

pub fn mismatched_generics(
    name: String,
    expected: usize,
    assigned: usize,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedGenericArgs {
            name,
            expected,
            assigned,
        },
        span,
    }
}

pub fn mismatched_assignment(left: String, right: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedAssignment { left, right },
        span,
    }
}

pub fn trait_as_type(name: String, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TraitAsType { name },
        span,
    }
}

pub fn enum_in_model_place(name: String, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::EnumInModelPlace { name },
        span,
    }
}

pub fn type_in_model_place(span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TypeInModelPlace,
        span,
    }
}

pub fn expected_model_got_abstract(name: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ExpectedModelGotAbstract(name),
        span,
    }
}

pub fn invalid_new_expression(span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidNewExpression,
        span,
    }
}

pub fn unconstructable_model(name: String, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnconstructableModel(name),
        span,
    }
}

pub fn mismatched_model_args(
    name: String,
    expected: usize,
    assigned: usize,
    span: whirl_ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedModelArgs {
            name,
            expected,
            assigned,
        },
        span,
    }
}

pub fn uninferrable_parameter(name: String, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UninferrableParameter(name),
        span,
    }
}

pub fn using_constructor_as_value_in_assign(left: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ConstructorAssigntoInstance(left),
        span,
    }
}

pub fn attribute_access_on_contructor(
    model: String,
    attribute_name: String,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::AttributeAccessOnConstructor {
            model,
            attribute_name,
        },
        span,
    }
}

pub fn contructor_non_static_method_access(
    model_name: String,
    method_name: String,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::ConstructorNonStaticMethodAccess {
            model_name,
            method_name,
        },
        span,
    }
}

pub fn private_property_leak(model_name: String, property_name: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::PrivatePropertyLeak {
            model_name,
            property_name,
        },
        span,
    }
}

pub fn accessing_on_trait(trait_: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::AccessingOnTrait { trait_ },
        span,
    }
}

pub fn type_as_value(type_: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TypeAsValue { type_ },
        span,
    }
}

pub fn instance_static_method_access(
    model_name: String,
    method_name: String,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::InstanceStaticMethodAccess {
            model_name,
            method_name,
        },
        span,
    }
}
