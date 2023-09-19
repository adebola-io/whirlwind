use whirl_ast::{
    DiscreteType, EnumSignature, Identifier, ModelSignature, ModuleScope, ScopeAddress, ScopeEntry,
    Span, TypeEval, TypeExpression, TypeSignature,
};

use crate::TypeError;
use whirl_errors as errors;

/// Confirms that a type expression is valid in the scope it is defined, and then generate a type evaluation for it.
pub fn eval_type_expression(
    module_scope: &ModuleScope,
    expression: &TypeExpression,
    scope: usize,
) -> Result<TypeEval, TypeError> {
    // Go to scope.
    match expression {
        // Type checking discrete types.
        TypeExpression::Discrete(discrete_type) => {
            evaluate_discrete_type(module_scope, discrete_type, scope)
        }
        // TODO: disallow This type outside model context.
        TypeExpression::This { .. } => todo!(),
        TypeExpression::Invalid => Err(errors::assigned_invalid(expression.span())),
        _ => todo!(),
    }
}

/// Try to convert a discrete type to an evaluation.
pub fn evaluate_discrete_type(
    module_scope: &ModuleScope,
    discrete_type: &DiscreteType,
    scope: usize,
) -> Result<TypeEval, TypeError> {
    let shadow = module_scope.create_shadow(scope);
    let name = &discrete_type.name.name;
    let span = discrete_type.name.span;
    let generic_args = &discrete_type.generic_args;

    let lookup = shadow.lookup(name);

    // Type is not found.
    if lookup.is_none() {
        return Err(errors::unknown_type(name, span));
    }

    let search = lookup.unwrap();
    match search.entry {
        // Block using variable names as types.
        ScopeEntry::Variable(_) | ScopeEntry::Function(_) | ScopeEntry::Parameter(_) => {
            Err(errors::value_as_type(name, span))
        }
        ScopeEntry::Trait(_) => return Err(errors::trait_as_type(name, span)),
        ScopeEntry::Type(TypeSignature { generic_params, .. })
        | ScopeEntry::Model(ModelSignature { generic_params, .. })
        | ScopeEntry::Enum(EnumSignature { generic_params, .. }) => {
            // Evaluate generic arguments.
            let args = if let Some(arguments) = generic_args {
                // Confirm that generics are allowed.
                if generic_params.is_none() {
                    return Err(errors::unexpected_generic_args(name, span));
                }
                // Confirm that generics and parameters are the same length.
                let params = generic_params.as_ref().unwrap();
                let plen = params.len();
                let alen = arguments.len();
                if plen != alen {
                    return Err(errors::mismatched_generics(name, plen, alen, span));
                }
                let mut evaluated_args = vec![];
                for argument in arguments {
                    // TODO: Compute trait guards.
                    evaluated_args.push(eval_type_expression(module_scope, argument, scope)?);
                }
                Some(evaluated_args)
            } else {
                None
            };
            let address = [search.scope.id, search.index].into();
            let eval = TypeEval::Pointer { address, args };
            Ok(eval)
        }
    }
}

/// Get the type of a value in the current scope.
pub fn evaluate_type_of_variable(
    module_scope: &ModuleScope,
    variable: &Identifier,
) -> Result<TypeEval, TypeError> {
    match module_scope.lookup(&variable.name) {
        Some(value) => match value.entry {
            ScopeEntry::Function(_)
            | ScopeEntry::Type(_)
            | ScopeEntry::Enum(_)
            | ScopeEntry::Trait(_)
            | ScopeEntry::Model(_) => Ok(TypeEval::Pointer {
                address: ScopeAddress {
                    scope_id: value.scope.id,
                    entry_no: value.index,
                },
                args: None,
            }),
            ScopeEntry::Variable(v) => Ok(v.var_type.inferred.clone().unwrap_or(TypeEval::Unknown)),
            ScopeEntry::Parameter(p) => {
                Ok(p.type_label.inferred.clone().unwrap_or(TypeEval::Unknown))
            }
        },
        None => {
            return Err(errors::unknown_variable_in_scope(
                variable.name.to_owned(),
                variable.span,
            ))
        }
    }
}

/// Attempt to assign two types together.
pub fn assign_right_to_left(
    left: TypeEval,
    right: TypeEval,
    span: Span,
) -> Result<TypeEval, TypeError> {
    // Types are equal.
    if left == right {
        return Ok(left);
    }
    Err(whirl_errors::mismatched_assignment(left, right, span))
}
