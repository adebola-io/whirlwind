use whirl_ast::{
    ClassSignature, DiscreteType, EnumSignature, ScopeEntry, ScopeManager, TypeEval,
    TypeExpression, TypeSignature,
};

use crate::TypeError;

use super::errors;

/// Confirms that a type expression is valid in the scope it is defined, and then generate a type evaluation for it.
pub fn eval_type_expression(
    scope_manager: &ScopeManager,
    expression: &TypeExpression,
    scope: usize,
) -> Result<TypeEval, TypeError> {
    // Go to scope.
    match expression {
        // Type checking discrete types.
        TypeExpression::Discrete(discrete_type) => {
            eval_discrete_type(scope_manager, discrete_type, scope)
        }
        // TODO: disallow This type outside class context.
        TypeExpression::This { .. } => todo!(),
        TypeExpression::Invalid => Err(errors::assigned_invalid(expression.span())),
        _ => todo!(),
    }
}

/// Try to convert a discrete type to an evaluation.
pub fn eval_discrete_type(
    scope_manager: &ScopeManager,
    discrete_type: &DiscreteType,
    scope: usize,
) -> Result<TypeEval, TypeError> {
    let shadow = scope_manager.create_shadow(scope);
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
        ScopeEntry::Variable(_) | ScopeEntry::Function(_) => Err(errors::value_as_type(name, span)),
        ScopeEntry::Type(TypeSignature { generic_params, .. })
        | ScopeEntry::Class(ClassSignature { generic_params, .. })
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
                    evaluated_args.push(eval_type_expression(scope_manager, argument, scope)?);
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
