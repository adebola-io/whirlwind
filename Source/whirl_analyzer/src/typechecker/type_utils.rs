use errors::{
    enum_in_model_place, expected_model_got_abstract, trait_as_type, type_in_model_place,
    value_as_type,
};
use whirl_ast::{
    ASTVisitorExprOutputNoArgs, DiscreteType, EnumSignature, Expression, Identifier,
    ModelSignature, ModuleAmbience, ScopeAddress, ScopeEntry, Span, Spannable, TypeEval,
    TypeExpression, TypeSignature,
};
use whirl_lexer::Lexer;

use crate::{TypeError, TypeInferrer};
use whirl_errors as errors;

/// Confirms that a type expression is valid in the scope it is defined, and then generate a type evaluation for it.
pub fn eval_type_expression(
    module_ambience: &ModuleAmbience,
    expression: &TypeExpression,
    scope: usize,
) -> Result<TypeEval, TypeError> {
    // Go to scope.
    match expression {
        // Type checking discrete types.
        TypeExpression::Discrete(discrete_type) => {
            evaluate_discrete_type(module_ambience, discrete_type, scope)
        }
        // TODO: disallow This type outside model context.
        TypeExpression::This { .. } => todo!(),
        TypeExpression::Invalid => Err(errors::assigned_invalid(expression.span())),
        _ => todo!(),
    }
}

/// Try to convert a discrete type to an evaluation.
pub fn evaluate_discrete_type(
    module_ambience: &ModuleAmbience,
    discrete_type: &DiscreteType,
    scope: usize,
) -> Result<TypeEval, TypeError> {
    let shadow = module_ambience.create_shadow(scope);
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
            Err(errors::value_as_type(span))
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
                    evaluated_args.push(eval_type_expression(module_ambience, argument, scope)?);
                }
                Some(evaluated_args)
            } else {
                None
            };
            let address = [search.scope.id, search.index].into();
            let eval = TypeEval::Instance { address, args };
            Ok(eval)
        }
    }
}

/// Get the type of a value in the current scope.
pub fn evaluate_type_of_variable(
    module_ambience: &ModuleAmbience,
    variable: &Identifier,
) -> Result<TypeEval, TypeError> {
    match module_ambience.lookup(&variable.name) {
        Some(value) => {
            let address: ScopeAddress = [value.scope.id, value.index].into();
            match value.entry {
                ScopeEntry::Type(_) => Ok(TypeEval::TypeAlias { address }),
                ScopeEntry::Enum(_) => Ok(TypeEval::EnumConstructor { address }),
                ScopeEntry::Trait(_) => Ok(TypeEval::TraitConstructor { address }),
                ScopeEntry::Model(_) => Ok(TypeEval::ModelConstructor { address }),
                ScopeEntry::Function(_) => Ok(TypeEval::Instance {
                    address,
                    // todo: function generics.
                    args: None,
                }),
                ScopeEntry::Variable(v) => {
                    Ok(v.var_type.inferred.clone().unwrap_or(TypeEval::Unknown))
                }
                ScopeEntry::Parameter(p) => {
                    Ok(p.type_label.inferred.clone().unwrap_or(TypeEval::Unknown))
                }
            }
        }
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

/// Check if an instance of the type can be created.
pub fn construct_type(
    typeval: TypeEval,
    ambience: &ModuleAmbience,
    span: Span,
) -> Result<ScopeAddress, TypeError> {
    match &typeval {
        TypeEval::Instance { .. } => Err(value_as_type(span)),
        TypeEval::Invalid | TypeEval::Unknown => {
            return Err(expected_model_got_abstract(typeval, span))
        }
        TypeEval::ModelConstructor { address } => Ok(*address),
        TypeEval::TraitConstructor { address } => Err(trait_as_type(
            ambience.get_entry_unguarded(*address).name(),
            span,
        )),
        TypeEval::EnumConstructor { address } => Err(enum_in_model_place(
            ambience.get_entry_unguarded(*address).name(),
            span,
        )),
        TypeEval::TypeAlias { .. } => Err(type_in_model_place(span)),
    }
}

pub fn build_model_from_call_expression<L: Lexer>(
    caller_type: TypeEval,
    arguments: &Vec<Expression>,
    inferrer: &TypeInferrer<L>,
    span: Span,
) -> Result<TypeEval, TypeError> {
    let ambience = inferrer.module_ambience();
    let address = construct_type(caller_type, ambience, span)?;
    let entry = ambience.get_entry_unguarded(address).model();
    let name = ambience.get_entry_unguarded(address).name();
    // Confirm constructor.
    match &entry.parameters {
        None => return Err(whirl_errors::unconstructable_model(name.to_string(), span)),
        Some(params) => {
            // Confirm arguments.
            // TODO: Generics.
            if arguments.len() != params.len() {
                return Err(whirl_errors::mismatched_model_args(
                    name.to_string(),
                    params.len(),
                    arguments.len(),
                    span,
                ));
            }
            // Compare parameters and arguments.
            for (index, argument) in arguments.iter().enumerate() {
                let inferred_argument_type = inferrer.expr(argument);
                if let Some(inferred_param_type) = &params[index].type_label.inferred {
                    if &inferred_argument_type != inferred_param_type {
                        return Err(errors::mismatched_assignment(
                            inferred_param_type.clone(),
                            inferred_argument_type,
                            argument.span(),
                        ));
                    }
                }
            }
            return Ok(TypeEval::Instance {
                address,
                args: None,
            });
        }
    }
}
