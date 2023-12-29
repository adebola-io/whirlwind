use std::collections::HashMap;

use super::typecheck_expression;
use crate::{
    unify_types,
    utils::{
        boolean_instance, get_implementation_of, is_numeric_type, rangify, update_expression_type,
    },
    EvaluatedType, SymbolLibrary, TypecheckerContext, UnifyOptions,
};
use ast::BinOperator;
use errors::TypeError;

///  Typechecks a binary expression.
pub fn typecheck_binary_expression(
    binexp: &mut crate::TypedBinExpr,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    binexp.inferred_type = (|| {
        let left = typecheck_expression(&mut binexp.left, checker_ctx, symbollib);
        let right = typecheck_expression(&mut binexp.right, checker_ctx, symbollib);

        match binexp.operator {
            // equality operations.
            // valid if a and b refer to the same base type.
            BinOperator::Equals | BinOperator::NotEquals => {
                converge_binary_types(symbollib, left, right, binexp, checker_ctx);
                match symbollib.bool {
                    Some(bool) => boolean_instance(bool),
                    None => {
                        checker_ctx.add_diagnostic(errors::missing_intrinsic(
                            format!("Bool"),
                            binexp.span,
                        ));
                        EvaluatedType::Unknown
                    }
                }
            }
            // ordering operations.
            // valid if both a and b are unifiable, and they both implement
            // Orderable.
            BinOperator::LessThan
            | BinOperator::GreaterThan
            | BinOperator::LessThanOrEquals
            | BinOperator::GreaterThanOrEquals => {
                let result_type =
                    converge_binary_types(symbollib, left, right, binexp, checker_ctx);
                if symbollib.orderable.is_none() {
                    checker_ctx.add_diagnostic(errors::missing_intrinsic(
                        format!("Orderable"),
                        binexp.span,
                    ));
                    return EvaluatedType::Unknown;
                }
                let implementation =
                    get_implementation_of(symbollib.orderable.unwrap(), &result_type, symbollib);
                if implementation.is_none() && !result_type.is_unknown() {
                    let result_type = symbollib.format_evaluated_type(&result_type);
                    checker_ctx.add_diagnostic(errors::not_orderable(
                        binexp.operator,
                        result_type,
                        binexp.span,
                    ));
                    EvaluatedType::Unknown
                } else {
                    match symbollib.bool {
                        Some(bool) => boolean_instance(bool),
                        None => {
                            checker_ctx.add_diagnostic(errors::missing_intrinsic(
                                format!("Bool"),
                                binexp.span,
                            ));
                            EvaluatedType::Unknown
                        }
                    }
                }
            }
            // range operation.
            // valid if a and b are unifiable, and they both implement Sequenced.
            BinOperator::Range => {
                let result_type =
                    converge_binary_types(symbollib, left, right, binexp, checker_ctx);
                if symbollib.sequenced.is_none() {
                    checker_ctx.add_diagnostic(errors::missing_intrinsic(
                        format!("Sequenced"),
                        binexp.span,
                    ));
                    return EvaluatedType::Unknown;
                }
                let implementation =
                    get_implementation_of(symbollib.sequenced.unwrap(), &result_type, symbollib);
                if implementation.is_none() && !result_type.is_unknown() {
                    let result_type = symbollib.format_evaluated_type(&result_type);
                    checker_ctx.add_diagnostic(errors::not_sequenced(result_type, binexp.span));
                    EvaluatedType::Unknown
                } else {
                    if symbollib.range.is_some() {
                        return rangify(result_type, &symbollib);
                    }
                    checker_ctx
                        .add_diagnostic(errors::missing_intrinsic(format!("Range"), binexp.span));
                    EvaluatedType::Unknown
                }
            }
            // % and ^ operations.
            // can only work on numbers.
            BinOperator::PowerOf | BinOperator::Remainder => {
                let lef = left.clone();
                let result_type =
                    converge_binary_types(symbollib, left, right, binexp, checker_ctx);
                if !is_numeric_type(&result_type, symbollib) {
                    let result_type = symbollib.format_evaluated_type(&result_type);
                    checker_ctx.add_diagnostic(errors::numeric_exclusive_operation(
                        result_type,
                        binexp.span,
                    ));
                    return EvaluatedType::Unknown;
                }
                if matches!(binexp.operator, BinOperator::Remainder) {
                    return lef.clone();
                }
                return result_type;
                // if result_type.
            }
            // the rest.
            BinOperator::Multiply
            | BinOperator::Add
            | BinOperator::Subtract
            | BinOperator::Divide
            | BinOperator::BitAnd
            | BinOperator::BitOr
            | BinOperator::LeftShift
            | BinOperator::RightShift => {
                let result_type =
                    converge_binary_types(symbollib, left, right, binexp, checker_ctx);
                let interface_ = match binexp.operator {
                    BinOperator::Add => symbollib.addition,
                    BinOperator::Multiply => symbollib.multiply,
                    BinOperator::Subtract => symbollib.subtract,
                    BinOperator::Divide => symbollib.divide,
                    _ => symbollib.bitwise,
                };
                if interface_.is_none() {
                    checker_ctx.add_diagnostic(errors::missing_intrinsic(
                        format!(
                            "{}",
                            match binexp.operator {
                                BinOperator::Add => "Addition",
                                BinOperator::Multiply => "Multiplication",
                                BinOperator::Subtract => "Subtraction",
                                BinOperator::Divide => "Division",
                                _ => "Bitwise",
                            }
                        ),
                        binexp.span,
                    ));
                    return EvaluatedType::Unknown;
                }
                let target_interface = interface_.unwrap();
                let implementation =
                    get_implementation_of(target_interface, &result_type, symbollib);
                if implementation.is_none() && !result_type.is_unknown() {
                    let result_type = symbollib.format_evaluated_type(&result_type);
                    let interface =
                        symbollib.format_evaluated_type(&EvaluatedType::InterfaceInstance {
                            interface_: target_interface,
                            is_invariant: false,
                            generic_arguments: vec![],
                        });
                    checker_ctx.add_diagnostic(errors::unimplemented_interface(
                        result_type,
                        interface,
                        binexp.span,
                    ));
                    EvaluatedType::Unknown
                } else {
                    return result_type;
                }
            } // _ => EvaluatedType::Unknown,
        }
    })();
    binexp.inferred_type.clone()
}

/// Checks two types to confirm that they are unifiable in either direction.
fn converge_binary_types(
    symbollib: &mut SymbolLibrary,
    left: EvaluatedType,
    right: EvaluatedType,
    binexp: &mut crate::TypedBinExpr,
    checker_ctx: &mut TypecheckerContext<'_>,
) -> EvaluatedType {
    let mut generic_hashmap = HashMap::new();
    let mut unifier = |left, right| {
        unify_types(
            left,
            right,
            symbollib,
            UnifyOptions::Conform,
            Some(&mut generic_hashmap),
        )
    };
    let mut unification = unifier(&left, &right);
    let is_numeric = is_numeric_type(&left, symbollib) && is_numeric_type(&right, symbollib);
    if is_numeric {
        unification = unification.or(unifier(&right, &left));
    };
    match unification {
        Ok(result_type) => {
            let generic_arguments = generic_hashmap.into_iter().collect::<Vec<_>>();
            update_expression_type(
                &mut binexp.left,
                symbollib,
                &generic_arguments,
                Some(&result_type),
            );
            update_expression_type(
                &mut binexp.right,
                symbollib,
                &generic_arguments,
                Some(&result_type),
            );
            result_type
        }
        Err(error_types) => {
            let left = symbollib.format_evaluated_type(&left);
            let right = symbollib.format_evaluated_type(&right);
            for error_type in error_types {
                let error = TypeError {
                    _type: error_type,
                    span: binexp.span,
                };
                checker_ctx.add_diagnostic(error);
            }
            checker_ctx.add_diagnostic(errors::incomparable(left, right, binexp.span));
            EvaluatedType::Unknown
        }
    }
}
