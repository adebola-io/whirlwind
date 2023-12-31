use super::{expression_is_attribute, is_valid_lhs, typecheck_expression};
use crate::{
    span_of_typed_expression,
    typing::checker::{AttributeAssignment, ScopeType},
    unify_types,
    utils::{
        ensure_assignment_validity, get_implementation_of, infer_ahead, is_numeric_type,
        is_updateable, update_expression_type,
    },
    EvaluatedType, SemanticSymbolKind, SymbolLibrary, TypecheckerContext, TypedAssignmentExpr,
    UnifyOptions,
};
use ast::AssignOperator;
use errors::TypeError;
use std::collections::HashMap;

/// Typechecks an assignment expression.
/// todo: handle constant values.
pub fn typecheck_assignment_expression(
    assexp: &mut TypedAssignmentExpr,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) -> EvaluatedType {
    assexp.inferred_type = (|| {
        let left_type = typecheck_expression(&mut assexp.left, checker_ctx, symbollib);
        let right_type = if !is_valid_lhs(&assexp.left) {
            checker_ctx.add_diagnostic(errors::invalid_assignment_target(assexp.span));
            typecheck_expression(&mut assexp.right, checker_ctx, symbollib); // For continuity.
            return EvaluatedType::Unknown;
        } else {
            // Try to guess the type of the right.
            infer_ahead(&mut assexp.right, &left_type, symbollib);
            typecheck_expression(&mut assexp.right, checker_ctx, symbollib)
        };
        let mut generic_hashmap = HashMap::new();
        let result_type = match &left_type {
            EvaluatedType::ModelInstance { .. }
            | EvaluatedType::EnumInstance { .. }
            | EvaluatedType::Unknown
            | EvaluatedType::HardGeneric { .. }
            | EvaluatedType::Generic { .. }
            | EvaluatedType::OpaqueTypeInstance { .. }
            | EvaluatedType::FunctionExpressionInstance { .. } => {
                let mut unification = unify_types(
                    &left_type,
                    &right_type,
                    symbollib,
                    UnifyOptions::Conform,
                    Some(&mut generic_hashmap),
                );
                if is_numeric_type(&left_type, symbollib)
                    && is_updateable(&assexp.left, &symbollib)
                    && is_numeric_type(&right_type, symbollib)
                    && is_updateable(&assexp.left, &symbollib)
                {
                    unification = unification.or(unify_types(
                        &right_type,
                        &left_type,
                        symbollib,
                        UnifyOptions::Conform,
                        Some(&mut generic_hashmap),
                    ))
                }
                match unification {
                    Ok(result_type) => result_type,
                    Err(errortypes) => {
                        for _type in errortypes {
                            checker_ctx.add_diagnostic(TypeError {
                                _type,
                                span: checker_ctx.span_of_expr(&assexp.right, symbollib),
                            })
                        }
                        return EvaluatedType::Unknown;
                    }
                }
            }
            EvaluatedType::MethodInstance { method, .. } => {
                let method_symbol = symbollib.get_forwarded(*method).unwrap();
                let name = method_symbol.name.clone();
                let owner = match &method_symbol.kind {
                    SemanticSymbolKind::Method {
                        owner_model_or_interface,
                        ..
                    } => symbollib
                        .get(*owner_model_or_interface)
                        .unwrap()
                        .name
                        .clone(),
                    _ => String::from("[Model]"),
                };
                checker_ctx.add_diagnostic(errors::mutating_method(owner, name, assexp.span));
                return EvaluatedType::Unknown;
            }
            _ => {
                checker_ctx.add_diagnostic(errors::invalid_assignment_target(assexp.span));
                return EvaluatedType::Unknown;
            }
        };
        if let EvaluatedType::MethodInstance { method, .. } = right_type {
            let method_symbol = symbollib.get_forwarded(method).unwrap();
            let name = method_symbol.name.clone();
            let owner = match &method_symbol.kind {
                SemanticSymbolKind::Method {
                    owner_model_or_interface,
                    ..
                } => symbollib
                    .get(*owner_model_or_interface)
                    .unwrap()
                    .name
                    .clone(),
                _ => String::from("[Model]"),
            };
            checker_ctx.add_diagnostic(errors::mutating_method(owner, name, assexp.span));
            return EvaluatedType::Unknown;
        }
        // For other assignment types.
        if let AssignOperator::MinusAssign | AssignOperator::PlusAssign = assexp.operator {
            let interface_ = match assexp.operator {
                AssignOperator::MinusAssign => symbollib.subtract,
                AssignOperator::PlusAssign => symbollib.addition,
                _ => unreachable!(),
            };
            if interface_.is_none() {
                checker_ctx.add_diagnostic(errors::missing_intrinsic(
                    format!(
                        "{}",
                        match assexp.operator {
                            AssignOperator::MinusAssign => "Subtraction",
                            AssignOperator::PlusAssign => "Addition",
                            _ => unreachable!(),
                        }
                    ),
                    assexp.span,
                ));
            }
            let target_interface = interface_.unwrap();
            let implementation = get_implementation_of(target_interface, &result_type, symbollib);
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
                    assexp.span,
                ));
            }
        }

        // If lhs is an attribute and we are in a constructor, rigorously check for attribute assignment..
        'check_for_attribute_assignment: {
            let expression_as_attrib = expression_is_attribute(&assexp.left, symbollib);
            if !matches!(assexp.operator, ast::AssignOperator::Assign) {
                break 'check_for_attribute_assignment;
            }
            if expression_as_attrib.is_none() {
                break 'check_for_attribute_assignment;
            }
            let attribute_idx = expression_as_attrib.unwrap();
            let contructor_ctx_opt = checker_ctx.current_constructor_context.last_mut();
            if contructor_ctx_opt.is_none() {
                break 'check_for_attribute_assignment;
            }
            let constructor_ctx = contructor_ctx_opt.unwrap();
            let scopes = &constructor_ctx.scopes;
            let current_scope_type = scopes.last().clone();
            let targeted_attribute = constructor_ctx
                .attributes
                .iter_mut()
                .find(|(idx, _)| **idx == attribute_idx);
            if targeted_attribute.is_none() {
                break 'check_for_attribute_assignment;
            }
            let (_, prior_assignments) = targeted_attribute.unwrap();
            // If assignment has already been settled.
            if prior_assignments
                .iter()
                .any(|assignment| assignment.is_definite())
            {
                break 'check_for_attribute_assignment;
            }
            // First check that the attribute does not reference itself in its initialization.
            let span_of_rhs =
                span_of_typed_expression(&assexp.right, symbollib, checker_ctx.literals);
            let attribute_symbol = symbollib.get(attribute_idx).unwrap();
            if attribute_symbol
                .references
                .first()
                .unwrap()
                .starts
                .iter()
                .any(|start| span_of_rhs.contains(*start))
            {
                checker_ctx.add_diagnostic(errors::using_attribute_before_assign(span_of_rhs));
                break 'check_for_attribute_assignment;
            }
            let assignment_type = match current_scope_type {
                // If we are in an if block.
                // For the assignment to be definite, there should also be an assignment in the else block.
                Some(ScopeType::IfBlock { id }) => AttributeAssignment::InIfBlock { id: *id },
                // If we are in an else block.
                // If assignment is definitive in the if block, and there is also an assignment here,
                // then we can resolve both to produce a single assignment.
                // But the assignment is not definitive overall, unless the branching expression is within the constructor.
                Some(ScopeType::ElseBlock { id_of_parent_if }) => {
                    if prior_assignments.iter().any(|assignment| matches!(assignment, AttributeAssignment::InIfBlock { id } if id == id_of_parent_if)) {
                            // Branching assignment, but in the constructor scope.
                            if constructor_ctx.scopes.len() == 1 {
                                AttributeAssignment::Definite { span: span_of_typed_expression(&assexp.left, symbollib, checker_ctx.literals) }
                            } else {
                                // Branching assignment, but in another scope.
                                let previous_scope =scopes.get(scopes.len() -2 ).unwrap();
                                if let ScopeType::IfBlock { id } = previous_scope {
                                    AttributeAssignment::InIfBlock { id: *id }
                                } else {
                                    AttributeAssignment::SomewhereElse
                                }
                            }
                        } else {
                            AttributeAssignment::SomewhereElse
                        }
                }
                Some(_) => AttributeAssignment::SomewhereElse,
                // Still within the constructor scope.
                None => AttributeAssignment::Definite {
                    span: span_of_typed_expression(&assexp.left, symbollib, checker_ctx.literals),
                },
            };
            prior_assignments.push(assignment_type);
        }
        let span = assexp.span;
        ensure_assignment_validity(&right_type, checker_ctx, span);

        // Update value of left hand side based on the inferred type.
        let generics = generic_hashmap.into_iter().collect::<Vec<_>>();
        update_expression_type(
            &mut assexp.left,
            symbollib,
            checker_ctx.literals,
            &generics,
            Some(&result_type),
        );
        return EvaluatedType::Void;
    })();
    assexp.inferred_type.clone()
}
