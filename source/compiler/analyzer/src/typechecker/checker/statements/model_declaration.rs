use std::ops::ControlFlow;

use super::{expressions::typecheck_block, *};
use crate::{
    evaluate_and_ignore_constraint, utils::distill_as_function_type, IntermediateType,
    IntermediateTypeClause, TypedModelDeclaration, TypedModelPropertyType,
};
use ast::{unwrap_or_continue, unwrap_or_return};

/// Typechecks a model declaration.
pub fn typecheck_model_declaration(
    model: &mut TypedModelDeclaration,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
) {
    let model_symbol = symbollib.get(model.name);
    if model_symbol.is_none() {
        return;
    }
    // Signifies to the checker context that we are now in model X, so private properties can be used.
    let former_enclosing = checker_ctx.enclosing_model_or_interface.take();
    checker_ctx.enclosing_model_or_interface = Some(model.name);
    if let ControlFlow::Break(_) = typecheck_model_constructor(model, symbollib, checker_ctx) {
        return;
    }

    // Check that the method names inherited from interfaces do not clash with other.
    let model_symbol = symbollib.get(model.name).unwrap();
    if let SemanticSymbolKind::Model {
        interfaces: implementations,
        generic_params,
        methods,
        ..
    } = &model_symbol.kind
    {
        typecheck_generic_params(generic_params, symbollib, checker_ctx);
        let model_methods = methods
            .iter()
            .filter_map(|method_idx| {
                let symbol = symbollib.get(*method_idx)?;
                return Some((*method_idx, symbol.name.as_str()));
            })
            .collect::<Vec<_>>();
        let model_evaluated_type = EvaluatedType::ModelInstance {
            model: model.name,
            is_invariant: false,
            generic_arguments: evaluate_generic_params(generic_params, true),
        };

        // This vector keeps track of each interface generics and their assigned solutions.
        // At the beginning it contains the transformation of the "This" type to refer to the model,
        // rather than an interface.
        let mut generics = vec![];
        let implementation_methods = get_all_implementation_methods(
            implementations,
            symbollib,
            checker_ctx,
            &mut generics,
            &model_evaluated_type,
            model_symbol,
        );
        // The vector keeps track of method names in the model or other interfaces that have already been
        // resolved to mean something, blocking methods from other interfaces that have the same name.
        let mut checked_methods = vec![];
        for (interface, method, constraint) in implementation_methods {
            let interface_symbol = unwrap_or_continue!(symbollib.get(interface));
            let interface_method_symbol = unwrap_or_continue!(symbollib.get(method));
            if let SemanticSymbolKind::Method {
                is_public: interface_method_is_public,
                is_static: interface_method_is_static,
                is_virtual: interface_method_is_virtual,
                generic_params: interface_method_generic_params,
                ..
            } = &interface_method_symbol.kind
            {
                let interface_method_name = interface_method_symbol.name.as_str();
                let duet = (interface_symbol.name.as_str(), interface_method_name);
                // Find its implementation (or override) in the model body.
                // If the method is virtual and it is also not implemented, error.
                // If the method is implemented, then the signature
                // of its interface method must match its override.
                if let Some(previous) = checked_methods
                    .iter()
                    .find(|(_, method_name)| method_name == &interface_method_name)
                {
                    checker_ctx.add_error(errors::conflicting_implementations(
                        *previous,
                        duet,
                        model_symbol.ident_span(),
                    ));
                    continue;
                }
                checked_methods.push(duet);
                let method_impl = model_methods
                    .iter()
                    .find(|(_, method_name)| *method_name == interface_method_symbol.name);
                let method_impl = match method_impl {
                    Some(method_impl) => method_impl,
                    None => {
                        if *interface_method_is_virtual {
                            // For numeric types, the interface implementations are built in.
                            if !is_numeric_type(&model_evaluated_type, symbollib) {
                                checker_ctx.add_error(errors::missing_implementation(
                                    &interface_symbol.name,
                                    interface_method_name,
                                    model_symbol.ident_span(),
                                ));
                            }
                        }
                        continue;
                    }
                };
                // Compare signatures.
                // The implemented method should have the same access, must have equal
                // public access, static nature, param signatures, return signature and
                // generic parameters.
                let model_method_symbol = unwrap_or_continue!(symbollib.get(method_impl.0));
                let (
                    model_method_generic_params,
                    model_method_is_public,
                    model_method_is_static,
                    model_constraint,
                ) = match &model_method_symbol.kind {
                    SemanticSymbolKind::Method {
                        generic_params,
                        is_static,
                        is_public,
                        constraint,
                        ..
                    } => (generic_params, is_public, is_static, constraint),
                    _ => continue,
                };

                let span = model_method_symbol.origin_span;
                // Compare generic parameters.
                let model_method_generic_param_len = model_method_generic_params.len();
                let interface_method_generic_param_len = interface_method_generic_params.len();
                if model_method_generic_param_len != interface_method_generic_param_len {
                    let method_name = interface_method_name.to_owned();
                    checker_ctx.add_error(errors::mismatched_generic_params(
                        method_name,
                        interface_method_generic_param_len,
                        model_method_generic_param_len,
                        span,
                    ));
                }
                // Compare markers.
                if model_method_is_public != interface_method_is_public {
                    let method_name =
                        format!("{}.{}", interface_symbol.name, interface_method_symbol.name);
                    checker_ctx.add_error(errors::mismatched_method_access(
                        method_name,
                        *model_method_is_public,
                        *interface_method_is_public,
                        span,
                    ))
                }
                if model_method_is_static != interface_method_is_static {
                    let method_name =
                        format!("{}.{}", interface_symbol.name, interface_method_symbol.name);
                    checker_ctx.add_error(errors::mismatched_method_static(
                        method_name,
                        *interface_method_is_static,
                        *model_method_is_static,
                        span,
                    ))
                }
                // Compare constraints.
                // The constraints on the interface implementation must appear as well in every method.
                if let Some(interface_constraint) = constraint {
                    match model_constraint {
                        Some((model_constraint, _)) => {
                            if interface_constraint != model_constraint {
                                checker_ctx.add_error(errors::mismatched_constraint(span))
                            }
                        }
                        None => checker_ctx.add_error(errors::missing_constraint(span)),
                    }
                } else if model_constraint.is_some() {
                    checker_ctx.add_error(errors::unexpected_constraint(span));
                }
                // Compare signatures.
                // They both have to be converted to function expression instances so
                // the params and return types can be coerced.
                let interface_method_type = EvaluatedType::MethodInstance {
                    method,
                    is_invariant: false,
                    generic_arguments: evaluate_generic_params(
                        interface_method_generic_params,
                        true,
                    ),
                };
                let function_type =
                    distill_as_function_type(&interface_method_type, symbollib).unwrap();
                let interface_method_type = EvaluatedType::FunctionExpressionInstance {
                    is_async: function_type.is_async,
                    is_invariant: false,
                    params: function_type.parameter_types,
                    return_type: Box::new(function_type.return_type),
                    generic_args: function_type.generic_arguments.to_vec(),
                };
                let expected_method_type = coerce(interface_method_type, &generics);
                let given_method_type = EvaluatedType::MethodInstance {
                    method: method_impl.0,
                    is_invariant: false,
                    generic_arguments: evaluate_generic_params(model_method_generic_params, true),
                };
                let function_type =
                    distill_as_function_type(&given_method_type, symbollib).unwrap();
                let given_method_type = EvaluatedType::FunctionExpressionInstance {
                    is_async: function_type.is_async,
                    is_invariant: false,
                    params: function_type.parameter_types,
                    return_type: Box::new(function_type.return_type),
                    generic_args: function_type.generic_arguments.to_vec(),
                };
                let unify =
                    |left, right| unify_types(left, right, symbollib, UnifyOptions::None, None);

                if unify(&expected_method_type, &given_method_type)
                    .and_then(|_| unify(&given_method_type, &expected_method_type))
                    .is_err()
                {
                    let method_name =
                        format!("{}.{}", interface_symbol.name, interface_method_symbol.name);
                    let left = symbollib.format_evaluated_type(&expected_method_type);
                    let right = symbollib.format_evaluated_type(&given_method_type);
                    checker_ctx.add_error(errors::mismatched_method_signature(
                        method_name,
                        left,
                        right,
                        span,
                    ))
                }
                //
            }
        }
    }

    // todo: Block model cycles.

    for property in &mut model.body.properties {
        typecheck_model_property(property, symbollib, checker_ctx)
    }

    checker_ctx.enclosing_model_or_interface = former_enclosing;
}

fn typecheck_model_property(
    property: &mut crate::TypedModelProperty,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
) {
    match &mut property._type {
        TypedModelPropertyType::TypedAttribute => {
            // Only thing to do is check that the type is valid,
            // and calculate the size.
            let attribute_symbol = symbollib.get(property.name);
            if attribute_symbol.is_none() {
                return;
            }
            let attribute_symbol = attribute_symbol.unwrap();
            let declared_type = match &attribute_symbol.kind {
                SemanticSymbolKind::Attribute { declared_type, .. } => declared_type,
                _ => return,
            };
            let span = attribute_symbol.ident_span();
            evaluate(
                declared_type,
                symbollib,
                None,
                &mut checker_ctx.tracker(span),
                0,
            );
        }
        // For model methods, if the return type
        TypedModelPropertyType::TypedMethod { body }
        | TypedModelPropertyType::InterfaceImpl { body, .. } => {
            let symbol = match symbollib.get_forwarded(property.name) {
                Some(symbol) => symbol,
                None => return,
            };
            let former_is_static = checker_ctx.current_function_is_static.take();
            let (evaluated_param_types, return_type, return_type_span) =
                if let SemanticSymbolKind::Method {
                    params,
                    generic_params,
                    return_type,
                    is_static,
                    ..
                } = &symbol.kind
                {
                    checker_ctx.current_function_is_static = Some(*is_static);
                    let generic_arguments = evaluate_generic_params(generic_params, true);
                    let evaluated_param_types =
                        evaluate_parameter_idxs(params, symbollib, generic_arguments, checker_ctx);
                    let return_type = return_type.as_ref();
                    (
                        evaluated_param_types,
                        return_type
                            .map(|typ| {
                                let span = typ.span();
                                evaluate(typ, &symbollib, None, &mut checker_ctx.tracker(span), 0)
                            })
                            .unwrap_or_else(|| EvaluatedType::Void),
                        return_type.map(|typ| typ.span()),
                    )
                } else {
                    (vec![], EvaluatedType::Void, None)
                };
            validate_return_type_and_params(
                &return_type,
                symbollib,
                checker_ctx,
                return_type_span,
                evaluated_param_types,
                false,
            );
            function::typecheck_function_body(
                property.name,
                checker_ctx,
                return_type,
                body,
                symbollib,
                return_type_span,
            );
            // If you ask me, I can't tell you.
            symbollib.pop_type_environment_stack(body.scopeid);
            checker_ctx.current_function_is_static = former_is_static;
        }
    }
    // For now, just check that the method being implemented exists on the
    // interface.
    if let TypedModelPropertyType::InterfaceImpl {
        interface_target, ..
    } = &property._type
    {
        let interface = &interface_target[interface_target.len() - 2];
        let span = interface.span();
        if let IntermediateType::SimpleType { value, .. } = interface {
            let interface_symbol = unwrap_or_return!(symbollib.get_forwarded(*value));
            if let SemanticSymbolKind::Interface { methods, .. } = &interface_symbol.kind {
                let property_symbol = unwrap_or_return!(symbollib.get(property.name));
                'check_interface_method: {
                    for method in methods {
                        let method_symbol = unwrap_or_continue!(symbollib.get(*method));
                        if method_symbol.name == property_symbol.name {
                            break 'check_interface_method; // Match found.
                        }
                    }
                    let base_type = interface_symbol.name.clone();
                    let property_name = property_symbol.name.clone();
                    checker_ctx.add_error(errors::unknown_property(
                        base_type,
                        property_name,
                        property.span,
                    ));
                }
            } else {
                checker_ctx.add_error(errors::interface_expected(
                    interface_symbol.name.clone(),
                    span,
                ));
            }
        }
    }
}

/// Returns a full list of every implementation method from a list of impls.
fn get_all_implementation_methods<'a>(
    implementations: &'a Vec<IntermediateType>,
    symbollib: &'a SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
    generics: &mut Vec<(SymbolIndex, EvaluatedType)>,
    model_evaluated_type: &EvaluatedType,
    model_symbol: &crate::SemanticSymbol,
) -> Vec<(SymbolIndex, SymbolIndex, Option<&'a IntermediateTypeClause>)> {
    let mut implementation_names = vec![];
    let implementation_methods = implementations
        .iter()
        .filter_map(|implementation| {
            let span = implementation.span();
            let evaluated = evaluate_and_ignore_constraint(
                implementation,
                symbollib,
                None,
                &mut checker_ctx.tracker(span),
                0,
            );
            if !evaluated.is_interface_instance() {
                let name = symbollib.format_evaluated_type(&evaluated);
                checker_ctx.add_error(errors::interface_expected(name, span));
                return None;
            }
            let constraint = match implementation {
                IntermediateType::BoundConstraintType { clause, .. } => Some(clause.as_ref()),
                _ => None,
            };
            if let EvaluatedType::InterfaceInstance {
                interface_,
                mut generic_arguments,
                ..
            } = evaluated
            {
                generics.push((interface_, model_evaluated_type.clone()));
                generics.append(&mut generic_arguments);
                let interface_symbol = symbollib.get(interface_)?;
                let name = interface_symbol.name.as_str();
                // Block multiple implementations of the same interface.
                if implementation_names.contains(&name) {
                    checker_ctx.add_error(errors::duplicate_implementation(
                        name.to_owned(),
                        model_symbol.ident_span(),
                    ));
                    return None;
                } else {
                    implementation_names.push(name);
                }
                if let SemanticSymbolKind::Interface { methods, .. } = &interface_symbol.kind {
                    return Some(
                        methods
                            .iter()
                            .map(move |method| (interface_, *method, constraint)),
                    );
                }
            }
            return None;
        })
        .flatten()
        .collect::<Vec<_>>();
    implementation_methods
}

fn typecheck_model_constructor(
    model: &mut TypedModelDeclaration,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
) -> ControlFlow<()> {
    // If the model has a constructor:
    if let Some(constructor) = &mut model.body.constructor {
        // For a model to be validly constructed, all its attributes have been definitively assigned in its constructor.
        // i.e. for every attribute, there must be an assignment expression (with =) where the attribute is the lhs.
        // question: What about in cases where the attribute is used before it is assigned? e.g.:
        // this.a = this.b;
        // this.b = someValue;
        // answer: All instances of the attribute are recorded and tracked. If the first instance is not an assignment, error.
        let model_symbol = symbollib.get(model.name).unwrap();
        let attribute_idxs =
            if let SemanticSymbolKind::Model { attributes, .. } = &model_symbol.kind {
                attributes
            } else {
                return ControlFlow::Break(());
            };
        let mut attributes = HashMap::new();
        for idx in attribute_idxs {
            let idx = *idx;
            attributes.insert(idx, Vec::new());
        }
        checker_ctx
            .current_constructor_context
            .push(CurrentConstructorContext {
                model: model.name,
                scopes: Vec::new(),
                attributes,
            });
        // Constructors should always return void.
        checker_ctx
            .current_function_context
            .push(CurrentFunctionContext {
                is_named: true,
                return_type: EvaluatedType::Void,
            });
        let block_type = typecheck_block(constructor, true, checker_ctx, symbollib);
        if !block_type.is_void() && !block_type.is_never() {
            let span = constructor
                .statements
                .last()
                .map(|statement| checker_ctx.span_of_stmnt(statement, symbollib))
                .unwrap_or_else(|| constructor.span);
            checker_ctx.add_error(errors::return_from_constructor(span));
        }
        checker_ctx.current_function_context.pop();
        let constructor_context = checker_ctx.current_constructor_context.pop().unwrap();
        for (attribute_idx, assignments) in constructor_context.attributes {
            let attribute_symbol = unwrap_or_continue!(symbollib.get(attribute_idx));
            if let Some(AttributeAssignment::Definite { span }) = assignments
                .iter()
                .find(|assignment| matches!(assignment, AttributeAssignment::Definite { .. }))
            {
                let assignment_span = *span;
                // Checks for prior usage before assignment with the spans.
                // todo: something about using spans is icky.
                let symbol_reference_list = attribute_symbol.references.first().unwrap(); // References in this module.
                let reference_starts_in_constructor_block =
                    symbol_reference_list.starts.iter().filter_map(|start| {
                        let start = *start;
                        constructor.span.contains(start).then(|| start)
                    });
                for start in reference_starts_in_constructor_block {
                    let reference_span = Span::on_line(start, attribute_symbol.name.len() as u32);
                    if reference_span.is_before(assignment_span) {
                        checker_ctx
                            .add_error(errors::using_attribute_before_assign(reference_span));
                        break;
                    }
                }
            } else {
                // If the attribute type implements Default, it can be omitted,
                // but only if the attribute type is neither opaque nor generic.
                // Opaque types cannot implement Default, by definition, so only generics are checked.
                let mut failed = true;
                let mut evaluated_type = EvaluatedType::Unknown;
                if let SemanticSymbolKind::Attribute { declared_type, .. } = &attribute_symbol.kind
                {
                    if let Some(default) = symbollib.default {
                        evaluated_type = evaluate(declared_type, symbollib, None, &mut None, 0);
                        if !evaluated_type.is_generic()
                            && get_implementation_of(default, &evaluated_type, symbollib).is_some()
                        {
                            failed = false;
                        }
                    }
                }
                if failed {
                    let name = symbollib.format_evaluated_type(&evaluated_type);
                    checker_ctx.add_error(errors::unassigned_attribute(
                        name,
                        attribute_symbol.origin_span,
                    ));
                }
            }
        }
    }
    ControlFlow::Continue(())
}
