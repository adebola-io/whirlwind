use ast::unwrap_or_continue;

use super::*;

/// Typechecks an access expression.
pub fn typecheck_access_expression(
    access: &mut TypedAccessExpr,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext,
) -> EvaluatedType {
    checker_ctx.current_expression_is_access = Some(true);
    access.inferred_type = (|| {
        let object_type = typecheck_expression(&mut access.object, checker_ctx, symbollib);
        let property_symbol_idx = match &access.property {
            TypedExpression::Identifier(i) => i.value,
            _ => unreachable!(),
        };
        extract_property_of(
            object_type,
            symbollib,
            property_symbol_idx,
            checker_ctx,
            access,
        )
    })();
    checker_ctx.current_expression_is_access = Some(false);
    access.inferred_type.clone()
}

/// Extract a property based on the value of its object.
pub fn extract_property_of(
    object_type: EvaluatedType,
    symbollib: &mut SymbolLibrary,
    property_symbol_idx: SymbolIndex,
    checker_ctx: &mut TypecheckerContext,
    access: &mut TypedAccessExpr,
) -> EvaluatedType {
    let property_span = symbollib.get(property_symbol_idx).unwrap().ident_span();
    match object_type {
        // Accessing a property on a model instance.
        EvaluatedType::ModelInstance {
            model: base,
            ref generic_arguments,
            is_invariant,
        }
        | EvaluatedType::InterfaceInstance {
            interface_: base,
            is_invariant,
            ref generic_arguments,
        } => search_for_property(
            checker_ctx,
            symbollib,
            base,
            property_symbol_idx,
            generic_arguments.clone(),
            true,
            is_invariant,
            property_span,
        ),
        EvaluatedType::Model(model) => {
            let symbol = symbollib.get_forwarded(model).unwrap();
            let object_is_instance = false;
            // The generic arguments is an unknown list from the generic parameters.
            let generic_arguments = match &symbol.kind {
                SemanticSymbolKind::Model { generic_params, .. } => {
                    evaluate_generic_params(generic_params, false)
                }
                _ => vec![],
            };
            search_for_property(
                checker_ctx,
                symbollib,
                model,
                property_symbol_idx,
                generic_arguments,
                object_is_instance,
                false,
                property_span,
            )
        }
        // EvaluatedType::Interface(_) => todo!(),
        // EvaluatedType::Enum(_) => todo!(),
        EvaluatedType::Module(module) => {
            let module = symbollib.get_forwarded(module).unwrap();
            let property_symbol = symbollib.get_forwarded(property_symbol_idx).unwrap();
            match &module.kind {
                SemanticSymbolKind::Module {
                    global_declaration_symbols,
                    ..
                } => {
                    for idx in global_declaration_symbols {
                        let symbol = match symbollib.get(*idx) {
                            Some(symbol) => symbol,
                            None => continue,
                        };
                        if symbol.name == property_symbol.name {
                            if !symbol.kind.is_public() {
                                checker_ctx.add_error(TypeError {
                                    _type: TypeErrorType::PrivateSymbolLeak {
                                        modulename: module.name.clone(),
                                        property: property_symbol.name.clone(),
                                    },
                                    span: property_span,
                                });
                            }
                            let actualidx = symbollib.forward(*idx);
                            let actualsymbol = match symbollib.get_forwarded(actualidx) {
                                Some(symbol) => symbol,
                                None => return EvaluatedType::Unknown,
                            };
                            let evaluated_type = symbol_to_type(actualsymbol, actualidx, symbollib)
                                .ok()
                                .unwrap_or(EvaluatedType::Unknown);
                            // get mutably and resolve.
                            let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
                            if let SemanticSymbolKind::Property { resolved, .. } =
                                &mut property_symbol.kind
                            {
                                *resolved = Some(actualidx)
                            }
                            return evaluated_type;
                        }
                    }
                    // No such symbol found.
                    checker_ctx.add_error(TypeError {
                        _type: TypeErrorType::NoSuchSymbol {
                            modulename: module.name.clone(),
                            property: property_symbol.name.clone(),
                        },
                        span: property_span,
                    });
                    return EvaluatedType::Unknown;
                }
                _ => return EvaluatedType::Unknown,
            }
        }
        EvaluatedType::OpaqueTypeInstance {
            ref available_methods,
            ref generic_arguments,
            ref available_interfaces,
            ..
        } => {
            let mut generic_arguments = generic_arguments.clone();
            // Gather methods from all the implementations.
            let implementation_methods = available_interfaces
                .iter()
                .filter_map(|implementation| {
                    match implementation {
                        EvaluatedType::InterfaceInstance {
                            interface_,
                            generic_arguments: interface_generics,
                            ..
                        } => {
                            let interface_ = *interface_;
                            // Update the solutions of the interfaces generics.
                            generic_arguments.append(&mut (interface_generics.clone()));
                            // Here a interface is treated as a generic argument and given a solution.
                            // This allows the `This` marker to refer to the implementing model, rather than the interface.
                            generic_arguments.push((interface_, object_type.clone()));
                            let interface_symbol = symbollib.get_forwarded(interface_)?;
                            match &interface_symbol.kind {
                                SemanticSymbolKind::Interface { methods, .. } => Some(methods),
                                _ => return None,
                            }
                        }
                        _ => return None,
                    }
                })
                .map(|methods| methods.iter())
                .flatten();
            let complete_method_list: Vec<_> = available_methods
                .iter()
                .chain(implementation_methods)
                .collect();
            let property_symbol = symbollib.get(property_symbol_idx).unwrap();
            for method in complete_method_list {
                let method = *method;
                let method_symbol = match symbollib.get(method) {
                    Some(sym) => sym,
                    None => continue,
                };
                if method_symbol.name == property_symbol.name && method_symbol.kind.is_public() {
                    // get mutably and resolve.
                    let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
                    if let SemanticSymbolKind::Property {
                        resolved,
                        is_opaque,
                    } = &mut property_symbol.kind
                    {
                        *resolved = Some(method);
                        *is_opaque = true;
                    }
                    // todo: Is method public?
                    return EvaluatedType::MethodInstance {
                        method,
                        generic_arguments: generic_arguments.clone(),
                        is_invariant: false,
                    };
                }
            }
            None
        }
        EvaluatedType::Enum(enum_) => {
            let enum_symbol = match symbollib.get(enum_) {
                Some(enum_symbol) => enum_symbol,
                None => return EvaluatedType::Unknown,
            };
            let mut target = None;
            if let SemanticSymbolKind::Enum {
                generic_params,
                variants,
                ..
            } = &enum_symbol.kind
            {
                for variant in variants {
                    let variant_symbol = ast::unwrap_or_continue!(symbollib.get(*variant));
                    let property_symbol =
                        ast::unwrap_or_continue!(symbollib.get(property_symbol_idx));
                    if variant_symbol.name == property_symbol.name {
                        target = Some((*variant, evaluate_generic_params(generic_params, false)));
                        break;
                    };
                }
            }
            if let Some((variant, generic_arguments)) = target {
                let variant_symbol = symbollib.get_mut(variant).unwrap();
                variant_symbol.add_reference(checker_ctx.path_idx, property_span);
                let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
                if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind {
                    *resolved = Some(variant)
                }
                Some(EvaluatedType::EnumInstance {
                    enum_,
                    is_invariant: false,
                    generic_arguments,
                })
            } else {
                None
            }
        }
        EvaluatedType::Generic { base } | EvaluatedType::HardGeneric { base, .. } => {
            search_for_property(
                checker_ctx,
                symbollib,
                base,
                property_symbol_idx,
                vec![],
                true,
                false,
                property_span,
            )
        }
        EvaluatedType::Unknown => return EvaluatedType::Unknown,
        _ => None,
    }
    .unwrap_or_else(|| {
        let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
        if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind {
            *resolved = None; // Just in case.
        }
        let property = property_symbol.name.clone();
        let error = TypeErrorType::NoSuchProperty {
            base_type: symbollib.format_evaluated_type(&object_type),
            property,
        };
        checker_ctx.add_error(TypeError {
            _type: error,
            span: checker_ctx.span_of_expr(&access.property, &symbollib),
        });
        EvaluatedType::Unknown
    })
}

/// Look through all the possible methods and attributes of a model, generic or interface
/// to determine the property being referenced.
pub fn search_for_property(
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
    base: SymbolIndex,
    property_symbol_idx: SymbolIndex,
    mut generic_args: Vec<(SymbolIndex, EvaluatedType)>,
    object_is_instance: bool,
    is_invariant: bool,
    property_span: Span,
) -> Option<EvaluatedType> {
    // The base type of the model, generic or interface.
    let base_symbol = symbollib.get_forwarded(base)?;
    let property_symbol = symbollib.get(property_symbol_idx)?;
    // The interfaces available to the base type.
    let intermediate_interfaces = match &base_symbol.kind {
        SemanticSymbolKind::Model { interfaces, .. }
        | SemanticSymbolKind::GenericParameter { interfaces, .. }
        | SemanticSymbolKind::Interface { interfaces, .. } => interfaces,
        _ => return None,
    };
    let empty = vec![];
    // Methods provided by environments.
    let environment_methods = symbollib
        .type_environments
        .iter()
        .filter_map(|environment| {
            environment
                .suppositions
                .iter()
                .find(|supposition| supposition.base == base)
        })
        .map(|supposition| supposition.methods.iter().map(|method| *method))
        .flatten();
    // Interfaces from the environment.
    let environment_implementations = symbollib
        .type_environments
        .iter()
        .filter_map(|environment| {
            environment
                .suppositions
                .iter()
                .find(|supposition| supposition.base == base)
        })
        .map(|supposition| supposition.implementations.iter())
        .flatten()
        .map(|implementation| implementation.clone());

    // Gather methods from all the implementations.
    let all_interfaces: Vec<_> = intermediate_interfaces
        .iter()
        .map(|int_typ| evaluate(int_typ, symbollib, Some(&generic_args), &mut None, 0))
        .chain(environment_implementations)
        .collect();

    let all_interface_methods = all_interfaces
        .into_iter()
        .filter_map(|implementation| {
            match implementation {
                EvaluatedType::InterfaceInstance {
                    interface_,
                    generic_arguments: mut interface_generics,
                    ..
                } => {
                    // Update the solutions of the interfaces generics.
                    generic_args.append(&mut interface_generics);
                    // Here a interface is treated as a generic argument and given a solution.
                    // This allows the `This` marker to refer to the implementing model, rather than the interface.
                    let generic_arguments = generic_args.clone();
                    let this_solution = match &base_symbol.kind {
                        SemanticSymbolKind::GenericParameter { .. } => {
                            EvaluatedType::HardGeneric { base }
                        }
                        SemanticSymbolKind::Model { .. } => EvaluatedType::ModelInstance {
                            model: base,
                            generic_arguments,
                            is_invariant: false,
                        },
                        SemanticSymbolKind::Interface { .. } => EvaluatedType::InterfaceInstance {
                            interface_: base,
                            is_invariant: false,
                            generic_arguments,
                        },
                        _ => unreachable!(),
                    };
                    generic_args.push((interface_, this_solution));
                    let interface_symbol = symbollib.get_forwarded(interface_)?;
                    match &interface_symbol.kind {
                        SemanticSymbolKind::Interface { methods, .. } => Some(methods),
                        _ => return None,
                    }
                }
                _ => return None,
            }
        })
        .map(|methods| methods.iter().map(|method| *method))
        .flatten();

    let model_props = match &base_symbol.kind {
        SemanticSymbolKind::Model {
            methods,
            attributes,
            ..
        } => Some((methods, attributes)),
        SemanticSymbolKind::Interface { methods, .. } => Some((methods, &empty)),
        _ => None,
    };
    // Collecting into a new vector here because I have not found a feasible way
    // to use different iterator types in the same context, without duplicating a
    // lot of code.
    let complete_method_list: Vec<_> = match &model_props {
        Some((methods, _)) => methods
            .iter()
            .map(|method| *method)
            .chain(all_interface_methods)
            .collect(),
        // Environments methods will only be available for generics, so there is no
        // need to chain the iterator for models.
        None => environment_methods.chain(all_interface_methods).collect(),
    };
    // Is property a method?
    // Search through the compound list of methods for appriopriate property.
    for method in complete_method_list.iter() {
        let method = *method;
        let method_symbol = unwrap_or_continue!(symbollib.get_forwarded(method));
        if method_symbol.name != property_symbol.name {
            continue;
        }
        let method_is_static = match &method_symbol.kind {
            SemanticSymbolKind::Method { is_static, .. } => *is_static,
            _ => false,
        };
        if method_is_static && object_is_instance {
            checker_ctx.add_error(errors::instance_static_method_access(
                base_symbol.name.clone(),
                method_symbol.name.clone(),
                property_span,
            ))
        } else if !method_is_static && !object_is_instance {
            checker_ctx.add_error(errors::contructor_non_static_method_access(
                base_symbol.name.clone(),
                method_symbol.name.clone(),
                property_span,
            ))
        }
        // get mutably and resolve.
        let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
        if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind {
            *resolved = Some(method)
        }
        // Add reference on source.
        let method_symbol = symbollib.get_mut(method).unwrap();
        method_symbol.add_reference(checker_ctx.path_idx, property_span);
        // Block non-public access.
        if !method_symbol.kind.is_public() && checker_ctx.enclosing_model_or_interface != Some(base)
        {
            checker_ctx.add_error(errors::private_property_leak(
                method_symbol.name.clone(),
                property_span,
            ));
        }
        // Block access in constructor.
        if checker_ctx
            .current_constructor_context
            .last()
            .is_some_and(|constructor_ctx| constructor_ctx.model == base)
            && !method_is_static
        {
            checker_ctx.add_error(errors::method_in_constructor(property_span));
        }
        return Some(EvaluatedType::MethodInstance {
            method,
            generic_arguments: generic_args,
            is_invariant,
        });
    }
    // Is property an attribute?
    if let Some((_, attributes)) = model_props {
        for attribute in attributes.iter() {
            let attribute = *attribute;
            let attribute_symbol = unwrap_or_continue!(symbollib.get_forwarded(attribute));
            if attribute_symbol.name != property_symbol.name {
                continue;
            }
            let result_type = match &attribute_symbol.kind {
                SemanticSymbolKind::Attribute { declared_type, .. } => {
                    let span = attribute_symbol.ident_span();
                    evaluate(
                        &declared_type,
                        symbollib,
                        Some(&generic_args),
                        &mut checker_ctx.tracker(span),
                        0,
                    )
                }
                _ => return Some(EvaluatedType::Unknown),
            };
            // get mutably.
            let property_symbol = symbollib.get_mut(property_symbol_idx).unwrap();
            if let SemanticSymbolKind::Property { resolved, .. } = &mut property_symbol.kind {
                *resolved = Some(attribute)
            }
            // Add reference on source.
            let attribute_symbol = symbollib.get_mut(attribute).unwrap();
            attribute_symbol.add_reference(checker_ctx.path_idx, property_span);
            if !attribute_symbol.kind.is_public()
                && checker_ctx.enclosing_model_or_interface != Some(base)
            {
                checker_ctx.add_error(errors::private_property_leak(
                    attribute_symbol.name.clone(),
                    property_span,
                ));
            }
            return Some(result_type);
        }
        // Property has ultimately not been found in the model.
        // Search through the attribute list for possible suggestions.
        for attributes in attributes.iter() {
            let attribute = *attributes;
            let attribute_symbol = symbollib.get_forwarded(attribute).unwrap();
            if attribute_symbol.name.to_lowercase() == property_symbol.name.to_lowercase() {
                checker_ctx.add_error(errors::mispelled_name(
                    attribute_symbol.name.clone(),
                    property_span,
                ));
                return None;
            }
        }
    }
    // Property has not been found anywhere.
    // Search through complete method list for suggestions.
    for method in complete_method_list {
        let method_symbol = symbollib.get_forwarded(method).unwrap();
        if method_symbol.name.to_lowercase() == property_symbol.name.to_lowercase() {
            checker_ctx.add_error(errors::mispelled_name(
                method_symbol.name.clone(),
                property_span,
            ));
            return None;
        }
    }
    None
}
