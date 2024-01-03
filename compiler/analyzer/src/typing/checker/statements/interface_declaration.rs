use ast::unwrap_or_return;

use crate::{
    evaluate, evaluate_parameter_idxs, utils::evaluate_generic_params, EvaluatedType,
    SemanticSymbolKind, SymbolLibrary, TypecheckerContext,
};

use super::{typecheck_function_body, typecheck_generic_params, validate_return_type_and_params};

pub fn typecheck_interface(
    interface: &mut crate::TypedInterfaceDeclaration,
    checker_ctx: &mut TypecheckerContext<'_>,
    symbollib: &mut SymbolLibrary,
) {
    let interface_symbol = unwrap_or_return!(symbollib.get(interface.name));
    let former_enclosing = checker_ctx.enclosing_model_or_interface.take();
    checker_ctx.enclosing_model_or_interface = Some(interface.name);

    if let SemanticSymbolKind::Interface { generic_params, .. } = &interface_symbol.kind {
        // todo: Check inherited interfaces.
        typecheck_generic_params(generic_params, symbollib, checker_ctx);
    }
    for property in &mut interface.body.properties {
        typecheck_interface_property(property, symbollib, checker_ctx)
    }
    checker_ctx.enclosing_model_or_interface = former_enclosing;
}

fn typecheck_interface_property(
    property: &mut crate::TypedInterfaceProperty,
    symbollib: &mut SymbolLibrary,
    checker_ctx: &mut TypecheckerContext<'_>,
) {
    let symbol = unwrap_or_return!(symbollib.get(property.name));
    match &mut property._type {
        crate::TypedInterfacePropertyType::Signature => {
            let (evaluated_param_types, return_type, return_type_span) =
                if let SemanticSymbolKind::Method {
                    params,
                    generic_params,
                    return_type,
                    ..
                } = &symbol.kind
                {
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
                true,
            );
        }
        crate::TypedInterfacePropertyType::Method { body } => {
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
                true,
            );
            typecheck_function_body(checker_ctx, return_type, body, symbollib, return_type_span);
            checker_ctx.current_function_is_static = former_is_static;
        }
    }
}
