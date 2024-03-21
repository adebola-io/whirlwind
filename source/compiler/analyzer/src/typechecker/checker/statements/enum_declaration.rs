use crate::TypedEnumDeclaration;

use super::*;

/// Typechecks an enum and all its variants.
pub fn typecheck_enum_declaration(
    enum_: &mut TypedEnumDeclaration,
    checker_ctx: &mut TypecheckerContext,
    symbollib: &mut SymbolLibrary,
) {
    let enum_symbol = unwrap_or_return!(symbollib.get(enum_.name));
    if let SemanticSymbolKind::Enum {
        generic_params,
        variants,
        ..
    } = &enum_symbol.kind
    {
        typecheck_generic_params(generic_params, symbollib, checker_ctx);
        for variant in variants {
            let variant_symbol = unwrap_or_continue!(symbollib.get(*variant));
            if let SemanticSymbolKind::Variant { tagged_types, .. } = &variant_symbol.kind {
                for typ in tagged_types {
                    let span = typ.span();
                    evaluate(typ, symbollib, None, &mut checker_ctx.tracker(span), 0);
                }
            }
        }
    }
}
