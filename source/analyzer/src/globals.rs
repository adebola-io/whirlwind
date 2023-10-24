use ast::Span;

use crate::{
    IntermediateType::SimpleType, SemanticSymbol, SemanticSymbolKind::TypeName, SymbolIndex,
    SymbolTable,
};

/// Builtin and global symbols.
pub const CORE_LIBRARY_PATH: &'static str =
    "/home/adebola/projects/whirlwind/examples/fakeCore/Core/Core.wrl";

pub const GLOBALS: [&str; 1] = ["never"];

/// Create the global functions, types and models.
// todo: add prelude module from Core.
pub fn create_globals(table: &mut SymbolTable) {
    let globals = [SemanticSymbol {
        name: String::from("never"),
        kind: TypeName {
            is_public: false,
            generic_params: vec![],
            value: SimpleType {
                value: SymbolIndex(0),
                generic_args: vec![],
                span: Span::default(),
            },
        },
        references: vec![],
        doc_info: None,
        origin_span: Span::default(),
    }];

    for global in globals {
        table.add(global);
    }
}
