use analyzer::{TypedModelPropertyType, TypedModule, TypedVisitorNoArgs};
use std::cell::RefCell;
use tower_lsp::lsp_types::{FoldingRange, FoldingRangeKind};

use crate::diagnostic::to_range;

/// Folding range traverser.
pub struct FoldingRangeFinder<'a> {
    pub ranges: RefCell<Vec<FoldingRange>>,
    module: &'a TypedModule,
}

impl<'a> FoldingRangeFinder<'a> {
    /// Creates a new folding range finder.
    pub fn new(module: &'a TypedModule) -> Self {
        FoldingRangeFinder {
            ranges: RefCell::new(vec![]),
            module,
        }
    }
    /// Traverses the module to gather the folding ranges.
    pub fn gather(&self) {
        for statement in &self.module.statements {
            self.statement(statement);
        }
    }
    fn add_range(&self, span: ast::Span) {
        let range = to_range(span);
        let folding_range = FoldingRange {
            start_line: range.start.line,
            start_character: Some(range.start.character),
            end_line: range.end.line,
            end_character: Some(range.end.character),
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: None,
        };
        self.ranges.borrow_mut().push(folding_range)
    }
}

impl TypedVisitorNoArgs for FoldingRangeFinder<'_> {
    fn block(&self, block: &analyzer::TypedBlock) {
        self.add_range(block.span);
        for statement in &block.statements {
            self.statement(&statement)
        }
    }

    fn model_decl(&self, model: &analyzer::TypedModelDeclaration) -> () {
        self.add_range(model.body.span);
        if let Some(constructor) = &model.body.constructor {
            self.block(constructor)
        }
        for property in &model.body.properties {
            match &property._type {
                TypedModelPropertyType::TypedMethod { body }
                | TypedModelPropertyType::InterfaceImpl { body, .. } => self.block(&body),
                _ => {}
            }
        }
    }
}
