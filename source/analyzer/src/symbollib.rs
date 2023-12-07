use std::collections::HashMap;

use crate::{
    evaluate,
    utils::{get_numeric_type, symbol_to_type},
    EvaluatedType, Literal, LiteralMap, PathIndex, SemanticSymbol, SemanticSymbolKind, SymbolIndex,
    TypedExpression,
};

#[derive(Debug, Default)]
pub enum SymbolEntry {
    #[default]
    Removed,
    Symbol(SemanticSymbol),
}

/// The symbol library contains all the symbols for the whole standpoint.
#[derive(Debug, Default)]
pub struct SymbolLibrary {
    tables: HashMap<PathIndex, SymbolTable>,
    pub float: Option<SymbolIndex>,
    pub float32: Option<SymbolIndex>,
    pub float64: Option<SymbolIndex>,
    pub int: Option<SymbolIndex>,
    pub sint: Option<SymbolIndex>,
    pub uint8: Option<SymbolIndex>,
    pub uint16: Option<SymbolIndex>,
    pub uint32: Option<SymbolIndex>,
    pub uint64: Option<SymbolIndex>,
    pub uint: Option<SymbolIndex>,
    pub string: Option<SymbolIndex>,
    pub array: Option<SymbolIndex>,
    pub bool: Option<SymbolIndex>,
    pub prospect: Option<SymbolIndex>,
    pub never: Option<SymbolIndex>,
    pub addition: Option<SymbolIndex>,
    pub try_s: Option<SymbolIndex>,
    pub guaranteed: Option<SymbolIndex>,
    pub maybe: Option<SymbolIndex>,
    pub range: Option<SymbolIndex>,
    pub flow: Option<SymbolIndex>,
    pub default: Option<SymbolIndex>,
}

#[derive(Debug)]
pub struct SymbolTable {
    symbols: Vec<SemanticSymbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { symbols: vec![] }
    }
}

impl SymbolLibrary {
    /// Create a new symbol table.
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
    /// Add a symbol to a module's symbol table and return its index number.
    pub fn add_to_table(&mut self, module: PathIndex, symbol: SemanticSymbol) -> SymbolIndex {
        // Create the table if it does not exist.
        if self.tables.get(&module).is_none() {
            self.tables.insert(module, SymbolTable::new());
        }
        let table = self.tables.get_mut(&module).unwrap();
        let id = table.symbols.len();
        table.symbols.push(symbol);
        // // Fill any holes by removed symbols.
        // let index = match self.holes.pop() {
        //     Some(void_idx) => {
        //         self.symbols[void_idx] = SymbolEntry::Symbol(symbol);
        //         void_idx
        //     }
        //     None => {
        //         let id = self.symbols.len();
        //         self.symbols.push(SymbolEntry::Symbol(symbol));
        //         id
        //     }
        // };
        SymbolIndex(module, id as u32)
    }
    /// Returns an iterator over all the symbols in the table.
    pub fn symbols(&self) -> impl Iterator<Item = (SymbolIndex, &SemanticSymbol)> {
        self.tables
            .iter()
            .map(|(pathindex, table)| {
                table
                    .symbols
                    .iter()
                    .enumerate()
                    .map(|(index, symbol)| (SymbolIndex(*pathindex, index as u32), symbol))
            })
            .flatten()
        // self.tables.iter()

        //     .enumerate()
        //     .filter_map(|symbolentry| match symbolentry {
        //         (idx, SymbolEntry::Symbol(symbol)) => Some((SymbolIndex(idx), symbol)),
        //         _ => None,
        //     })
    }
    /// Get a symbol using its index.
    pub fn get(&self, index: SymbolIndex) -> Option<&SemanticSymbol> {
        self.tables.get(&index.0)?.symbols.get(index.1 as usize)
    }
    /// Returns an iterator of the undeclared values in the table.
    pub fn undeclared_values(&self) -> impl Iterator<Item = &SemanticSymbol> {
        self.symbols()
            .map(|(_, symbol)| symbol)
            .filter(|symbol| matches!(symbol.kind, SemanticSymbolKind::UndeclaredValue))
    }
    /// Returns a list of the symbols in a module.
    pub fn in_module(&self, module_path: PathIndex) -> impl Iterator<Item = &SemanticSymbol> {
        let table = self.tables.get(&module_path);
        table.unwrap().symbols.iter()
    }
    /// Returns the first symbol in the table that adheres to a predicate.
    pub fn find<F: FnMut(&&SemanticSymbol) -> bool>(
        &self,
        predicate: F,
    ) -> Option<&SemanticSymbol> {
        self.symbols().map(|(_, symbol)| symbol).find(predicate)
    }
    /// Returns the type of an expression.
    pub fn get_expression_type(
        &self,
        expression: &TypedExpression,
        literals: &LiteralMap,
    ) -> Option<EvaluatedType> {
        Some(match expression {
            TypedExpression::Identifier(ident) => {
                let symbol = self.get_forwarded(ident.value)?;
                return symbol_to_type(symbol, ident.value, self).ok();
            }
            TypedExpression::Literal(literal) => EvaluatedType::ModelInstance {
                model: match literals.get(*literal)? {
                    Literal::StringLiteral { .. } => self.string?,
                    Literal::NumericLiteral { value, .. } => {
                        return Some(get_numeric_type(self, value, None))
                    } // todo.
                    Literal::BooleanLiteral { .. } => self.bool?,
                },
                generic_arguments: vec![],
            },
            TypedExpression::NewExpr(new) => new.inferred_type.clone(),
            TypedExpression::ThisExpr(this) => this.inferred_type.clone(),
            TypedExpression::CallExpr(call) => call.inferred_type.clone(),
            TypedExpression::FnExpr(f) => f.inferred_type.clone(),
            TypedExpression::Block(block) => block.inferred_type.clone(),
            TypedExpression::IfExpr(if_) => if_.inferred_type.clone(),
            TypedExpression::AccessExpr(access) => access.inferred_type.clone(),
            TypedExpression::ArrayExpr(array) => array.inferred_type.clone(),
            TypedExpression::IndexExpr(i) => i.inferred_type.clone(),
            TypedExpression::BinaryExpr(b) => b.inferred_type.clone(),
            TypedExpression::AssignmentExpr(a) => a.inferred_type.clone(),
            TypedExpression::UnaryExpr(u) => u.inferred_type.clone(),
            TypedExpression::LogicExpr(l) => l.inferred_type.clone(),
            TypedExpression::UpdateExpr(u) => u.inferred_type.clone(),
        })
    }
    /// Get a symbol mutably using its index.
    pub fn get_mut(&mut self, idx: SymbolIndex) -> Option<&mut SemanticSymbol> {
        self.tables.get_mut(&idx.0)?.symbols.get_mut(idx.1 as usize)
    }
    /// A modified version of the `get()` method that also accounts for import and property redirections.
    pub fn get_forwarded(&self, idx: SymbolIndex) -> Option<&SemanticSymbol> {
        let base = self.get(idx)?;
        match &base.kind {
            SemanticSymbolKind::Property {
                resolved: Some(next),
                ..
            }
            | SemanticSymbolKind::Import {
                source: Some(next), ..
            } => self.get_forwarded(*next),
            _ => Some(base),
        }
    }
    /// Follow a property or an import chain to its source index.
    pub fn forward(&self, idx: SymbolIndex) -> SymbolIndex {
        match self.get(idx) {
            Some(base) => match &base.kind {
                SemanticSymbolKind::Property {
                    resolved: Some(next),
                    ..
                }
                | SemanticSymbolKind::Import {
                    source: Some(next), ..
                } => self.forward(*next),
                _ => idx,
            },
            None => idx,
        }
    }
    /// Remove a symbol table using its index.
    pub fn remove_module_table(&mut self, module_path: PathIndex) -> Option<SymbolTable> {
        self.tables.remove(&module_path)
    }
    /// Get a list of at most five related symbols for a symbol at an index.
    pub fn get_relations(&self, _index: SymbolIndex) -> Option<Vec<&SemanticSymbol>> {
        todo!()
    }
    /// Returns the number of symbols in the table.
    pub fn len(&self) -> usize {
        self.tables
            .iter()
            .map(|(_, table)| table.symbols.len())
            .fold(0, |acc, x| acc + x)
    }

    /// Prints a list of generic types.
    fn format_generics_into<'a>(
        &self,
        generics: impl Iterator<Item = &'a EvaluatedType>,
        string: &mut String,
        len: usize,
    ) {
        if len > 0 {
            string.push('<');
            for (index, genarg) in generics.enumerate() {
                string.push_str(&self.format_evaluated_type(genarg));
                if index + 1 < len {
                    string.push_str(", ")
                }
            }
            string.push('>');
        }
    }

    /// Prints an evaluated type using the symbol table.
    pub fn format_evaluated_type(&self, eval_type: &EvaluatedType) -> String {
        let mut string = String::new();
        match eval_type {
            EvaluatedType::ModelInstance {
                model: base,
                generic_arguments,
            }
            | EvaluatedType::InterfaceInstance {
                interface_: base,
                generic_arguments,
            } => {
                let symbol = self.get(*base).unwrap();
                string = symbol.name.clone();
                // Only print the arguments that are parameters to this type.
                let generic_params = match &symbol.kind {
                    SemanticSymbolKind::Interface { generic_params, .. }
                    | SemanticSymbolKind::Model { generic_params, .. } => generic_params,
                    _ => return string,
                };
                let truncated_generic_args = truncate_arguments(generic_params, generic_arguments);
                self.format_generics_into(
                    truncated_generic_args.iter(),
                    &mut string,
                    truncated_generic_args.len(),
                );
            }
            EvaluatedType::Model(_) => string.push_str("{model}"),
            EvaluatedType::Interface(_) => string.push_str("{interface}"),
            EvaluatedType::EnumInstance {
                enum_,
                generic_arguments,
            } => {
                let symbol = self.get(*enum_).unwrap();
                string = symbol.name.clone();
                self.format_generics_into(
                    get_just_types(generic_arguments),
                    &mut string,
                    generic_arguments.len(),
                );
            }
            EvaluatedType::FunctionInstance {
                function,
                generic_arguments,
            } => {
                self.format_function_details(function, &mut string, generic_arguments);
            }
            EvaluatedType::FunctionExpressionInstance {
                is_async,
                params,
                return_type,
                ..
            } => {
                if *is_async {
                    string.push_str("async ")
                }
                string.push_str("fn(");
                for (idx, param) in params.iter().enumerate() {
                    string.push_str(&param.name);
                    if param.is_optional {
                        string.push('?');
                    }
                    string.push_str(": ");
                    string.push_str(&self.format_evaluated_type(&param.inferred_type));
                    if idx != params.len() - 1 {
                        string.push_str(", ");
                    }
                }
                string.push_str(")");
                if !return_type.is_void() {
                    string.push_str(&format!(": {}", self.format_evaluated_type(return_type)));
                }
            }
            EvaluatedType::MethodInstance {
                method,
                generic_arguments,
            } => {
                self.format_function_details(method, &mut string, &generic_arguments);
            }
            EvaluatedType::Enum(_enum) => {
                string.push_str("{enum ");
                let symbol = self.get(*_enum).unwrap();
                string.push_str(&symbol.name);
                string.push_str("}");
            }
            EvaluatedType::Module(module) => {
                string.push_str("{module ");
                let symbol = self.get(*module).unwrap();
                string.push_str(&symbol.name);
                string.push_str("}");
            }
            EvaluatedType::Generic { base, .. } => {
                string.push_str("{type ");
                let symbol = self.get(*base).unwrap();
                string.push_str(&symbol.name);
                string.push_str("}");
            }
            EvaluatedType::HardGeneric { base } => {
                let symbol = self.get(*base).unwrap();
                string.push_str(&symbol.name);
            }
            EvaluatedType::Borrowed { base } => {
                string.push('&');
                string.push_str(&self.format_evaluated_type(base));
            }
            EvaluatedType::OpaqueTypeInstance {
                collaborators,
                aliased_as,
                generic_arguments,
                ..
            } => {
                if let Some(alias) = aliased_as {
                    let symbol = self.get(*alias).unwrap();
                    string = symbol.name.clone();
                    // Only print the arguments that are parameters to this type.
                    let generic_params = match &symbol.kind {
                        SemanticSymbolKind::TypeName { generic_params, .. } => generic_params,
                        _ => return string,
                    };
                    let truncated_generic_args =
                        truncate_arguments(generic_params, generic_arguments);
                    self.format_generics_into(
                        truncated_generic_args.iter(),
                        &mut string,
                        truncated_generic_args.len(),
                    );
                    return string;
                }
                for (index, collaborator) in collaborators.iter().enumerate() {
                    let name = &self.get(*collaborator).unwrap().name;
                    string.push_str(name);
                    if index + 1 < collaborators.len() {
                        string.push_str(" | ");
                    }
                    // Show at most 5 types + the last one.
                    if index == 4 && collaborators.len() > 6 {
                        let len = collaborators.len();
                        string.push_str("... ");
                        string.push_str(&(len - 6).to_string());
                        string.push_str(" more ... | ");
                        let name = &self.get(*collaborators.last().unwrap()).unwrap().name;
                        string.push_str(name);
                        break;
                    }
                }
            }
            EvaluatedType::Void => string.push_str("{void}"),
            EvaluatedType::Never => string.push_str("never"),
            EvaluatedType::Unknown { .. } => string.push_str("{unknown}"),
            EvaluatedType::Partial { types } => {
                for (idx, typ) in types.iter().enumerate() {
                    string.push_str(&self.format_evaluated_type(&typ));
                    if idx + 1 != types.len() {
                        string.push_str(" | ");
                    }
                }
            }
        }
        string
    }

    /// Prints out a functional evaluated type.
    fn format_function_details(
        &self,
        function: &SymbolIndex,
        string: &mut String,
        generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
    ) {
        let function_symbol = self.get(*function).unwrap();
        let (is_async, _, params, return_type) = match &function_symbol.kind {
            SemanticSymbolKind::Function {
                is_async,
                params,
                generic_params,
                return_type,
                ..
            }
            | SemanticSymbolKind::Method {
                is_async,
                params,
                generic_params,
                return_type,
                ..
            } => (*is_async, generic_params, params, return_type),
            _ => unreachable!("Expected a functional type but got {:?}", &function_symbol),
        };
        if is_async {
            string.push_str("async ");
        }
        string.push_str("fn(");
        // format (evaluated) parameter types.
        for (idx, param) in params.iter().enumerate() {
            let parameter_symbol = self.get(*param).unwrap();
            string.push_str(&parameter_symbol.name);
            if let SemanticSymbolKind::Parameter {
                is_optional,
                param_type,
                ..
            } = &parameter_symbol.kind
            {
                if *is_optional {
                    string.push('?');
                }
                string.push_str(": ");
                let eval_type = param_type
                    .as_ref()
                    .map(|param_type| {
                        evaluate(param_type, self, Some(generic_arguments), &mut None, 0)
                    })
                    .unwrap_or(EvaluatedType::Unknown);
                string.push_str(&self.format_evaluated_type(&eval_type));
            }
            if idx != params.len() - 1 {
                string.push_str(", ");
            }
        }
        string.push(')');
        if let Some(typ) = return_type {
            let evaluated = evaluate(typ, self, Some(&generic_arguments), &mut None, 0);
            if !evaluated.is_void() {
                string.push_str(": ");
                string.push_str(&self.format_evaluated_type(&evaluated));
            }
        }
    }
}

fn truncate_arguments(
    generic_params: &Vec<SymbolIndex>,
    generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
) -> Vec<EvaluatedType> {
    generic_params
        .iter()
        .map(|param_idx| {
            if let Some(typ) = generic_arguments.iter().find(|typ| typ.0 == *param_idx) {
                return typ.1.clone();
            } else {
                return EvaluatedType::Generic { base: *param_idx };
            }
        })
        .collect::<Vec<_>>()
}

fn get_just_types(
    generic_arguments: &Vec<(SymbolIndex, EvaluatedType)>,
) -> impl Iterator<Item = &EvaluatedType> {
    generic_arguments
        .iter()
        .map(|(_, evaluated_type)| evaluated_type)
}
#[cfg(test)]
mod tests {
    use ast::Span;

    use crate::{ScopeId, SemanticSymbol, SemanticSymbolKind, SymbolLibrary};

    #[test]
    fn test_symbol_adding() {
        let mut symbollib = SymbolLibrary::new();
        let symbol_index = symbollib.add_to_table(
            crate::PathIndex(0),
            SemanticSymbol {
                name: format!("newVariable"),
                kind: SemanticSymbolKind::TypeName {
                    is_public: false,
                    generic_params: vec![],
                    value: crate::IntermediateType::Placeholder,
                },
                references: vec![],
                doc_info: None,
                origin_span: Span::default(),
                origin_scope_id: Some(ScopeId(0)),
            },
        );
        assert_eq!(symbollib.get(symbol_index).unwrap().name, "newVariable")
    }

    #[test]
    fn test_symbol_removal() {
        let mut symbollib = SymbolLibrary::new();
        let symbol_index = symbollib.add_to_table(
            crate::PathIndex(0),
            SemanticSymbol {
                name: format!("newVariable"),
                kind: SemanticSymbolKind::TypeName {
                    is_public: false,
                    generic_params: vec![],
                    value: crate::IntermediateType::Placeholder,
                },
                references: vec![],
                doc_info: None,
                origin_span: Span::default(),
                origin_scope_id: Some(ScopeId(0)),
            },
        );
        assert_eq!(symbollib.len(), 1);

        symbollib.remove_module_table(crate::PathIndex(0)).unwrap();

        assert_eq!(symbollib.len(), 0);

        assert!(symbollib.get(symbol_index).is_none(),)
    }
}
