#![allow(unused)]
use analyzer::{
    EvaluatedType, FullProgramContext, IntermediateType, SemanticSymbolKind, SymbolIndex,
};

pub struct SymbolWriter<'a> {
    context: &'a FullProgramContext,
}

impl<'a> SymbolWriter<'a> {
    /// Creates a new symbol printer.
    pub fn new(context: &'a FullProgramContext) -> Self {
        Self { context }
    }
    /// Print a symbol.
    pub fn print_symbol_with_idx(&self, symbol_idx: SymbolIndex) -> String {
        let symbol = self.context.symbol_table.get(symbol_idx).unwrap();
        let mut string = String::new();
        match &symbol.kind {
            SemanticSymbolKind::Module {
                parent_modules,
                imports,
                exports,
            } => {
                string.push_str("module "); // todo: print full path.
                string.push_str(&symbol.name);
            }
            SemanticSymbolKind::Trait {
                implementations,
                methods,
            } => todo!(),
            SemanticSymbolKind::Model {
                is_public,
                is_constructable,
                generic_params,
                implementations,
                methods,
                attributes,
            } => todo!(),
            SemanticSymbolKind::Enum {
                is_public,
                generic_params,
                variants,
            } => {
                if *is_public {
                    string.push_str("public ");
                }
                string.push_str("enum ");
                string.push_str(&symbol.name);
                self.maybe_print_generic_params_into_string(&mut string, generic_params);
            }
            SemanticSymbolKind::Variant {
                owner_enum,
                variant_index,
                tagged_types,
            } => {
                let name_of_owner = &self
                    .context
                    .symbol_table
                    .get(*owner_enum)
                    .expect("Could not retrieve owner enum fro variant symbol while printing.")
                    .name;
                string.push_str("(variant) ");
                string.push_str(name_of_owner);
                string.push('.');
                string.push_str(&symbol.name);
                if tagged_types.len() > 0 {
                    string.push('(');
                    for (i, tagged_type) in tagged_types.iter().enumerate() {
                        string.push_str(&self.print_intermediate_type(tagged_type));
                        if i + 1 != tagged_types.len() {
                            string.push_str(" + ");
                        }
                    }
                    string.push(')');
                }
            }
            SemanticSymbolKind::Variable {
                is_public,
                declared_type,
                inferred_type,
            } => {
                if *is_public {
                    string.push_str("public ")
                }
                string.push_str("var ");
                string.push_str(&symbol.name);
                // Always favor displaying the inferred type over the declared one.
                if !matches!(inferred_type, EvaluatedType::Unknown { .. }) {
                    todo!()
                } else if let Some(typ) = declared_type {
                    string.push_str(": ");
                    string.push_str(&self.print_intermediate_type(typ))
                } else {
                    // could not infer type
                    string.push_str(": {{unknown}}")
                }
            }
            SemanticSymbolKind::Constant {
                is_public,
                declared_type,
                inferred_type,
            } => {
                if *is_public {
                    string.push_str("public ")
                }
                string.push_str("const ");
                string.push_str(&symbol.name);
                // Always favor displaying the inferred type over the declared one.
                if !matches!(inferred_type, EvaluatedType::Unknown { .. }) {
                    todo!()
                } else {
                    string.push_str(": ");
                    string.push_str(&self.print_intermediate_type(declared_type))
                }
            }
            SemanticSymbolKind::Attribute {
                owner_model,
                is_public,
                property_index,
                declared_type,
                inferred_type,
            } => todo!(),
            SemanticSymbolKind::Method {
                is_public,
                is_static,
                is_async,
                owner_model_or_trait,
                property_index,
                params,
                generic_params,
                return_type,
            } => todo!(),
            SemanticSymbolKind::Parameter {
                is_optional,
                param_type,
            } => {
                string.push_str(&symbol.name);
                if *is_optional {
                    string.push('?')
                }
                string.push_str(": ");
                if let Some(label) = param_type.as_ref() {
                    string.push_str(&self.print_intermediate_type(label));
                } else {
                    string.push_str("{unknown}")
                }
            }
            SemanticSymbolKind::GenericParameter {
                traits,
                default_value,
                solutions,
            } => {
                string.push_str(&symbol.name);
                if traits.len() > 0 {
                    string.push_str(": ");
                    for (i, trait_) in traits.iter().enumerate() {
                        string.push_str(&self.print_intermediate_type(trait_));
                        if i + 1 != traits.len() {
                            string.push_str(" + ");
                        }
                    }
                }
                if let Some(ref default) = default_value {
                    string.push_str(" = ");
                    string.push_str(&self.print_intermediate_type(default));
                }
            }
            SemanticSymbolKind::Function {
                is_public,
                is_async,
                params,
                generic_params,
                return_type,
            } => {
                // Construct function signature.
                if *is_public {
                    string.push_str("public ");
                }
                if *is_async {
                    string.push_str("async ");
                }
                string.push_str("function ");
                string.push_str(&symbol.name);
                self.maybe_print_generic_params_into_string(&mut string, generic_params);
                self.print_parameters_into_string(&mut string, params);
                self.maybe_print_return_type_into_string(&mut string, return_type.as_ref());
            }
            SemanticSymbolKind::FnExpr {
                is_async,
                params,
                generic_params,
                return_type,
            } => unreachable!("Attempting to print a function expression. Thats...not possible."),
            SemanticSymbolKind::TypeName {
                is_public,
                generic_params,
                value,
            } => {
                if *is_public {
                    string.push_str("public ");
                }
                string.push_str("type ");
                string.push_str(&symbol.name);
                self.maybe_print_generic_params_into_string(&mut string, generic_params);
                string.push_str(" = ");
                string.push_str(&self.print_intermediate_type(value));
            }
            SemanticSymbolKind::UndeclaredValue => {
                string.push_str(&symbol.name);
                string.push_str(": {{unknown}}");
            }
            SemanticSymbolKind::Import { is_public, source } => todo!(),
            SemanticSymbolKind::Property { resolved } => match resolved {
                Some(idx) => {
                    string = self.print_symbol_with_idx(*idx);
                }
                _ => {
                    string.push_str("(property) ");
                    string.push_str(&symbol.name);
                    string.push_str(": {{unknown}}")
                }
            },
        };
        string
    }
    /// Print a set of parameters into a string.
    fn print_parameters_into_string(&self, string: &mut String, params: &[SymbolIndex]) {
        string.push('(');
        for (index, parameter) in params.iter().enumerate() {
            string.push_str(&self.print_symbol_with_idx(*parameter));
            if index < params.len() - 1 {
                string.push_str(", ");
            }
        }
        string.push(')');
    }

    fn maybe_print_generic_params_into_string(
        &self,
        string: &mut String,
        generic_params: &[SymbolIndex],
    ) {
        if generic_params.len() == 0 {
            return;
        }
        string.push('<');
        for (index, generic_parameter) in generic_params.iter().enumerate() {
            string.push_str(&self.print_symbol_with_idx(*generic_parameter));
            if index + 1 != generic_params.len() {
                string.push_str(", ");
            }
        }
        string.push('>');
    }

    fn maybe_print_return_type_into_string(
        &self,
        string: &mut String,
        return_type: Option<&IntermediateType>,
    ) {
        if let Some(rettype) = return_type {
            string.push_str(": ");
            string.push_str(&self.print_intermediate_type(rettype))
        }
    }
    /// Prints an intermediate type using the symbol table.
    pub fn print_intermediate_type(&self, typ: &IntermediateType) -> String {
        match typ {
            IntermediateType::FunctionType {
                params,
                return_type,
                span,
            } => {
                let mut string = String::from("fn(");
                for (i, param) in params.iter().enumerate() {
                    string.push_str(&self.print_parameter_type(param));
                    if i + 1 < params.len() {
                        string.push_str(", ")
                    }
                }
                string.push(')');
                if let Some(ref rettype) = return_type {
                    string.push_str(": ");
                    string.push_str(&self.print_intermediate_type(rettype))
                }
                string
            }
            IntermediateType::SimpleType {
                value,
                generic_args,
                span,
            } => {
                let mut symbol = self.context.symbol_table.get(*value).unwrap();
                let mut string = symbol.name.clone();
                if generic_args.len() > 0 {
                    string.push('<');
                    for (index, genarg) in generic_args.iter().enumerate() {
                        string.push_str(&self.print_intermediate_type(genarg));
                        if index + 1 < generic_args.len() {
                            string.push_str(", ")
                        }
                    }
                    string.push('>');
                }
                string
            }
            IntermediateType::UnionType { types, .. } => {
                let mut string = String::new();
                for (index, typeexp) in types.iter().enumerate() {
                    string.push_str(&self.print_intermediate_type(typeexp));
                    if index + 1 < types.len() {
                        string.push_str(" | ");
                    }
                    // Show at most 5 types + the last one.
                    if index == 4 && types.len() > 6 {
                        let len = types.len();
                        string.push_str("... ");
                        string.push_str(&(len - 6).to_string());
                        string.push_str(" more ... | ");
                        string.push_str(&self.print_intermediate_type(types.last().unwrap()));
                        break;
                    }
                }
                string
            }
            IntermediateType::This { .. } => format!("This"),
            IntermediateType::BorrowedType { value, .. } => {
                format!("&{}", self.print_intermediate_type(value))
            }
            IntermediateType::Placeholder => {
                unreachable!("Attempted to print a placeholder intermediate type.")
            }
        }
    }

    pub fn print_parameter_type(&self, param: &analyzer::ParameterType) -> String {
        let mut string = String::new();
        string.push_str(&param.name);
        if param.is_optional {
            string.push('?')
        }
        string.push_str(": ");
        if let Some(label) = &param.type_label {
            string.push_str(&self.print_intermediate_type(label));
        } else {
            string.push_str("{unknown}")
        }
        string
    }
}
