use analyzer::{
    EvaluatedType, IntermediateType, IntermediateTypeProperty, ParameterType, SemanticSymbolKind,
    Standpoint, SymbolIndex, VariablePatternForm,
};
use std::{cell::RefCell, path::Path};
use utils::{get_dir_basename, get_parent_dir};

pub struct SymbolWriter<'a> {
    pub standpoint: &'a Standpoint,
    pub is_opaque: RefCell<bool>,
}

impl<'a> SymbolWriter<'a> {
    /// Creates a new symbol printer.
    pub fn new(standpoint: &'a Standpoint) -> Self {
        Self {
            standpoint,
            is_opaque: RefCell::new(false),
        }
    }
    /// Print a symbol.
    pub fn print_symbol_with_idx(&self, symbol_idx: SymbolIndex) -> String {
        let symbol = match self.standpoint.symbol_library.get(symbol_idx) {
            Some(symbol) => symbol,
            None => return String::from("[[[UNKNOWN, UNTRACKED SYMBOL]]]"),
        };
        let mut string = String::new();
        // Print the name of the module.
        if symbol.kind.is_public()
            && !(matches!(
                symbol.kind,
                SemanticSymbolKind::Property { .. }
                    | SemanticSymbolKind::Import { .. }
                    | SemanticSymbolKind::Module { .. }
            ))
            && !*self.is_opaque.borrow()
        {
            if let Some(reference) = symbol.references.first() {
                if let Some(module) = self.standpoint.module_map.get(reference.module_path) {
                    string = self.print_symbol_with_idx(module.symbol_idx);
                    string.push('\n');
                }
            };
        }
        match &symbol.kind {
            SemanticSymbolKind::Module { .. } => {
                // todo: print full path.
                string.push_str("module ");
                let standpoint = self.standpoint;
                let mut rope = vec![];
                let mut curr_symbol = symbol;
                loop {
                    let module = standpoint
                        .module_map
                        .get(curr_symbol.references.first().unwrap().module_path)
                        .unwrap();
                    let mut parent_directory = get_parent_dir(&module.path_buf);
                    if let Some(dir) = parent_directory.as_ref() {
                        if dir
                            .file_name()
                            .and_then(|name| name.to_str())
                            .is_some_and(|name| name == curr_symbol.name)
                        {
                            parent_directory = get_parent_dir(dir);
                        }
                    }
                    let parent_module_symbol = get_directory_symbol(parent_directory, standpoint);
                    if let Some(parent_symbol) = parent_module_symbol {
                        rope.push(parent_symbol.name.to_owned());
                        curr_symbol = parent_symbol;
                    } else {
                        break;
                    }
                }
                for modulename in rope.iter().rev() {
                    string.push_str(modulename);
                    string.push_str(".")
                }
                string.push_str(&symbol.name);
            }
            SemanticSymbolKind::Interface {
                is_public,
                generic_params,
                ..
            } => {
                if *is_public {
                    string.push_str("public ");
                }
                string.push_str("interface ");
                string.push_str(&symbol.name);
                self.maybe_print_generic_params_into_string(&mut string, generic_params);
            }
            SemanticSymbolKind::Model {
                is_public,
                generic_params,
                ..
            } => {
                if *is_public {
                    string.push_str("public ");
                }
                string.push_str("model ");
                string.push_str(&symbol.name);
                self.maybe_print_generic_params_into_string(&mut string, generic_params);
            }
            SemanticSymbolKind::Enum {
                is_public,
                generic_params,
                ..
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
                tagged_types,
                ..
            } => {
                let name_of_owner = &self
                    .standpoint
                    .symbol_library
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
                pattern_type,
            } => {
                if *is_public {
                    string.push_str("public ")
                }
                string.push_str("var ");
                string.push_str(&symbol.name);
                string.push_str(": ");
                // Always favor displaying the inferred type over the declared one.
                if !matches!(inferred_type, EvaluatedType::Unknown { .. }) {
                    let type_as_string = self
                        .standpoint
                        .symbol_library
                        .format_evaluated_type(inferred_type);
                    string.push_str(&type_as_string);
                } else if let Some(typ) = declared_type {
                    string.push_str(&self.print_intermediate_type(typ));
                    match pattern_type {
                        VariablePatternForm::Normal => {}
                        VariablePatternForm::DestructuredFromObject { .. } => {
                            string.push_str("  [(property)]")
                        }
                        VariablePatternForm::DestructuredFromArray => string.push_str("[item]"),
                    }
                } else {
                    // could not infer type
                    string.push_str("{unknown}")
                }
            }
            SemanticSymbolKind::LoopVariable {
                inferred_type,
                pattern_type,
            } => {
                string.push_str(&symbol.name);
                string.push_str(": ");
                // Always favor displaying the inferred type over the declared one.
                if !matches!(inferred_type, EvaluatedType::Unknown { .. }) {
                    let type_as_string = self
                        .standpoint
                        .symbol_library
                        .format_evaluated_type(inferred_type);
                    string.push_str(&type_as_string);
                } else {
                    match pattern_type {
                        VariablePatternForm::Normal => string.push_str("{unknown}"),
                        VariablePatternForm::DestructuredFromObject { .. } => {
                            string.push_str("  [(property)]")
                        }
                        VariablePatternForm::DestructuredFromArray => string.push_str("[item]"),
                    }
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
                string.push_str(": ");
                // Always favor displaying the inferred type over the declared one.
                if !matches!(inferred_type, EvaluatedType::Unknown { .. }) {
                    let type_as_string = self
                        .standpoint
                        .symbol_library
                        .format_evaluated_type(inferred_type);
                    string.push_str(&type_as_string);
                } else {
                    string.push_str(&self.print_intermediate_type(declared_type))
                }
            }
            SemanticSymbolKind::Attribute {
                owner_model,
                is_public,
                declared_type,
                ..
            } => {
                string = self.print_symbol_with_idx(*owner_model);
                string.push('\n');
                if *is_public {
                    string.push_str("public ");
                }
                string.push_str("var ");
                string.push_str(&symbol.name);
                string.push_str(": ");
                string.push_str(&self.print_intermediate_type(declared_type))
            }
            SemanticSymbolKind::Method {
                is_public,
                is_static,
                is_async,
                owner_model_or_interface,
                params,
                generic_params,
                return_type,
                ..
            } => {
                if !*self.is_opaque.borrow() {
                    string = self.print_symbol_with_idx(*owner_model_or_interface);
                    string.push('\n');
                }
                if *is_public {
                    string.push_str("public ");
                }
                if *is_static {
                    string.push_str("static ");
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
            SemanticSymbolKind::Parameter {
                is_optional,
                param_type,
                inferred_type,
            } => {
                string.push_str(&symbol.name);
                if *is_optional {
                    string.push('?')
                }
                string.push_str(": ");
                if let Some(label) = param_type.as_ref() {
                    string.push_str(&self.print_intermediate_type(label));
                } else {
                    string.push_str(
                        &self
                            .standpoint
                            .symbol_library
                            .format_evaluated_type(inferred_type),
                    );
                }
            }
            SemanticSymbolKind::GenericParameter {
                interfaces,
                default_value,
                ..
            } => {
                string.push_str(&symbol.name);
                if interfaces.len() > 0 {
                    string.push_str(" implements ");
                    for (i, interface_) in interfaces.iter().enumerate() {
                        string.push_str(&self.print_intermediate_type(interface_));
                        if i + 1 != interfaces.len() {
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
                string.push_str(": {unknown}");
            }
            SemanticSymbolKind::Import { is_public, source } => match source {
                Some(source) => return self.print_symbol_with_idx(*source),
                None => {
                    if *is_public {
                        string.push_str("public ");
                    }
                    string.push_str("use ");
                    string.push_str(&symbol.name);
                    string.push_str(": {unknown}");
                }
            },
            SemanticSymbolKind::Property {
                resolved,
                is_opaque,
            } => match resolved {
                Some(idx) => {
                    *self.is_opaque.borrow_mut() = *is_opaque;
                    string = self.print_symbol_with_idx(*idx);
                }
                _ => {
                    string.push_str("(property) ");
                    string.push_str(&symbol.name);
                    string.push_str(": {unknown}")
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
            string.push_str(" -> ");
            string.push_str(&self.print_intermediate_type(rettype))
        }
    }
    /// Prints an intermediate type using the symbol table.
    pub fn print_intermediate_type(&self, typ: &IntermediateType) -> String {
        match typ {
            IntermediateType::FunctionType {
                params,
                return_type,
                ..
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
                    string.push_str(" -> ");
                    string.push_str(&self.print_intermediate_type(rettype))
                }
                string
            }
            IntermediateType::SimpleType {
                value,
                generic_args,
                ..
            } => {
                let symbol = self.standpoint.symbol_library.get(*value).unwrap();
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
            IntermediateType::Placeholder => {
                unreachable!("Attempted to print a placeholder intermediate type.")
            }
            IntermediateType::MemberType {
                object, property, ..
            } => {
                format!(
                    "{}.{}",
                    self.print_intermediate_type(object),
                    self.print_intermediate_type_property(property)
                )
            }
            IntermediateType::ArrayType { element_type, .. } => {
                let mut string = String::from("[]");
                let mut parenthesize = false;
                match **element_type {
                    IntermediateType::UnionType { .. } | IntermediateType::FunctionType { .. } => {
                        parenthesize = true;
                    }
                    _ => {}
                };
                if parenthesize {
                    string.push_str("(")
                }
                string.push_str(self.print_intermediate_type(element_type).as_str());
                if parenthesize {
                    string.push_str(")")
                }
                string
            }
            IntermediateType::MaybeType { value, .. } => {
                let mut string = String::from("?");
                let mut parenthesize = false;
                match **value {
                    IntermediateType::UnionType { .. }
                    | IntermediateType::FunctionType { .. }
                    | IntermediateType::ArrayType { .. } => {
                        parenthesize = true;
                    }
                    _ => {}
                };
                if parenthesize {
                    string.push_str("(")
                }
                string.push_str(self.print_intermediate_type(&value).as_str());
                if parenthesize {
                    string.push_str(")")
                }
                string
            }
        }
    }

    pub fn print_intermediate_type_property(&self, prop: &IntermediateTypeProperty) -> String {
        let mut string = prop.name.clone();
        let generic_args = &prop.generic_args;
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

    pub fn print_parameter_type(&self, param: &ParameterType) -> String {
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

fn get_directory_symbol<'a>(
    path: Option<&'a Path>,
    standpoint: &'a Standpoint,
) -> Option<&'a analyzer::SemanticSymbol> {
    let symbollib = &standpoint.symbol_library;
    let module_map = &standpoint.module_map;
    path.and_then(|path| {
        standpoint
            .directories
            .get(path)
            .map(|dirmap| (path, dirmap))
    })
    .and_then(|(path, dirmap)| {
        get_dir_basename(path)
            .and_then(|dirname| dirmap.get(dirname))
            .and_then(|pathidx| module_map.get(*pathidx))
            .and_then(|module| symbollib.get(module.symbol_idx))
    })
}
