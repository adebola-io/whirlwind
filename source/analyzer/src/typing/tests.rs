#![cfg(test)]

use errors::TypeErrorType;

use crate::{DiagnosticType, Error, Module, SemanticSymbolKind, Standpoint, CORE_LIBRARY_PATH};

use std::path::PathBuf;

macro_rules! text_produces_errors{
    ($text: expr, $errors: expr) => {{
        let mut module = Module::from_text($text);
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let mut standpoint = Standpoint::new(true, Some(CORE_LIBRARY_PATH.into()));
        standpoint.validate();
        standpoint.add_module(module);
        standpoint.validate();
        for error in $errors {
            if !standpoint.diagnostics.iter().any(
                |prog_diagnostic| matches!(&prog_diagnostic._type, DiagnosticType::Error(Error::Typing(e)) if &e._type == error),
            ) {
                panic!("Error not produced {error:?}")
            }
        }
    }}
}

macro_rules! check_types {
    ($text: expr, $types: expr) => {
        let mut module = Module::from_text($text);
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let mut standpoint = Standpoint::new(true, Some(CORE_LIBRARY_PATH.into()));
        standpoint.validate();
        let path_idx = standpoint.add_module(module).unwrap();
        standpoint.validate();
        let symbol_library = &standpoint.symbol_library;
        for symbol in symbol_library.in_module(path_idx) {
            for (symbol_name, type_value) in $types {
                if symbol_name == &symbol.name {
                    let inferred_type = match &symbol.kind {
                        SemanticSymbolKind::Variable { inferred_type, .. }
                        | SemanticSymbolKind::Constant { inferred_type, .. } => inferred_type,
                        _ => {
                            println!("{symbol:#?}");
                            unreachable!("Symbol for {symbol_name} is not a variable or constant.")
                        }
                    };
                    let inferred_type_symbol = symbol_library.format_evaluated_type(inferred_type);
                    assert_eq!(
                        &inferred_type_symbol, type_value,
                        "Errors: {:#?}",
                        standpoint.diagnostics
                    );
                }
            }
        }
    };
}

#[test]
fn it_solves_generics() {}

#[test]
fn it_solves_interface_impls_for_generics() {}

#[test]
fn it_solves_assignment() {}

#[test]
fn it_creates_intrinsic_instances() {
    check_types!(
        "
    module Test;

    function main() {
        boolean := true;
        str := \"Hello, World\";
        num := 34;
    }
    ",
        &[("boolean", "Bool"), ("str", "String"), ("num", "UInt8")]
    );
}

#[test]
fn it_creates_instances() {
    check_types!(
        "
    module Test;

    model Person {
        public var name: String;
        new(name: String) {
            this.name = name;
        }
    }

    function main() {
        var person = new Person();
        var { name as personName } = new Person(\"John Doe\");
    }    
    ",
        &[("person", "Person"), ("personName", "String")]
    );
}

#[test]
fn it_blocks_type_as_values() {}

#[test]
fn it_blocks_value_as_types() {}

#[test]
fn it_blocks_method_mutation() {}

#[test]
fn it_errors_for_incomplete_interface_impls() {}

#[test]
fn it_allows_interface_method_overriding() {
    // in a model.

    // in another interface.
}

#[test]
fn it_errors_for_missing_else() {
    // in free variable.

    // in assignment.
}

#[test]
fn it_errors_for_incompatble_block_type() {}

#[test]
fn it_errors_on_string_and_number_binexp() {
    text_produces_errors!(
        "
    module Test;

    function main() {
         a := \"Hello\";
         b := 902;
         c := a == b;
    }
    ",
        &[TypeErrorType::Incomparable {
            left: format!("String"),
            right: format!("UInt16")
        }]
    );
}

#[test]
fn ordering_types() {
    check_types!(
        "module Test;
        
        function main() {
            a /*: UInt8 */ := 30;
            b /*: UInt16 */ := 300;

            c := b > a;       
        }
        ",
        &[("a", "UInt16"), ("b", "UInt16"), ("c", "Bool")]
    );
}

#[test]
fn sequencing_types() {
    check_types!(
        "module Test;
        
        function main() {
            a /*: UInt8 */ := 30;
            b /*: UInt16 */ := 300;

            c := a..b;       
        }
        ",
        &[("a", "UInt16"), ("b", "UInt16"), ("c", "Range<UInt16>")]
    );
}

#[test]
fn other_binary_operations() {
    check_types!(
        "module Test;
        
        function main() {
            a /*: UInt8 */ := 30;
            b /*: UInt16 */ := 300;

            c := (a + b) - (a * b);
            d := a % 10;
            e := d ^ 0.5;       
        }
        ",
        &[
            ("a", "UInt16"),
            ("b", "UInt16"),
            ("c", "UInt16"),
            ("d", "Float"),
            ("e", "Float")
        ]
    );
}

#[test]
fn it_errors_on_invalid_unary_exp() {}

#[test]
fn it_errors_on_borrowed_instead_of_owned() {}

#[test]
fn it_errors_on_incompatible_return_type() {}

#[test]
fn it_errors_on_unknown_property() {}

#[test]
fn it_errors_on_mismatched_generics() {}

#[test]
fn it_errors_on_creating_new_instance_on_non_model_value() {}

#[test]
fn it_errors_on_unification_to_never() {}

#[test]
fn it_allows_unification_from_never() {}

#[test]
fn it_errors_on_strings_literals_if_model_not_present() {}

#[test]
fn it_errors_on_array_literals_if_model_not_present() {}

#[test]
fn it_errors_on_boolean_literals_if_model_not_present() {}

#[test]
fn it_errors_on_numeric_literals_if_model_not_present() {}

#[test]
fn it_errors_on_incompatible_numeric_type() {}

#[test]
fn it_errors_for_uncombinable_unions() {}

#[test]
fn it_errors_for_self_referential_types() {}

#[test]
fn it_resolves_infinitely_cyclic_types_to_never() {}

#[test]
fn it_errors_for_uninferable_types() {}

#[test]
fn it_maintains_generic_parameter_invariance() {}

#[test]
fn it_infers_anonymous_function_parameter_types() {}

#[test]
fn it_errors_on_attribute_usage_before_assign() {}

#[test]
fn it_errors_on_recursive_models() {}

#[test]
fn it_creates_enum_instances() {
    check_types!(
        "
    module Test;

    enum Room {
        Kitchen,
        Bathroom,
        Bedroom,
    }
    function main() {
        room := Room.Kitchen;
        room = Room.Bathroom;
    }
    ",
        &[("room", "Room")]
    );
}
