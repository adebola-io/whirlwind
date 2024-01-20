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
        standpoint
    }}
}

macro_rules! check_types {
    ($text: expr) => {{
        let mut module = Module::from_text($text);
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let mut standpoint = Standpoint::new(true, Some(CORE_LIBRARY_PATH.into()));
        standpoint.validate();
        standpoint.add_module(module);
        standpoint.validate();
        standpoint
    }};
    ($text: expr, $types: expr) => {{
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
        standpoint
    }};
}

#[test]
fn it_solves_generics() {}

#[test]
fn it_solves_interface_impls_for_generics() {}

#[test]
fn coerces_this_type() {
    check_types!(
        "
    module Test;
    interface Identity {
        public function self() -> This;
    }

    model WrapInt implements Identity {
        new() {}
        public function [Identity.self]() -> This {
            todo()
        }
    }

    model Object<T implements Identity> {
        var value: T;
        new(value: T) {
            this.value = value;
        }
        public function val() -> T {
            return this.value.self();
        }
    }

    function main() {
        obj := Object(WrapInt());
        objValue := obj.val();
    }
    ",
        &[("objValue", "WrapInt")]
    );
}

#[test]
fn conditional_interface_impl() {
    check_types!(
        "
        module Test;

        function displayable<T implements Display>(value: T) -> T {
            return value;
        }
        
        function main() {
            // should implement Display, because child implements Display.
            a := [\"hello, world\"];
            b := displayable(a);
            // should implement Display, because internal value implements Display.
            c := some(true);
            d := displayable(c);
        }
        ",
        &[("b", "Array<String>"), ("d", "Maybe<Bool>")]
    );
}

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
        &[("boolean", "Bool"), ("str", "String"), ("num", "Int32")]
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
        var person = Person();
        var { name as personName } = Person(\"John Doe\");
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
            right: format!("Int32")
        }]
    );
}

#[test]
fn ordering_types() {
    check_types!(
        "module Test;
        
        function main() {
            a /*: Int32 */ := 30;
            b /*: Int32 */ := 300;

            c := b > a;       
        }
        ",
        &[("a", "Int32"), ("b", "Int32"), ("c", "Bool")]
    );
}

#[test]
fn sequencing_types() {
    check_types!(
        "module Test;
        
        function main() {
            a /*: Int32 */ := 30;
            b /*: Int32 */ := 300;

            c := a..b;       
        }
        ",
        &[("a", "Int32"), ("b", "Int32"), ("c", "Range<Int32>")]
    );
}

#[test]
fn other_binary_operations() {
    check_types!(
        "module Test;
        
        function main() {
            a /*: Int32 */ := 30;
            b /*: Int32 */ := 300;

            c := (a + b) - (a * b);
            d := a % 10;
            e := d ^ 0.5;       
        }
        ",
        &[
            ("a", "Int32"),
            ("b", "Int32"),
            ("c", "Int32"),
            ("d", "Float64"),
            ("e", "Float64")
        ]
    );
}

#[test]
fn test_assignment_types() {
    let standpoint = check_types!(
        "module Test;
        
        function main() {
            a := 0;
            a += 9; // valid.

            a += 0.293; // valid.
            
            string := \"Hello, world.\";
            string += \"Welcome.\";
        }
        ",
        &[("a", "Float64"), ("string", "String")]
    );
    assert_eq!(
        standpoint
            .diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.is_error())
            .count(),
        0
    );
}

#[test]
fn unary_minus_or_plus() {
    let standpoint = check_types!(
        "module Test;
        
        function main() {
            a := 20;
            a = -99;

            b := 800;
            b = -b;
        }
        ",
        &[("a", "Int32"), ("b", "Int32")]
    );
    assert_eq!(
        standpoint
            .diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.is_error())
            .count(),
        0
    );

    text_produces_errors!(
        "module Test;
        
        function main() {
            a := 'Hello, world';
            a = -a;
        }
        ",
        &[TypeErrorType::NumericExclusiveOperation {
            typ: format!("String")
        }]
    );
}

#[test]
fn it_infers_default_generic_arguments() {
    check_types!(
        "module Test;
        
        function main() {
            a := Generic();
        }

        function Generic<T = String>(value?: T): T {
            todo()
        }
        ",
        &[("a", "String")]
    );
}

#[test]
fn it_typechecks_type_declaration() {
    text_produces_errors!(
        "module Test;

        type BoolIterator = Iteratable<Bool>;

        function main() {
            
        }
        ",
        &[TypeErrorType::ExpectedImplementableGotSomethingElse(
            format!("Iteratable<Bool>")
        )]
    );
}

#[test]
fn method_inherits_generic_arguments() {
    check_types!(
        "
    module Test;

    // Method generic inheritance.
    model GenericModel<T> {
        var value: T;
        new(value: T) {
            this.value = value
        }
        public function getValue() -> T {
            return this.value
        }
        public function getStringClone() -> GenericModel<String> {
            todo()
        }
    }

    genericModelInst := GenericModel(true);
    outerValue := genericModelInst.getValue();
    strClone := genericModelInst.getStringClone();

    tuple := Tuple(true, 'story');
    swapped := tuple.swap();
    ",
        &[
            ("outerValue", "Bool"),
            ("strClone", "GenericModel<String>"),
            ("swapped", "Tuple<String, Bool>")
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
