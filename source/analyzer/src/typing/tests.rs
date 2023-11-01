#![cfg(test)]

use crate::{
    EvaluatedType, Module, ProgramErrorType, SemanticSymbolKind, Standpoint, CORE_LIBRARY_PATH,
};
use errors::TypeError;
use std::path::PathBuf;

#[track_caller]
macro_rules! text_produces_errors{
    ($text: expr, $errors: expr) => {{
        let mut module = Module::from_text($text.to_string());
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let mut standpoint = Standpoint::new(true, Some(CORE_LIBRARY_PATH.into()));
        standpoint.add_module(module);
        standpoint.check_all_modules();
        for error in $errors {
            if !standpoint.errors.iter().any(
                |prog_error| matches!(&prog_error.error_type, ProgramErrorType::Typing(e) if e == error),
            ) {
                panic!("Error not produced {error:?}")
            }
        }
    }}
}

#[track_caller]
macro_rules! check_types {
    ($text: expr, $types: expr) => {
        let mut module = Module::from_text($text.to_string());
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let mut standpoint = Standpoint::new(true, Some(CORE_LIBRARY_PATH.into()));
        let path_idx = standpoint.add_module(module).unwrap();
        standpoint.check_all_modules();
        let symbol_table = &standpoint.symbol_table;
        for symbol in symbol_table.in_module(path_idx) {
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
                    let inferred_type_symbol = match inferred_type {
                        EvaluatedType::ModelInstance { model, .. } => {
                            symbol_table.get(*model).unwrap()
                        }
                        other => {
                            println!("{symbol:#?}");
                            println!("Errors: {:#?}", standpoint.errors);
                            unreachable!("Inferred type for {symbol_name} is {other:?}, not a model instance.")
                        }
                    };
                    assert_eq!(&inferred_type_symbol.name, type_value);
                }
            }
        }
    };
}

#[test]
fn it_solves_generics() {}

#[test]
fn it_solves_trait_impls_for_generics() {}

#[test]
fn it_solves_assignment() {}

#[test]
fn it_creates_intrinsic_instances() {
    // check_types!(
    //     "
    // module Testing;

    // function Main() {
    //     stringVal := true;
    // }
    // ",
    //     &[("stringVal", "String")]
    // );
}

#[test]
fn it_creates_instances() {}

#[test]
fn it_blocks_type_as_values() {}

#[test]
fn it_blocks_value_as_types() {}

#[test]
fn it_blocks_method_mutation() {}

#[test]
fn it_errors_for_incomplete_trait_impls() {}

#[test]
fn it_allows_trait_method_overriding() {
    // in a model.

    // in another trait.
}

#[test]
fn it_errors_for_missing_else() {
    // in free variable.

    // in assignment.
}

#[test]
fn it_errors_for_incompatble_block_type() {}

#[test]
fn it_errors_on_string_and_number_binexp() {}

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
fn it_errors_for_infinitely_cyclic_types() {}

#[test]
fn it_errors_for_uninferable_types() {}
