#![cfg(test)]

#[test]
fn it_solves_generics() {}

#[test]
fn it_solves_trait_impls_for_generics() {}

#[test]
fn it_solves_assignment() {}

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
