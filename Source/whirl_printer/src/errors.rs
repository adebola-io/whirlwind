use whirl_ast::{ScopeManager, TypeEval};
use whirl_semantic::TypeErrorType;

/// Stringify a type error.
pub fn stringify_type_error(scope_manager: &ScopeManager, error: TypeErrorType) -> String {
    match error {
        TypeErrorType::InvalidBinary {
            left,
            operator,
            right,
        } => format!(
            "Operator {:?} is not defined for '{}' and '{}'.",
            operator,
            stringify_type_eval(scope_manager, &left),
            stringify_type_eval(scope_manager, &right)
        ),
        TypeErrorType::AssignedInvalid => {
            format!("The invalid type cannot be used as an expression.")
        }
        TypeErrorType::UnknownType { name } => {
            format!("Cannot resolve type '{name}'")
        }
        TypeErrorType::ValueAsType { name } => {
            format!("{name} refers to a value, but it is being used as a type here.")
        }
    }
}

/// Stringify a type evaluation.
pub fn stringify_type_eval(scope_manager: &ScopeManager, eval: &TypeEval) -> String {
    match eval {
        TypeEval::Pointer {
            scope_address,
            generic_args,
        } => {
            let mut string = String::new();
            let entry = scope_manager
                .get_scope(scope_address.scope_id)
                .unwrap()
                .get_entry(scope_address.entry_no)
                .unwrap();

            string.push_str(entry.name());
            if let Some(args) = generic_args {
                string.push('<');
                for (index, arg) in args.iter().enumerate() {
                    string.push_str(&stringify_type_eval(scope_manager, arg));
                    if index + 1 != args.len() {
                        string.push_str(", ");
                    }
                }
                string.push('>');
            }
            string
        }
        TypeEval::Invalid => format!("invalid"),
    }
}
