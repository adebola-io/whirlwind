use whirl_ast::{ScopeManager, TypeEval};

/// Stringify a type evaluation.
pub fn stringify_type_eval(scope_manager: &ScopeManager, eval: &TypeEval) -> String {
    match eval {
        TypeEval::Pointer {
            address: scope_address,
            args: generic_args,
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
