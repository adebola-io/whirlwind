// use whirl_ast::{
//     EnumSignature, ModelSignature, ModuleAmbience, TraitSignature, TypeEval, TypeSignature,
// };

// /// Stringify a type evaluation.
// pub fn stringify_type_eval(module_ambience: &ModuleAmbience, eval: &TypeEval) -> String {
//     match eval {
//         TypeEval::ModelInstance {
//             model_address: scope_address,
//             args: generic_args,
//         } => {
//             let mut string = String::new();
//             let entry = module_ambience
//                 .get_scope(scope_address.scope_id)
//                 .unwrap()
//                 .get_entry(scope_address.entry_no)
//                 .unwrap();

//             string.push_str(entry.name());
//             if let Some(args) = generic_args {
//                 string.push('<');
//                 for (index, arg) in args.iter().enumerate() {
//                     string.push_str(&stringify_type_eval(module_ambience, arg));
//                     if index + 1 != args.len() {
//                         string.push_str(", ");
//                     }
//                 }
//                 string.push('>');
//             }
//             string
//         }
//         TypeEval::EnumConstructor { address } => {
//             let mut string = String::new();
//             let entry = module_ambience
//                 .get_scope(address.scope_id)
//                 .unwrap()
//                 .get_entry(address.entry_no)
//                 .unwrap();
//             string.push_str("enum ");
//             string.push_str(entry.name());
//             string
//         }
//         TypeEval::ModelConstructor { address } => {
//             let mut string = String::new();
//             let entry = module_ambience
//                 .get_scope(address.scope_id)
//                 .unwrap()
//                 .get_entry(address.entry_no)
//                 .unwrap();
//             string.push_str("model ");
//             string.push_str(entry.name());
//             string
//         }
//         TypeEval::TraitConstructor { address } => {
//             let mut string = String::new();
//             let entry = module_ambience
//                 .get_scope(address.scope_id)
//                 .unwrap()
//                 .get_entry(address.entry_no)
//                 .unwrap();
//             string.push_str("trait ");
//             string.push_str(entry.name());
//             string
//         }
//         TypeEval::TypeAlias { address } => {
//             let mut string = String::new();
//             let entry = module_ambience
//                 .get_scope(address.scope_id)
//                 .unwrap()
//                 .get_entry(address.entry_no)
//                 .unwrap();

//             string.push_str(entry.name());
//             match entry {
//                 whirl_ast::ScopeEntry::Type(TypeSignature { generic_params, .. })
//                 | whirl_ast::ScopeEntry::Model(ModelSignature { generic_params, .. })
//                 | whirl_ast::ScopeEntry::Enum(EnumSignature { generic_params, .. })
//                 | whirl_ast::ScopeEntry::Trait(TraitSignature { generic_params, .. }) => {
//                     if let Some(params) = generic_params {
//                         string.push('<');
//                         for (index, param) in params.iter().enumerate() {
//                             string.push_str(&param.name.name);
//                             if index + 1 != params.len() {
//                                 string.push_str(", ");
//                             }
//                         }
//                         string.push('>');
//                     }
//                 }
//                 _ => unreachable!(),
//             }

//             string
//         }
//         TypeEval::Invalid => format!("invalid"),
//         TypeEval::Unknown => format!("unknown"),
//     }
// }
