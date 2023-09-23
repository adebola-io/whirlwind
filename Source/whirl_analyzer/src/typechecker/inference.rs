// use std::cell::RefCell;
// use whirl_ast::{ASTVisitorExprOutputNoArgs, ModuleAmbience, ScopeEntry, Statement, TypeEval};

// /// Resolves all values in the program and infers their types.
// /// It is technically 50% of the typechecker and control flow analyzer, since it
// /// also checks function return types, if expressions, etc. but eh.
// ///
// /// It is at this stage that the pipeline stops being lazy and relies on
// /// the completion of the previous phase.
// pub struct Program {
//     /// whether or not to push type errors.
//     pub silent: RefCell<bool>,
// }

// impl Program {
//     /// Resolve all variables and types in the list of statements and module ambience.
//     pub fn infer(&mut self) {
//         self.statements
//             .iter()
//             .for_each(|statement| self.statement(statement));
//         if self.ambience().get_module_name().is_none() {
//             self.type_errors
//                 .borrow_mut()
//                 .push(whirl_errors::nameless_module())
//         }
//     }

// }

// // impl TypeInferrer {
// //     fn add_error(&self, error: TypeError) {
// //         if !*self.silent.borrow() {
// //             self.type_errors.borrow_mut().push(error);
// //         }
// //     }
// //     fn toggle_silence(&self) {
// //         self.silent.replace(!*self.silent.borrow());
// //     }
// // }

// // impl ASTVisitorExprOutputNoArgs<TypeEval> for TypeInferrer {
// //     /// Perform inference on a shorthand variable declaration.
// //     fn shorthand_var_decl(&self, var_decl: &whirl_ast::ShorthandVariableDeclaration) {
// //         let address = var_decl.address;
// //         let value_type = self.expr(&var_decl.value);
// //         let signature = self.ambience().get_entry_unguarded_mut(address).var_mut();

// //         if let Some(ref type_expression) = signature.var_type.declared {
// //             // Check if assigned type is valid in the given scope.
// //             match type_utils::infer_type_expression(
// //                 self.ambience(),
// //                 type_expression,
// //                 address.scope_id,
// //             ) {
// //                 Err(error) => {
// //                     signature.var_type.inferred = Some(TypeEval::Invalid);
// //                     self.add_error(error);
// //                 }
// //                 Ok(expression_as_eval) => {
// //                     // Fuse the value and declared types.
// //                     match type_utils::assign_right_to_left(
// //                         expression_as_eval,
// //                         value_type,
// //                         var_decl.span,
// //                     ) {
// //                         Ok(eval) => signature.var_type.inferred = Some(eval),
// //                         Err(error) => self.add_error(error),
// //                     }
// //                 }
// //             }
// //         } else {
// //             // todo: uninferrable generics.
// //             signature.var_type.inferred = Some(value_type);
// //         }
// //     }

// //     /// Perform inference on a function declaration.
// //     fn function_declaration(&self, function: &whirl_ast::FunctionDeclaration) {
// //         let module_ambience = self.ambience();
// //         // Input parameters into body scope.
// //         module_ambience.jump_to_scope(function.body.scope_id);
// //         let parameters = &mut module_ambience
// //             .get_entry_unguarded_mut(function.address)
// //             .func_mut()
// //             .params;
// //         let module_ambience = self.ambience(); // unrecommended FU to rust.

// //         // resolve parameter types.
// //         for parameter in parameters.iter_mut() {
// //             // The parameter could have been already inferred if the function was called from a previous statement.
// //             if let None = parameter.type_label.inferred {
// //                 parameter.type_label.inferred = Some(
// //                     match type_utils::infer_parameter_type(
// //                         parameter,
// //                         function.address.scope_id,
// //                         &module_ambience,
// //                     ) {
// //                         Ok(eval) => eval,
// //                         Err(err) => {
// //                             self.add_error(err);
// //                             TypeEval::Invalid
// //                         }
// //                     },
// //                 )
// //             }

// //             module_ambience.register(ScopeEntry::Parameter(parameter.clone()));
// //         }
// //         for (_index, statement) in function.body.statements.iter().enumerate() {
// //             self.statement(statement)
// //         }
// //         self.ambience().leave_scope()
// //     }

// //     /// Perform inference on a test declaration.
// //     fn test_declaration(&self, test_decl: &whirl_ast::TestDeclaration) {
// //         if !self.ambience().is_in_global_scope() {
// //             self.type_errors
// //                 .borrow_mut()
// //                 .push(whirl_errors::test_in_non_global_scope(test_decl.span))
// //         }
// //         self.ambience().jump_to_scope(test_decl.body.scope_id);
// //         for statement in &test_decl.body.statements {
// //             self.statement(statement)
// //         }
// //         self.ambience().leave_scope();
// //     }

// //     /// Perform inference on an access expression.
// //     fn access(&self, access_expr: &whirl_ast::AccessExpr) -> TypeEval {
// //         let object = self.expr(&access_expr.object);
// //         let property_name = match &access_expr.property {
// //             whirl_ast::Expression::Identifier(identifier) => identifier,
// //             _ => unreachable!(),
// //         };
// //         let propname_str = property_name.name.as_str();
// //         match &object {
// //             TypeEval::ModelInstance {
// //                 model_address,
// //                 args,
// //             } => {
// //                 // todo: generic args.
// //                 let model_signature = self
// //                     .ambience()
// //                     .get_entry_unguarded_mut(*model_address)
// //                     .model();
// //                 let model_name = model_signature.name.name.as_str();
// //                 // Check for attribute name.
// //                 if let Some((idx, attribute_signature)) = model_signature
// //                     .attributes
// //                     .iter()
// //                     .enumerate()
// //                     .find(|attribute| attribute.1.name.name == propname_str)
// //                 {
// //                     self.confirm_public(*model_address, attribute_signature, property_name.span);
// //                     let typeof = model_signature.
// //                     return TypeEval::AttributeOnInstance {
// //                         model_address: *model_address,
// //                         attrib_no: idx,
// //                         generic_args: None, // todo: generics in attribute.
// //                     };
// //                 }
// //                 // Check for method.
// //                 if let Some((idx, method_signature)) = model_signature
// //                     .methods
// //                     .iter()
// //                     .enumerate()
// //                     .find(|method| method.1.name.name == propname_str)
// //                 {
// //                     // static call.
// //                     if method_signature.is_static {
// //                         self.add_error(whirl_errors::instance_static_method_access(
// //                             model_name.to_owned(),
// //                             propname_str.to_owned(),
// //                             access_expr.span,
// //                         ));
// //                         return TypeEval::Invalid;
// //                     }
// //                     // private method call.
// //                     self.confirm_public(*model_address, method_signature, access_expr.span);
// //                     return TypeEval::MethodOfInstance {
// //                         model_address: *model_address,
// //                         method_no: idx,
// //                         generic_args: None, // todo: generics in  method.
// //                     };
// //                 }
// //                 // Unknown_property.
// //                 self.add_error(whirl_errors::unknown_property(
// //                     model_name.to_owned(),
// //                     propname_str.to_owned(),
// //                     property_name.span,
// //                 ));
// //                 return TypeEval::Invalid;
// //             }
// //             TypeEval::Unknown | TypeEval::Invalid => {
// //                 self.add_error(whirl_errors::no_type_on_eval(
// //                     object,
// //                     propname_str.to_owned(),
// //                     property_name.span,
// //                 ));
// //                 return TypeEval::Invalid;
// //             }
// //             TypeEval::ModelConstructor { address } => {
// //                 let model_signature = self.ambience().get_entry_unguarded_mut(*address).model();
// //                 let model_name = model_signature.name.name.as_str();
// //                 // Check for attribute name.
// //                 if let Some(attribute_signature) = model_signature
// //                     .attributes
// //                     .iter()
// //                     .find(|attribute| attribute.name.name == propname_str)
// //                 {
// //                     self.add_error(whirl_errors::attribute_access_on_contructor(
// //                         model_name.to_owned(),
// //                         propname_str.to_owned(),
// //                         access_expr.span,
// //                     ));
// //                     return TypeEval::Invalid;
// //                 }
// //                 // check for method name.
// //                 if let Some((idx, method_signature)) = model_signature
// //                     .methods
// //                     .iter()
// //                     .enumerate()
// //                     .find(|method| method.1.name.name == propname_str)
// //                 {
// //                     // non - static call.
// //                     if !method_signature.is_static {
// //                         self.add_error(whirl_errors::contructor_non_static_method_access(
// //                             model_name.to_owned(),
// //                             propname_str.to_owned(),
// //                             access_expr.span,
// //                         ));
// //                         return TypeEval::Invalid;
// //                     }
// //                     // private method call.
// //                     if !method_signature.is_public {
// //                         let contextual_model_address = self.ambience().get_surrounding_model();
// //                         if contextual_model_address.is_none()
// //                             || contextual_model_address.unwrap() != address
// //                         {
// //                             self.add_error(whirl_errors::private_property_leak(
// //                                 model_name.to_owned(),
// //                                 propname_str.to_owned(),
// //                                 access_expr.span,
// //                             ))
// //                         }
// //                     }
// //                     return TypeEval::MethodOfInstance {
// //                         model_address: address,
// //                         method_no: idx,
// //                         generic_args: None, // todo: generics in  method.
// //                     };
// //                 }
// //                 // Unknown_property.
// //                 self.add_error(whirl_errors::unknown_property(
// //                     model_name.to_owned(),
// //                     propname_str.to_owned(),
// //                     property_name.span,
// //                 ));
// //                 return TypeEval::Invalid;
// //             }
// //             TypeEval::TraitConstructor { .. } => {
// //                 self.add_error(whirl_errors::accessing_on_trait(object, access_expr.span));
// //                 return TypeEval::Invalid;
// //             }
// //             TypeEval::EnumConstructor { .. } => {
// //                 // Todo: Variants
// //                 return TypeEval::Unknown;
// //             }
// //             TypeEval::TypeAlias { .. } => {
// //                 self.add_error(whirl_errors::type_as_value(
// //                     object,
// //                     access_expr.object.span(),
// //                 ));
// //                 return TypeEval::Invalid;
// //             }
// //             TypeEval::MethodOfInstance {
// //                 method_no,
// //                 generic_args: args,
// //                 ..
// //             } => {
// //                 let model_signature = self.ambience().get_entry_unguarded_mut(address).model();
// //                 let method_name = &model_signature.methods[method_no].name.name;
// //                 self.add_error(whirl_errors::method_accessed_as_object(
// //                     method_name.to_owned(),
// //                     access_expr.object.span(),
// //                 ));
// //                 return TypeEval::Invalid;
// //             }
// //         }
// //     }

// //     /// Perform inference on an expression statement.
// //     fn expr_statement(&self, exp: &whirl_ast::Expression) {
// //         let ambience = self.ambience();

// //         if ambience.is_in_global_scope() {
// //             self.type_errors
// //                 .borrow_mut()
// //                 .push(whirl_errors::global_control(exp.span()));
// //         }

// //         self.expr(exp);
// //     }

// //     /// Perform inference on a free expression.
// //     fn free_expr(&self, exp: &whirl_ast::Expression) {
// //         let ambience = self.ambience();

// //         if ambience.is_in_global_scope() {
// //             self.type_errors
// //                 .borrow_mut()
// //                 .push(whirl_errors::global_control(exp.span()));
// //         }

// //         let free_expression_type = self.expr(exp);

// //         // todo: bubble type to block.
// //         if !free_expression_type.is_invalid() && ambience.is_in_function_context() {
// //             //  todo. compare return types.
// //         }
// //     }

// //     /// Perform inference on a new expression.
// //     fn new_expr(&self, new_expr: &whirl_ast::NewExpr) -> TypeEval {
// //         // Assert than sub expression is a call.
// //         if let whirl_ast::Expression::CallExpr(call_expr) = &new_expr.value {
// //             // Get type of caller.
// //             let model_type = self.expr(&call_expr.caller);
// //             match type_utils::build_model_instance(
// //                 model_type,
// //                 &call_expr.arguments,
// //                 self,
// //                 new_expr.span,
// //             ) {
// //                 Ok(eval) => return eval,
// //                 Err(e) => {
// //                     self.add_error(e);
// //                     return TypeEval::Invalid;
// //                 }
// //             }
// //         } else {
// //             self.type_errors
// //                 .borrow_mut()
// //                 .push(whirl_errors::invalid_new_expression(new_expr.span));
// //             TypeEval::Invalid
// //         }
// //     }

// //     /// Perform inference on an identifier.
// //     fn identifier(&self, ident: &whirl_ast::Identifier) -> TypeEval {
// //         match type_utils::evaluate_type_of_variable(self.ambience(), ident) {
// //             Ok(eval) => return eval,
// //             Err(error) => {
// //                 self.add_error(error);
// //                 return TypeEval::Invalid;
// //             }
// //         }
// //     }

// //     /// Perform inference on a string.
// //     fn string(&self, _string: &whirl_ast::WhirlString) -> TypeEval {
// //         TypeEval::ModelInstance {
// //             model_address: [0, 0].into(), // scope address for strings.
// //             args: None,
// //         }
// //     }

// //     /// Perform inference on a boolean value.
// //     fn boolean(&self, _bool: &whirl_ast::WhirlBoolean) -> TypeEval {
// //         TypeEval::ModelInstance {
// //             model_address: [0, 2].into(), // scope address for booleans.
// //             args: None,
// //         }
// //     }

// //     /// Perform inference on a binary expression.
// //     fn binary_expr(&self, bin_exp: &whirl_ast::BinaryExpr) -> TypeEval {
// //         let left = self.expr(&bin_exp.left);
// //         let right = self.expr(&bin_exp.right);

// //         // TODO: Trait operator overloads and generic unknowns.
// //         if left != right {
// //             self.type_errors
// //                 .borrow_mut()
// //                 .push(whirl_errors::invalid_binary(
// //                     left,
// //                     bin_exp.operator,
// //                     right,
// //                     bin_exp.span,
// //                 ));
// //             return TypeEval::Invalid;
// //         }
// //         left
// //     }

// //     /// Perform inference on a number.
// //     fn number(&self, _number: &whirl_ast::WhirlNumber) -> TypeEval {
// //         TypeEval::ModelInstance {
// //             model_address: [0, 1].into(), // scope address for integers.
// //             args: None,
// //         }
// //     }
// // }
