use std::cell::RefCell;

use whirl_ast::{
    ASTVisitorExprOutputNoArgs, ModuleAmbience, ScopeEntry, Spannable, Statement, TypeEval,
};
use whirl_errors::{LexError, ParseError};
use whirl_lexer::Lexer;
use whirl_parser::parse_text;

use crate::{primitive::Primitives, type_utils};

use whirl_errors::TypeError;

/// Resolves all values in the program and infers their types.
/// It is technically 50% of the typechecker and control flow analyzer, since it
/// also checks function return types, if expressions, etc. but eh.
///
/// It is at this stage that the pipeline stops being lazy and relies on
/// the completion of the previous phase.
pub struct TypeInferrer {
    pub ambience: RefCell<ModuleAmbience>,
    pub statements: Vec<Statement>,
    pub type_errors: RefCell<Vec<TypeError>>,
    pub lexical_errors: Vec<LexError>,
    pub syntax_errors: Vec<ParseError>,
    pub line_lengths: Vec<u32>,
}

impl TypeInferrer {
    /// Resolve all variables and types in the list of statements and module ambience.
    pub fn infer(&mut self) {
        self.statements
            .iter()
            .for_each(|statement| self.statement(statement))
    }

    /// Returns the module ambience.
    pub fn ambience(&self) -> &mut ModuleAmbience {
        unsafe { &mut *(self.ambience.as_ptr()) }
    }

    /// lex, parse and build an inference machine from text.
    pub fn from_text(input: &str, primitives: Primitives) -> TypeInferrer {
        let mut parser = parse_text(input);
        let mut syntax_errors = vec![];
        let mut statements = vec![];
        loop {
            match parser.next() {
                Some(mut partial) => {
                    syntax_errors.append(&mut partial.errors);
                    if let Some(statement) = partial.value {
                        statements.push(statement);
                    }
                }
                None => break,
            }
        }

        let ambience = RefCell::new(std::mem::take(parser.module_ambience()));
        let lexical_errors = std::mem::take(parser.lexer.borrow_mut().errors());
        let line_lengths = std::mem::take(&mut parser.lexer.borrow_mut().line_lengths);

        // Register all primitive values.
        for primitive_type in primitives.types {
            ambience
                .borrow_mut()
                .register(ScopeEntry::Type(primitive_type));
        }
        for primitive_model in primitives.models {
            ambience
                .borrow_mut()
                .register(ScopeEntry::Model(primitive_model));
        }

        TypeInferrer {
            statements,
            type_errors: RefCell::new(vec![]),
            lexical_errors,
            syntax_errors,
            ambience,
            line_lengths,
        }
    }
}

impl ASTVisitorExprOutputNoArgs<TypeEval> for TypeInferrer {
    /// Perform inference on a shorthand variable declaration.
    fn shorthand_var_decl(&self, var_decl: &whirl_ast::ShorthandVariableDeclaration) {
        let address = var_decl.address;
        let value_type = self.expr(&var_decl.value);
        let signature = self.ambience().get_entry_unguarded_mut(address).var_mut();

        if let Some(ref type_expression) = signature.var_type.declared {
            // Check if assigned type is valid in the given scope.
            match type_utils::infer_type_expression(
                self.ambience(),
                type_expression,
                address.scope_id,
            ) {
                Err(error) => {
                    signature.var_type.inferred = Some(TypeEval::Invalid);
                    self.type_errors.borrow_mut().push(error);
                }
                Ok(expression_as_eval) => {
                    // Fuse the value and declared types.
                    match type_utils::assign_right_to_left(
                        expression_as_eval,
                        value_type,
                        var_decl.span,
                    ) {
                        Ok(eval) => signature.var_type.inferred = Some(eval),
                        Err(error) => self.type_errors.borrow_mut().push(error),
                    }
                }
            }
        } else {
            // todo: uninferrable generics.
            signature.var_type.inferred = Some(value_type);
        }
    }

    /// Perform inference on a function declaration.
    fn function_declaration(&self, function: &whirl_ast::FunctionDeclaration) {
        let module_ambience = self.ambience();
        // Input parameters into body scope.
        module_ambience.jump_to_scope(function.body.scope_id);
        let parameters = &mut module_ambience
            .get_entry_unguarded_mut(function.address)
            .func_mut()
            .params;
        let module_ambience = self.ambience(); // unrecommended FU to rust.

        // resolve parameter types.
        for parameter in parameters.iter_mut() {
            // The parameter could have been already inferred if the function was called from a previous statement.
            if let None = parameter.type_label.inferred {
                parameter.type_label.inferred = Some(
                    match type_utils::infer_parameter_type(
                        parameter,
                        function.address.scope_id,
                        &module_ambience,
                    ) {
                        Ok(eval) => eval,
                        Err(err) => {
                            self.type_errors.borrow_mut().push(err);
                            TypeEval::Invalid
                        }
                    },
                )
            }

            module_ambience.register(ScopeEntry::Parameter(parameter.clone()));
        }
        for (_index, statement) in function.body.statements.iter().enumerate() {
            self.statement(statement)
        }
        self.ambience().leave_scope()
    }

    /// Perform inference on a test declaration.
    fn test_declaration(&self, test_decl: &whirl_ast::TestDeclaration) {
        if !self.ambience().is_in_global_scope() {
            self.type_errors
                .borrow_mut()
                .push(whirl_errors::test_in_non_global_scope(test_decl.span))
        }
        self.ambience().jump_to_scope(test_decl.body.scope_id);
        for statement in &test_decl.body.statements {
            self.statement(statement)
        }
        self.ambience().leave_scope();
    }

    /// Perform inference on an expression statement.
    fn expr_statement(&self, exp: &whirl_ast::Expression) {
        let ambience = self.ambience();

        if ambience.is_in_global_scope() {
            self.type_errors
                .borrow_mut()
                .push(whirl_errors::global_control(exp.span()));
        }

        self.expr(exp);
    }

    /// Perform inference on a free expression.
    fn free_expr(&self, exp: &whirl_ast::Expression) {
        let ambience = self.ambience();

        if ambience.is_in_global_scope() {
            self.type_errors
                .borrow_mut()
                .push(whirl_errors::global_control(exp.span()));
        }

        let free_expression_type = self.expr(exp);

        // todo: bubble type to block.
        if !free_expression_type.is_invalid() && ambience.is_in_function_context() {
            //  todo. compare return types.
        }
    }

    /// Perform inference on a new expression.
    fn new_expr(&self, new_expr: &whirl_ast::NewExpr) -> TypeEval {
        // Assert than sub expression is a call.
        if let whirl_ast::Expression::CallExpr(call_expr) = &new_expr.value {
            // Get type of caller.
            let model_type = self.expr(&call_expr.caller);
            match type_utils::build_model_instance(
                model_type,
                &call_expr.arguments,
                self,
                new_expr.span,
            ) {
                Ok(eval) => return eval,
                Err(e) => {
                    self.type_errors.borrow_mut().push(e);
                    return TypeEval::Invalid;
                }
            }
        } else {
            self.type_errors
                .borrow_mut()
                .push(whirl_errors::invalid_new_expression(new_expr.span));
            TypeEval::Invalid
        }
    }

    /// Perform inference on an identifier.
    fn identifier(&self, ident: &whirl_ast::Identifier) -> TypeEval {
        match type_utils::evaluate_type_of_variable(self.ambience(), ident) {
            Ok(eval) => return eval,
            Err(error) => {
                self.type_errors.borrow_mut().push(error);
                return TypeEval::Invalid;
            }
        }
    }

    /// Perform inference on a string.
    fn string(&self, _string: &whirl_ast::WhirlString) -> TypeEval {
        TypeEval::Instance {
            address: [0, 0].into(), // scope address for strings.
            args: None,
        }
    }

    /// Perform inference on a boolean value.
    fn boolean(&self, _bool: &whirl_ast::WhirlBoolean) -> TypeEval {
        TypeEval::Instance {
            address: [0, 2].into(), // scope address for booleans.
            args: None,
        }
    }

    /// Perform inference on a binary expression.
    fn binary_expr(&self, bin_exp: &whirl_ast::BinaryExpr) -> TypeEval {
        let left = self.expr(&bin_exp.left);
        let right = self.expr(&bin_exp.right);

        // TODO: Trait operator overloads and generic unknowns.
        if left != right {
            self.type_errors
                .borrow_mut()
                .push(whirl_errors::invalid_binary(
                    left,
                    bin_exp.operator,
                    right,
                    bin_exp.span,
                ));
            return TypeEval::Invalid;
        }
        left
    }

    /// Perform inference on a number.
    fn number(&self, _number: &whirl_ast::WhirlNumber) -> TypeEval {
        TypeEval::Instance {
            address: [0, 1].into(), // scope address for integers.
            args: None,
        }
    }
}
