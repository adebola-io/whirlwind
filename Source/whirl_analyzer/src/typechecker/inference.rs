use std::{cell::RefCell, str::Chars};

use whirl_ast::{ASTVisitorNoArgs, ModuleAmbience, ScopeEntry, Span, Statement, TypeEval};
use whirl_errors::{LexError, ParseError};
use whirl_lexer::{Lexer, TextLexer};
use whirl_parser::parse_text;

use crate::{primitive::Primitives, type_utils};

use whirl_errors::TypeError;

/// Resolves all values in the program and infers their types.
pub struct TypeInferrer<L: Lexer> {
    pub parser: whirl_parser::Parser<L>,
    pub statements: Vec<Statement>,
    pub type_errors: RefCell<Vec<TypeError>>,
    pub lexical_errors: Vec<LexError>,
    pub syntax_errors: Vec<ParseError>,
}

impl<L: Lexer> TypeInferrer<L> {
    /// Check a statement and return all encountered errors.
    fn check(&mut self, statement: &mut Statement) -> Vec<TypeError> {
        self.statement(statement);
        self.type_errors.take()
    }

    /// Returns the (parser's) module ambience.
    pub fn module_ambience(&self) -> &mut ModuleAmbience {
        self.parser.module_ambience()
    }

    pub fn from_text(input: &str, primitives: Primitives) -> TypeInferrer<TextLexer<Chars>> {
        let checker = TypeInferrer {
            parser: parse_text(input),
            statements: vec![],
            type_errors: RefCell::new(vec![]),
            lexical_errors: vec![],
            syntax_errors: vec![],
        };
        let module_ambience = checker.parser.module_ambience();
        // Register all primitive values.
        for primitive_type in primitives.types {
            module_ambience.register(ScopeEntry::Type(primitive_type));
        }
        for primitive_model in primitives.models {
            module_ambience.register(ScopeEntry::Model(primitive_model));
        }

        checker
    }
}

impl<L: Lexer> Iterator for TypeInferrer<L> {
    type Item = Vec<TypeError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.parser.next() {
                Some(mut partial) => {
                    self.syntax_errors.append(&mut partial.errors);
                    if partial.is_none() {
                        continue;
                    }
                    let mut statement = partial.unwrap();
                    let errors = self.check(&mut statement);
                    self.statements.push(statement);
                    return Some(errors);
                }
                None => {
                    // Collect all lexical errors if parsing ends.
                    self.lexical_errors
                        .append(self.parser.lexer.borrow_mut().errors());
                    return None;
                }
            }
        }
    }
}

impl<L: Lexer> ASTVisitorNoArgs<TypeEval> for TypeInferrer<L> {
    /// Perform inference on a shorthand variable declaration.
    fn shorthand_variable_declaration(
        &self,
        variable_decl: &whirl_ast::ShorthandVariableDeclaration,
    ) {
        let address = variable_decl.address;
        let value_type = self.expr(&variable_decl.value);
        let signature = self
            .module_ambience()
            .get_entry_unguarded_mut(address)
            .var_mut();

        if let Some(ref type_expression) = signature.var_type.declared {
            // Check if assigned type is valid in the given scope.
            match type_utils::eval_type_expression(
                self.module_ambience(),
                type_expression,
                address.scope_id,
            ) {
                Err(error) => {
                    signature.var_type.inferred = Some(TypeEval::Invalid);
                    self.type_errors.borrow_mut().push(error);
                }
                Ok(expression_as_eval) => {
                    // Fuse the value and declared types.
                    match assign_right_to_left(expression_as_eval, value_type, variable_decl.span) {
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
        let module_ambience = self.module_ambience();
        // Input parameters into body scope.
        module_ambience.jump_to_scope(function.body.scope_id);
        let parameters = &module_ambience
            .get_entry_unguarded(function.address)
            .func()
            .params;
        let module_ambience = self.module_ambience(); // unrecommended FU to rust.
        for parameter in parameters.iter() {
            module_ambience.register(ScopeEntry::Parameter(parameter.clone()));
        }
        for (_index, statement) in function.body.statements.iter().enumerate() {
            self.statement(statement)
        }
        self.module_ambience().leave_scope()
    }

    /// Perform inference on an identifier.
    fn identifier(&self, ident: &whirl_ast::Identifier) -> TypeEval {
        match type_utils::evaluate_type_of_variable(self.module_ambience(), ident) {
            Ok(eval) => return eval,
            Err(e) => {
                self.type_errors.borrow_mut().push(e);
                return TypeEval::Invalid;
            }
        }
    }

    /// Perform inference on a string.
    fn string(&self, _string: &whirl_ast::WhirlString) -> TypeEval {
        TypeEval::Pointer {
            address: [0, 0].into(), // scope address for strings.
            args: None,
        }
    }

    /// Perform inference on a boolean value.
    fn boolean(&self, _bool: &whirl_ast::WhirlBoolean) -> TypeEval {
        TypeEval::Pointer {
            address: [0, 2].into(), // scope address for booleans.
            args: None,
        }
    }

    /// Perform inference on a binary expression.
    fn binary_expr(&self, bin_exp: &whirl_ast::BinaryExpr) -> TypeEval {
        let left = self.expr(&bin_exp.left);
        let right = self.expr(&bin_exp.right);

        // TODO: Trait operator overloads.
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
        TypeEval::Pointer {
            address: [0, 1].into(), // scope address for integers.
            args: None,
        }
    }
}

/// Attempt to assign two types together.
fn assign_right_to_left(
    left: TypeEval,
    right: TypeEval,
    span: Span,
) -> Result<TypeEval, TypeError> {
    // Types are equal.
    if left == right {
        return Ok(left);
    }
    Err(whirl_errors::mismatched_assignment(left, right, span))
}
