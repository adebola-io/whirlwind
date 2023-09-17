mod test;
pub mod type_utils;

use std::str::Chars;

use whirl_ast::{MutASTVisitor, ScopeEntry, ScopeManager, Span, Statement, TypeEval};
use whirl_errors::{LexError, ParseError};
use whirl_lexer::{Lexer, TextLexer};
use whirl_parser::parse_text;

use crate::primitive::Primitives;

pub use whirl_errors::{TypeError, TypeErrorType};

pub struct Typechecker<L: Lexer> {
    pub parser: whirl_parser::Parser<L>,
    pub statements: Vec<Statement>,
    pub type_errors: Vec<TypeError>,
    pub lexical_errors: Vec<LexError>,
    pub syntax_errors: Vec<ParseError>,
}

impl<L: Lexer> Typechecker<L> {
    /// Check a statement and return all encountered errors.
    fn check(&mut self, statement: &mut Statement) -> Vec<TypeError> {
        self.visit_statement(statement);
        std::mem::take(&mut self.type_errors)
    }

    /// Returns the (parser's) scope manager.
    pub fn scope_manager(&self) -> &mut ScopeManager {
        self.parser.scope_manager()
    }

    pub fn from_text(input: &str, primitives: Primitives) -> Typechecker<TextLexer<Chars>> {
        let checker = Typechecker {
            parser: parse_text(input),
            statements: vec![],
            type_errors: vec![],
            lexical_errors: vec![],
            syntax_errors: vec![],
        };
        let scope_manager = checker.parser.scope_manager();
        // Register all primitive values.
        for primitive_type in primitives.types {
            scope_manager.register(ScopeEntry::Type(primitive_type));
        }
        for primitive_model in primitives.models {
            scope_manager.register(ScopeEntry::Model(primitive_model));
        }

        checker
    }
}

impl<L: Lexer> Iterator for Typechecker<L> {
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

impl<L: Lexer> MutASTVisitor<TypeEval> for Typechecker<L> {
    /// Typecheck a shorthand variable declaration.
    fn visit_shorthand_variable_declaration(
        &mut self,
        variable_decl: &mut whirl_ast::ShorthandVariableDeclaration,
    ) {
        let address = variable_decl.address;
        let value_type = self.visit_expression(&mut variable_decl.value);
        let signature = self
            .scope_manager()
            .get_entry_unguarded_mut(address)
            .var_mut();

        if let Some(ref type_expression) = signature.var_type.declared {
            // Check if assigned type is valid in the given scope.
            match type_utils::eval_type_expression(
                self.scope_manager(),
                type_expression,
                address.scope_id,
            ) {
                Err(error) => {
                    signature.var_type.inferred = Some(TypeEval::Invalid);
                    self.type_errors.push(error);
                }
                Ok(expression_as_eval) => {
                    // Fuse the value and declared types.
                    match assign_right_to_left(expression_as_eval, value_type, variable_decl.span) {
                        Ok(eval) => signature.var_type.inferred = Some(eval),
                        Err(error) => self.type_errors.push(error),
                    }
                }
            }
        } else {
            // todo: uninferrable generics.
            signature.var_type.inferred = Some(value_type);
        }
    }

    /// Typecheck a function declaration.
    fn visit_function_declaration(&mut self, function: &mut whirl_ast::FunctionDeclaration) {
        for (_index, statement) in function.body.statements.iter_mut().enumerate() {
            self.visit_statement(statement)
        }
    }

    /// Typecheck a string.
    fn visit_string(&mut self, _string: &mut whirl_ast::WhirlString) -> TypeEval {
        TypeEval::Pointer {
            address: [0, 0].into(), // scope address for strings.
            args: None,
        }
    }

    /// Typecheck a boolean value.
    fn visit_boolean(&mut self, _bool: &mut whirl_ast::WhirlBoolean) -> TypeEval {
        TypeEval::Pointer {
            address: [0, 2].into(), // scope address for booleans.
            args: None,
        }
    }

    /// Typecheck a binary expression.
    fn visit_binary_expr(&mut self, bin_exp: &mut whirl_ast::BinaryExpr) -> TypeEval {
        let left = self.visit_expression(&mut bin_exp.left);
        let right = self.visit_expression(&mut bin_exp.right);

        // TODO: Trait operator overloads.
        if left != right {
            self.type_errors.push(whirl_errors::invalid_binary(
                left,
                bin_exp.operator,
                right,
                bin_exp.span,
            ));
            return TypeEval::Invalid;
        }
        left
    }

    /// Typecheck a number.
    fn visit_number(&mut self, _number: &mut whirl_ast::WhirlNumber) -> TypeEval {
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
