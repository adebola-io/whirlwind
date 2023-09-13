mod errors;
mod test;

use whirl_ast::{
    DiscreteType, MutASTVisitor, ScopeEntry, ScopeManager, Statement, Type, TypeEval,
    TypeExpression,
};
use whirl_lexer::{LexError, Lexer};
use whirl_parser::{parse_text, ParseError};

use crate::primitive::Primitives;

pub use errors::{TypeError, TypeErrorType};

pub struct Typechecker<L: Lexer> {
    pub parser: whirl_parser::Parser<L>,
    pub statements: Vec<Statement>,
    pub type_errors: Vec<TypeError>,
    pub lexical_errors: Vec<LexError>,
    pub syntax_errors: Vec<ParseError>,
    pub types: Vec<*mut Type>,
}

impl<L: Lexer> Typechecker<L> {
    fn check(&mut self, statement: &mut Statement) -> Vec<TypeError> {
        self.visit_statement(statement);
        std::mem::take(&mut self.type_errors)
    }

    /// Returns the (parser's) scope manager.
    pub fn scope_manager(&self) -> &mut ScopeManager {
        self.parser.scope_manager()
    }

    pub fn new(input: &str, primitives: Primitives) -> Typechecker<whirl_lexer::TextLexer> {
        let checker = Typechecker {
            parser: parse_text(input),
            statements: vec![],
            type_errors: vec![],
            lexical_errors: vec![],
            syntax_errors: vec![],
            types: vec![],
        };
        let scope_manager = checker.parser.scope_manager();
        // Register all primitive values.
        for primitive_type in primitives.types {
            scope_manager.register(ScopeEntry::Type(primitive_type));
        }
        for primitive_class in primitives.classes {
            scope_manager.register(ScopeEntry::Class(primitive_class));
        }

        checker
    }

    /// Confirms that a type expression is valid in the scope it is defined, and then generate a type evaluation for it.
    fn evaluate_type_expression(
        &self,
        expression: &TypeExpression,
        scope: usize,
    ) -> Result<TypeEval, TypeError> {
        // Go to scope.
        match expression {
            // Type checking discrete types.
            TypeExpression::Discrete(discrete_type) => {
                self.evaluate_discrete_type(discrete_type, scope)
            }
            // TODO: disallow This type outside class context.
            TypeExpression::This { span } => todo!(),
            TypeExpression::Invalid => Err(errors::assigned_invalid(expression.span())),
            _ => todo!(),
        }
    }

    /// Convert a discrete type to an evaluation.
    fn evaluate_discrete_type(
        &self,
        discrete_type: &DiscreteType,
        scope: usize,
    ) -> Result<TypeEval, TypeError> {
        let shadow = self.scope_manager().create_shadow(scope);
        match shadow.lookup(&discrete_type.name.name) {
            // Block using variable names as types.
            Some(search) => {
                if let ScopeEntry::Variable(_) = search.entry {
                    return Err(errors::value_as_type(
                        &discrete_type.name.name,
                        discrete_type.name.span,
                    ));
                } else {
                    todo!()
                }
            }
            None => {
                return Err(errors::unknown_type(
                    &discrete_type.name.name,
                    discrete_type.name.span,
                ))
            }
        }
    }
}

impl<L: Lexer> Iterator for Typechecker<L> {
    type Item = Vec<TypeError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.parser.next() {
                Some(statement) => match statement {
                    Ok(mut statement) => {
                        let errors = self.check(&mut statement);
                        self.statements.push(statement);
                        return Some(errors);
                    }
                    Err(error) => self.syntax_errors.push(error),
                },
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
            match self.evaluate_type_expression(type_expression, address.scope_id) {
                Err(error) => {
                    signature.var_type.inferred = Some(TypeEval::Invalid);
                    self.type_errors.push(error);
                }
                Ok(expression_as_eval) => {
                    // Fuse the value and declared types.
                    match assign_right_to_left(expression_as_eval, value_type) {
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

    fn visit_function_declaration(&mut self, function: &mut whirl_ast::FunctionDeclaration) {
        for (_index, statement) in function.body.statements.iter_mut().enumerate() {
            self.visit_statement(statement)
        }
    }

    fn visit_string(&mut self, _string: &mut whirl_ast::WhirlString) -> TypeEval {
        TypeEval::Pointer {
            scope_address: [0, 0].into(), // scope address for strings.
            generic_args: None,
        }
    }

    fn visit_boolean(&mut self, _bool: &mut whirl_ast::WhirlBoolean) -> TypeEval {
        TypeEval::Pointer {
            scope_address: [0, 2].into(), // scope address for booleans.
            generic_args: None,
        }
    }

    fn visit_binary_expr(&mut self, bin_exp: &mut whirl_ast::BinaryExpr) -> TypeEval {
        let left = self.visit_expression(&mut bin_exp.left);
        let right = self.visit_expression(&mut bin_exp.right);

        // TODO: Trait operator overloads.

        if left != right {
            self.type_errors.push(errors::invalid_binary(
                left,
                bin_exp.operator,
                right,
                bin_exp.span,
            ));
            return TypeEval::Invalid;
        }

        left
    }

    fn visit_number(&mut self, _number: &mut whirl_ast::WhirlNumber) -> TypeEval {
        TypeEval::Pointer {
            scope_address: [0, 1].into(), // scope address for integers.
            generic_args: None,
        }
    }
}

/// Attempt to assign two types together.
fn assign_right_to_left(left: TypeEval, right: TypeEval) -> Result<TypeEval, TypeError> {
    todo!()
}
