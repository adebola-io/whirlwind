use crate::{
    BinaryExpr, EnumDeclaration, Expression, FunctionDeclaration, Identifier, ModelDeclaration,
    Parameter, ShorthandVariableDeclaration, Statement, TestDeclaration, TypeDeclaration,
    UseDeclaration, WhirlBoolean, WhirlNumber, WhirlString,
};

#[allow(unused_variables)]
/// A very skeletal trait for immutably traversing the Abstract Syntax Tree.
pub trait ASTVisitor<Arguments = (), Output: Default = ()> {
    /// Visit a statement node.
    fn visit_statement(&self, statement: &Statement, args: &Arguments) -> Output {
        match statement {
            Statement::FunctionDeclaration(f) => self.visit_function(f, args),
            Statement::TypeDeclaration(t) => self.visit_type_declaration(t, args),
            Statement::EnumDeclaration(e) => self.visit_enum_declaration(e, args),
            Statement::ModelDeclaration(m) => self.visit_model_declaration(m, args),
            Statement::ShorthandVariableDeclaration(v) => {
                self.visit_shorthand_variable_declaration(v, args)
            }
            _ => Output::default(),
        }
    }
    // Visit a type declaration node.
    fn visit_type_declaration(&self, type_decl: &TypeDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
    fn visit_shorthand_variable_declaration(
        &self,
        var_decl: &ShorthandVariableDeclaration,
        args: &Arguments,
    ) -> Output {
        Output::default()
    }
    /// Visit a function node.
    fn visit_function(&self, function: &FunctionDeclaration, args: &Arguments) -> Output {
        let body = &function.body;
        for statement in &body.statements {
            self.visit_statement(statement, args);
        }
        Output::default()
    }
    /// Visit a parameter node.
    fn visit_parameter(&self, parameter: &Parameter, args: &Arguments) -> Output {
        self.visit_identifier(&parameter.name, args)
    }
    /// Visit an enum node.
    fn visit_enum_declaration(&self, enum_decl: &EnumDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
    /// Visit a model node.
    fn visit_model_declaration(&self, model: &ModelDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
    /// Visit an identifier node.
    fn visit_identifier(&self, ident: &Identifier, args: &Arguments) -> Output {
        Output::default()
    }
}

/// Mutable implementation of [`ASTVisitor`].
#[allow(unused)]
pub trait MutASTVisitor<Output: Default = ()> {
    fn visit_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::TestDeclaration(t) => self.visit_test_declaration(t),
            Statement::UseDeclaration(u) => self.visit_use_declaration(u),
            Statement::ShorthandVariableDeclaration(v) => {
                self.visit_shorthand_variable_declaration(v)
            }
            Statement::FunctionDeclaration(f) => self.visit_function_declaration(f),
            Statement::EnumDeclaration(e) => self.visit_enum_declaration(e),
            Statement::TypeDeclaration(t) => self.visit_type_declaration(t),
            Statement::ExpressionStatement(e) => {
                self.visit_expression(e);
            }
            Statement::FreeExpression(e) => {
                self.visit_expression(e);
            }
            _ => {}
        }
    }

    fn visit_test_declaration(&mut self, test_decl: &mut TestDeclaration) {}

    fn visit_use_declaration(&mut self, use_decl: &mut UseDeclaration) {}

    fn visit_type_declaration(&mut self, type_decl: &mut TypeDeclaration) {}

    fn visit_shorthand_variable_declaration(
        &mut self,
        variable_decl: &mut ShorthandVariableDeclaration,
    ) {
    }

    fn visit_function_declaration(&mut self, function: &mut FunctionDeclaration) {}

    fn visit_enum_declaration(&mut self, enum_decl: &mut EnumDeclaration) {}

    fn visit_expression(&mut self, exp: &mut Expression) -> Output {
        match exp {
            Expression::Identifier(i) => self.visit_identifier(i),
            Expression::StringLiteral(s) => self.visit_string(s),
            Expression::NumberLiteral(n) => self.visit_number(n),
            Expression::BooleanLiteral(b) => self.visit_boolean(b),
            Expression::BinaryExpr(b) => self.visit_binary_expr(b),
            _ => Output::default(),
        }
    }

    fn visit_identifier(&mut self, ident: &mut Identifier) -> Output {
        Output::default()
    }

    fn visit_string(&mut self, string: &mut WhirlString) -> Output {
        Output::default()
    }

    fn visit_boolean(&mut self, bool: &mut WhirlBoolean) -> Output {
        Output::default()
    }

    fn visit_binary_expr(&mut self, bin_exp: &mut BinaryExpr) -> Output {
        Output::default()
    }

    fn visit_number(&mut self, number: &mut WhirlNumber) -> Output {
        Output::default()
    }
}
