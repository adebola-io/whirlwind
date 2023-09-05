use crate::{FunctionDeclaration, Identifier, Parameter, Statement, TypeDeclaration};

#[allow(unused_variables)]
/// A very skeletal trait for immutably traversing the Abstract Syntax Tree.
pub trait ASTVisitor<Arguments = (), Output: Default = ()> {
    /// Visit a statement node.
    fn visit_statement(&self, statement: &Statement, args: &Arguments) -> Output {
        match statement {
            Statement::FunctionDeclaration(f) => self.visit_function(f, args),
            Statement::TypeDeclaration(t) => self.visit_type_declaration(t, args),
            _ => Output::default(),
        }
    }
    // Visit a type declaration node.
    fn visit_type_declaration(&self, type_decl: &TypeDeclaration, args: &Arguments) -> Output {
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
    /// Visit an identifier node.
    fn visit_identifier(&self, ident: &Identifier, args: &Arguments) -> Output {
        Output::default()
    }
}
