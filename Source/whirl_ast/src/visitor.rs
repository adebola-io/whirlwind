use crate::{FunctionDeclaration, Identifier, Statement};

/// A trait for traversing the Abstract Syntax Tree.
pub trait ASTVisitor<Arguments = (), Output: Default = ()> {
    /// Visit a statement node.
    fn visit_statement(&self, statement: &Statement, args: &Arguments) -> Output {
        match statement {
            Statement::FunctionDeclaration(f) => self.visit_function(f, args),
            _ => Output::default(),
        }
    }
    /// Visit a function node.
    fn visit_function(&self, function: &FunctionDeclaration, args: &Arguments) -> Output {
        let body = &function.body;
        for statement in &body.statements {
            self.visit_statement(statement, args);
        }
        Output::default()
    }
    /// Visit an identifier node.
    fn visit_identifier(&self, _ident: &Identifier, _args: &Arguments) -> Output {
        Output::default()
    }
}
