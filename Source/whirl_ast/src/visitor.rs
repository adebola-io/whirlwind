use crate::{FunctionDeclaration, Identifier, Statement};

/// A trait for traversing the Abstract Syntax Tree.
pub trait ASTVisitor<Arguments = (), Output: Default = ()> {
    fn visit_statement(&self, statement: &Statement, args: &Arguments) -> Output {
        match statement {
            Statement::FunctionDeclaration(f) => self.visit_function(f, args),
            _ => Output::default(),
        }
    }

    fn visit_function(&self, function: &FunctionDeclaration, args: &Arguments) -> Output {
        let signature = function.signature.lock().unwrap();
        let body = &function.body;

        self.visit_identifier(&signature.name, args);

        for statement in &body.statements {
            self.visit_statement(statement, args);
        }
        Output::default()
    }

    fn visit_identifier(&self, _ident: &Identifier, _args: &Arguments) -> Output {
        Output::default()
    }
}
