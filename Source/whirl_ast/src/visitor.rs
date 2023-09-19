use crate::{
    AccessExpr, ArrayExpr, AssignmentExpr, BinaryExpr, Block, CallExpr, EnumDeclaration,
    Expression, FunctionDeclaration, FunctionExpr, Identifier, IfExpression, IndexExpr, LogicExpr,
    ModelDeclaration, ModuleDeclaration, NewExpr, Parameter, ReturnStatement,
    ShorthandVariableDeclaration, Statement, TestDeclaration, ThisExpr, TraitDeclaration,
    TypeDeclaration, UnaryExpr, UseDeclaration, WhileStatement, WhirlBoolean, WhirlNumber,
    WhirlString,
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
            Statement::ExpressionStatement(e) | Statement::FreeExpression(e) => {
                self.visit_expression(e, args)
            }
            _ => Output::default(),
        }
    }
    fn visit_expression(&self, exp: &Expression, args: &Arguments) -> Output {
        match exp {
            Expression::Identifier(i) => self.visit_identifier(i, args),
            Expression::StringLiteral(_)
            | Expression::NumberLiteral(_)
            | Expression::BooleanLiteral(_) => Output::default(),
            Expression::NewExpr(n) => self.visit_expression(&n.value, args),
            Expression::ThisExpr(t) => self.visit_this_expression(t, args),
            Expression::CallExpr(_) => todo!(),
            Expression::FnExpr(_) => todo!(),
            Expression::IfExpr(_) => todo!(),
            Expression::ArrayExpr(_) => todo!(),
            Expression::AccessExpr(_) => todo!(),
            Expression::IndexExpr(_) => todo!(),
            Expression::BinaryExpr(_) => todo!(),
            Expression::AssignmentExpr(_) => todo!(),
            Expression::UnaryExpr(_) => todo!(),
            Expression::LogicExpr(_) => todo!(),
            Expression::BlockExpr(_) => todo!(),
        }
    }
    // Visit a type declaration node.
    fn visit_type_declaration(&self, type_decl: &TypeDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
    fn visit_this_expression(&self, this: &ThisExpr, args: &Arguments) -> Output {
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
            // Statement::TraitDeclaration(t) => self.visit_trait_declaration(t),
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

///  [`ASTVisitor`] with no arguments.
#[allow(unused)]
pub trait ASTVisitorNoArgs<Output: Default = ()> {
    fn statement(&self, statement: &Statement) {
        match statement {
            Statement::TestDeclaration(t) => self.test_declaration(t),
            Statement::UseDeclaration(u) => self.use_declaration(u),
            Statement::ShorthandVariableDeclaration(v) => {
                self.visit_shorthand_variable_declaration(v)
            }
            Statement::FunctionDeclaration(f) => self.function_declaration(f),
            Statement::EnumDeclaration(e) => self.enum_declaration(e),
            Statement::TypeDeclaration(t) => self.type_declaration(t),
            Statement::TraitDeclaration(t) => self.trait_declaration(t),
            Statement::ExpressionStatement(e) | Statement::FreeExpression(e) => {
                self.expr(e);
            }
            Statement::ModelDeclaration(m) => self.model_declaration(m),
            Statement::ModuleDeclaration(m) => self.module_declaration(m),
            Statement::RecordDeclaration => todo!(),
            Statement::WhileStatement(w) => self.while_statement(w),
            Statement::ReturnStatement(r) => self.return_statement(r),
            Statement::VariableDeclaration => todo!(),
            Statement::ConstantDeclaration => todo!(),
            Statement::ForStatement => todo!(),
        }
    }

    fn while_statement(&self, whil: &WhileStatement) {}

    fn return_statement(&self, ret: &ReturnStatement) {}

    fn model_declaration(&self, model_decl: &ModelDeclaration) {}

    fn module_declaration(&self, module_decl: &ModuleDeclaration) {}

    fn trait_declaration(&self, trait_decl: &TraitDeclaration) {}

    fn test_declaration(&self, test_decl: &TestDeclaration) {}

    fn use_declaration(&self, use_decl: &UseDeclaration) {}

    fn type_declaration(&self, type_decl: &TypeDeclaration) {}

    fn visit_shorthand_variable_declaration(&self, variable_decl: &ShorthandVariableDeclaration) {}

    fn function_declaration(&self, function: &FunctionDeclaration) {}

    fn enum_declaration(&self, enum_decl: &EnumDeclaration) {}

    fn expr(&self, exp: &Expression) -> Output {
        match exp {
            Expression::Identifier(i) => self.visit_identifier(i),
            Expression::StringLiteral(s) => self.string(s),
            Expression::NumberLiteral(n) => self.number(n),
            Expression::BooleanLiteral(b) => self.visit_boolean(b),
            Expression::BinaryExpr(b) => self.visit_binary_expr(b),
            Expression::NewExpr(n) => self.new_expr(n),
            Expression::ThisExpr(t) => self.this_expr(t),
            Expression::CallExpr(c) => self.call_expr(c),
            Expression::FnExpr(f) => self.func_expr(f),
            Expression::IfExpr(i) => self.if_expr(i),
            Expression::ArrayExpr(a) => self.array(a),
            Expression::AccessExpr(a) => self.access(a),
            Expression::IndexExpr(i) => self.index(i),
            Expression::AssignmentExpr(a) => self.assignment_expr(a),
            Expression::UnaryExpr(u) => self.unary_expr(u),
            Expression::LogicExpr(l) => self.logical_expr(l),
            Expression::BlockExpr(b) => self.block(b),
        }
    }

    fn logical_expr(&self, log_expr: &LogicExpr) -> Output {
        self.expr(&log_expr.left);
        self.expr(&log_expr.right)
    }

    fn unary_expr(&self, unary_expr: &UnaryExpr) -> Output {
        self.expr(&unary_expr.operand)
    }

    fn assignment_expr(&self, ass: &AssignmentExpr) -> Output {
        self.expr(&ass.right);
        self.expr(&ass.left)
    }

    fn block(&self, block: &Block) -> Output {
        for statement in &block.statements {
            self.statement(statement);
        }
        Output::default()
    }

    fn visit_identifier(&self, ident: &Identifier) -> Output {
        Output::default()
    }

    fn new_expr(&self, new_expr: &NewExpr) -> Output {
        self.expr(&new_expr.value)
    }

    fn this_expr(&self, this_expr: &ThisExpr) -> Output {
        Output::default()
    }

    fn func_expr(&self, func_expr: &FunctionExpr) -> Output {
        self.expr(&func_expr.body)
    }

    fn if_expr(&self, if_exp: &IfExpression) -> Output {
        self.expr(&if_exp.condition);
        self.block(&if_exp.consequent);
        if let Some(else_) = &if_exp.alternate {
            self.expr(&else_.expression)
        } else {
            Output::default()
        }
    }

    fn array(&self, array: &ArrayExpr) -> Output {
        for element in &array.elements {
            self.expr(element);
        }
        Output::default()
    }

    fn access(&self, access_expr: &AccessExpr) -> Output {
        self.expr(&access_expr.object);
        self.expr(&access_expr.property)
    }

    fn index(&self, index_expr: &IndexExpr) -> Output {
        self.expr(&index_expr.object);
        self.expr(&index_expr.index)
    }

    fn call_expr(&self, call_expr: &CallExpr) -> Output {
        self.expr(&call_expr.caller);
        for expr in &call_expr.arguments {
            self.expr(expr);
        }
        Output::default()
    }

    fn string(&self, string: &WhirlString) -> Output {
        Output::default()
    }

    fn visit_boolean(&self, bool: &WhirlBoolean) -> Output {
        Output::default()
    }

    fn visit_binary_expr(&self, bin_exp: &BinaryExpr) -> Output {
        self.expr(&bin_exp.left);
        self.expr(&bin_exp.right)
    }

    fn number(&self, number: &WhirlNumber) -> Output {
        Output::default()
    }
}
