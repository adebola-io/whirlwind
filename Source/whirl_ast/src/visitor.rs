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
    fn statement(&self, statement: &Statement, args: &Arguments) -> Output {
        match statement {
            Statement::FunctionDeclaration(f) => self.function(f, args),
            Statement::TypeDeclaration(t) => self.type_decl(t, args),
            Statement::EnumDeclaration(e) => self.enum_decl(e, args),
            Statement::ModelDeclaration(m) => self.model_decl(m, args),
            Statement::ShorthandVariableDeclaration(v) => self.shorthand_var_decl(v, args),
            Statement::ExpressionStatement(e) | Statement::FreeExpression(e) => self.expr(e, args),
            _ => Output::default(),
        }
    }
    fn expr(&self, exp: &Expression, args: &Arguments) -> Output {
        match exp {
            Expression::Identifier(i) => self.identifier(i, args),
            Expression::StringLiteral(s) => self.string(s, args),
            Expression::NumberLiteral(n) => self.number(n, args),
            Expression::BooleanLiteral(b) => self.boolean(b, args),
            Expression::NewExpr(n) => self.new_expr(n, args),
            Expression::ThisExpr(t) => self.this_expr(t, args),
            Expression::CallExpr(c) => self.call_expr(c, args),
            Expression::FnExpr(f) => self.function_expr(f, args),
            Expression::IfExpr(i) => self.if_expr(i, args),
            Expression::ArrayExpr(a) => self.array(a, args),
            Expression::AccessExpr(a) => self.access(a, args),
            Expression::IndexExpr(i) => self.index(i, args),
            Expression::BinaryExpr(b) => self.bin_exp(b, args),
            Expression::AssignmentExpr(a) => self.ass_exp(a, args),
            Expression::UnaryExpr(u) => self.un_exp(u, args),
            Expression::LogicExpr(l) => self.log_exp(l, args),
            Expression::BlockExpr(b) => self.block(b, args),
        }
    }

    fn if_expr(&self, ifexp: &IfExpression, args: &Arguments) -> Output {
        self.expr(&ifexp.condition, args);
        self.block(&ifexp.consequent, args);
        if let Some(el) = &ifexp.alternate {
            self.expr(&el.expression, args);
        }
        Output::default()
    }

    fn block(&self, block: &Block, args: &Arguments) -> Output {
        for stat in &block.statements {
            self.statement(stat, args);
        }
        Output::default()
    }

    fn log_exp(&self, logexp: &LogicExpr, args: &Arguments) -> Output {
        self.expr(&logexp.left, args);
        self.expr(&logexp.right, args)
    }

    fn un_exp(&self, unexp: &UnaryExpr, args: &Arguments) -> Output {
        self.expr(&unexp.operand, args)
    }

    fn ass_exp(&self, assexp: &AssignmentExpr, args: &Arguments) -> Output {
        self.expr(&assexp.left, args);
        self.expr(&assexp.right, args)
    }

    fn bin_exp(&self, binexp: &BinaryExpr, args: &Arguments) -> Output {
        self.expr(&binexp.left, args);
        self.expr(&binexp.right, args)
    }

    fn index(&self, index_expr: &IndexExpr, args: &Arguments) -> Output {
        self.expr(&index_expr.object, args);
        self.expr(&index_expr.index, args)
    }

    fn access(&self, acces_expr: &AccessExpr, args: &Arguments) -> Output {
        self.expr(&acces_expr.object, args);
        self.expr(&acces_expr.property, args)
    }

    fn array(&self, arr: &ArrayExpr, args: &Arguments) -> Output {
        for elem in &arr.elements {
            self.expr(elem, args);
        }
        Output::default()
    }

    fn function_expr(&self, function_expr: &FunctionExpr, args: &Arguments) -> Output {
        self.expr(&function_expr.body, args)
    }
    fn call_expr(&self, call: &CallExpr, args: &Arguments) -> Output {
        self.expr(&call.caller, args);
        for arg in &call.arguments {
            self.expr(arg, args);
        }
        Output::default()
    }
    fn type_decl(&self, type_decl: &TypeDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
    fn string(&self, string: &WhirlString, args: &Arguments) -> Output {
        Output::default()
    }
    fn number(&self, number: &WhirlNumber, args: &Arguments) -> Output {
        Output::default()
    }
    fn boolean(&self, bool: &WhirlBoolean, args: &Arguments) -> Output {
        Output::default()
    }
    fn this_expr(&self, this: &ThisExpr, args: &Arguments) -> Output {
        Output::default()
    }
    fn identifier(&self, ident: &Identifier, args: &Arguments) -> Output {
        Output::default()
    }
    fn new_expr(&self, new_exp: &NewExpr, args: &Arguments) -> Output {
        self.expr(&new_exp.value, args)
    }
    fn shorthand_var_decl(
        &self,
        var_decl: &ShorthandVariableDeclaration,
        args: &Arguments,
    ) -> Output {
        Output::default()
    }
    /// Visit a function node.
    fn function(&self, function: &FunctionDeclaration, args: &Arguments) -> Output {
        let body = &function.body;
        for statement in &body.statements {
            self.statement(statement, args);
        }
        Output::default()
    }
    /// Visit a parameter node.
    fn param(&self, parameter: &Parameter, args: &Arguments) -> Output {
        self.identifier(&parameter.name, args)
    }
    /// Visit an enum node.
    fn enum_decl(&self, enum_decl: &EnumDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
    /// Visit a model node.
    fn model_decl(&self, model: &ModelDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
}

/// Mutable implementation of [`ASTVisitor`].
#[allow(unused)]
pub trait MutASTVisitor<Output: Default = ()> {
    fn statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::TestDeclaration(t) => self.test_declaration(t),
            Statement::UseDeclaration(u) => self.use_declaration(u),
            Statement::ShorthandVariableDeclaration(v) => self.shorthand_variable_declaration(v),
            Statement::FunctionDeclaration(f) => self.function_declaration(f),
            Statement::EnumDeclaration(e) => self.enum_declaration(e),
            Statement::TypeDeclaration(t) => self.type_declaration(t),
            // Statement::TraitDeclaration(t) => self.trait_declaration(t),
            Statement::ExpressionStatement(e) => {
                self.expression(e);
            }
            Statement::FreeExpression(e) => {
                self.expression(e);
            }
            _ => {}
        }
    }

    fn test_declaration(&mut self, test_decl: &mut TestDeclaration) {}

    fn use_declaration(&mut self, use_decl: &mut UseDeclaration) {}

    fn type_declaration(&mut self, type_decl: &mut TypeDeclaration) {}

    fn shorthand_variable_declaration(&mut self, variable_decl: &mut ShorthandVariableDeclaration) {
    }

    fn function_declaration(&mut self, function: &mut FunctionDeclaration) {}

    fn enum_declaration(&mut self, enum_decl: &mut EnumDeclaration) {}

    fn expression(&mut self, exp: &mut Expression) -> Output {
        match exp {
            Expression::Identifier(i) => self.identifier(i),
            Expression::StringLiteral(s) => self.string(s),
            Expression::NumberLiteral(n) => self.number(n),
            Expression::BooleanLiteral(b) => self.boolean(b),
            Expression::BinaryExpr(b) => self.binary_expr(b),
            _ => Output::default(),
        }
    }

    fn identifier(&mut self, ident: &mut Identifier) -> Output {
        Output::default()
    }

    fn string(&mut self, string: &mut WhirlString) -> Output {
        Output::default()
    }

    fn boolean(&mut self, bool: &mut WhirlBoolean) -> Output {
        Output::default()
    }

    fn binary_expr(&mut self, bin_exp: &mut BinaryExpr) -> Output {
        Output::default()
    }

    fn number(&mut self, number: &mut WhirlNumber) -> Output {
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
            Statement::ShorthandVariableDeclaration(v) => self.shorthand_variable_declaration(v),
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

    fn shorthand_variable_declaration(&self, variable_decl: &ShorthandVariableDeclaration) {}

    fn function_declaration(&self, function: &FunctionDeclaration) {}

    fn enum_declaration(&self, enum_decl: &EnumDeclaration) {}

    fn expr(&self, exp: &Expression) -> Output {
        match exp {
            Expression::Identifier(i) => self.identifier(i),
            Expression::StringLiteral(s) => self.string(s),
            Expression::NumberLiteral(n) => self.number(n),
            Expression::BooleanLiteral(b) => self.boolean(b),
            Expression::BinaryExpr(b) => self.binary_expr(b),
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

    fn identifier(&self, ident: &Identifier) -> Output {
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

    fn boolean(&self, bool: &WhirlBoolean) -> Output {
        Output::default()
    }

    fn binary_expr(&self, bin_exp: &BinaryExpr) -> Output {
        self.expr(&bin_exp.left);
        self.expr(&bin_exp.right)
    }

    fn number(&self, number: &WhirlNumber) -> Output {
        Output::default()
    }
}
