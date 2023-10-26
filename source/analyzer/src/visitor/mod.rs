use crate::{
    LiteralIndex, TypedAccessExpr, TypedArrayExpr, TypedAssignmentExpr, TypedBinExpr, TypedBlock,
    TypedBreakStatement, TypedCallExpr, TypedConstantDeclaration, TypedContinueStatement,
    TypedEnumDeclaration, TypedExpression, TypedFnExpr, TypedForStatement,
    TypedFunctionDeclaration, TypedIdent, TypedIfExpr, TypedIndexExpr, TypedLogicExpr,
    TypedModelDeclaration, TypedModuleDeclaration, TypedNewExpr, TypedReturnStatement,
    TypedShorthandVariableDeclaration, TypedStmnt, TypedTestDeclaration, TypedThisExpr,
    TypedTraitDeclaration, TypedTypeDeclaration, TypedUnaryExpr, TypedUpdateExpr,
    TypedUseDeclaration, TypedVariableDeclaration, TypedWhileStatement,
};

#[allow(unused_variables)]
/// A very skeletal trait for immutably traversing the Typed Module Tree.
pub trait TypedTreeVisitor<Arguments = (), Output: Default = ()> {
    /// Visit a statement node.
    fn statement(&self, statement: &TypedStmnt, args: &Arguments) -> Output {
        match statement {
            TypedStmnt::FunctionDeclaration(f) => self.function(f, args),
            TypedStmnt::TypeDeclaration(t) => self.type_decl(t, args),
            TypedStmnt::EnumDeclaration(e) => self.enum_decl(e, args),
            TypedStmnt::ModelDeclaration(m) => self.model_decl(m, args),
            TypedStmnt::ShorthandVariableDeclaration(v) => self.shorthand_var_decl(v, args),
            TypedStmnt::ExpressionStatement(e) => self.expr_statement(e, args),
            TypedStmnt::FreeExpression(e) => self.free_expr(e, args),
            // TypedStmnt::TraitDeclaration(t) => self.trait_declaraion(t, args),
            _ => Output::default(),
        }
    }
    // fn trait_declaraion(&self, _trait: &TraitDeclaration, args: &Arguments) -> Output {
    //     Output::default()
    // }
    fn expr_statement(&self, exp: &TypedExpression, args: &Arguments) -> Output {
        self.expr(exp, args)
    }
    fn free_expr(&self, exp: &TypedExpression, args: &Arguments) -> Output {
        self.expr(exp, args)
    }
    fn expr(&self, exp: &TypedExpression, args: &Arguments) -> Output {
        match exp {
            TypedExpression::Identifier(i) => self.identifier(i, args),
            TypedExpression::Literal(l) => self.literal(l, args),
            TypedExpression::NewExpr(n) => self.new_expr(n, args),
            TypedExpression::ThisExpr(t) => self.this_expr(t, args),
            TypedExpression::CallExpr(c) => self.call_expr(c, args),
            TypedExpression::FnExpr(f) => self.function_expr(f, args),
            TypedExpression::IfExpr(i) => self.if_expr(i, args),
            TypedExpression::ArrayExpr(a) => self.array(a, args),
            TypedExpression::AccessExpr(a) => self.access(a, args),
            TypedExpression::IndexExpr(i) => self.index(i, args),
            TypedExpression::BinaryExpr(b) => self.bin_exp(b, args),
            TypedExpression::AssignmentExpr(a) => self.ass_exp(a, args),
            TypedExpression::UnaryExpr(u) => self.un_exp(u, args),
            TypedExpression::LogicExpr(l) => self.log_exp(l, args),
            TypedExpression::Block(b) => self.block(b, args),
            TypedExpression::UpdateExpr(u) => self.update(u, args),
        }
    }

    fn if_expr(&self, ifexp: &TypedIfExpr, args: &Arguments) -> Output {
        self.expr(&ifexp.condition, args);
        self.block(&ifexp.consequent, args);
        if let Some(el) = &ifexp.alternate {
            self.expr(&el.expression, args);
        }
        Output::default()
    }

    fn block(&self, block: &TypedBlock, args: &Arguments) -> Output {
        for stat in &block.statements {
            self.statement(stat, args);
        }
        Output::default()
    }

    fn log_exp(&self, logexp: &TypedLogicExpr, args: &Arguments) -> Output {
        self.expr(&logexp.left, args);
        self.expr(&logexp.right, args)
    }

    fn un_exp(&self, unexp: &TypedUnaryExpr, args: &Arguments) -> Output {
        self.expr(&unexp.operand, args)
    }

    fn update(&self, updateexp: &TypedUpdateExpr, args: &Arguments) -> Output {
        self.expr(&updateexp.operand, args)
    }

    fn ass_exp(&self, assexp: &TypedAssignmentExpr, args: &Arguments) -> Output {
        self.expr(&assexp.left, args);
        self.expr(&assexp.right, args)
    }

    fn bin_exp(&self, binexp: &TypedBinExpr, args: &Arguments) -> Output {
        self.expr(&binexp.left, args);
        self.expr(&binexp.right, args)
    }

    fn index(&self, index_expr: &TypedIndexExpr, args: &Arguments) -> Output {
        self.expr(&index_expr.object, args);
        self.expr(&index_expr.index, args)
    }

    fn access(&self, acces_expr: &TypedAccessExpr, args: &Arguments) -> Output {
        self.expr(&acces_expr.object, args);
        self.expr(&acces_expr.property, args);
        Output::default()
    }

    fn array(&self, arr: &TypedArrayExpr, args: &Arguments) -> Output {
        for elem in &arr.elements {
            self.expr(elem, args);
        }
        Output::default()
    }

    fn function_expr(&self, function_expr: &TypedFnExpr, args: &Arguments) -> Output {
        self.expr(&function_expr.body, args)
    }
    fn call_expr(&self, call: &TypedCallExpr, args: &Arguments) -> Output {
        self.expr(&call.caller, args);
        for arg in &call.arguments {
            self.expr(arg, args);
        }
        Output::default()
    }
    fn type_decl(&self, type_decl: &TypedTypeDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
    fn literal(&self, literal: &LiteralIndex, args: &Arguments) -> Output {
        Output::default()
    }
    fn this_expr(&self, this: &TypedThisExpr, args: &Arguments) -> Output {
        Output::default()
    }
    fn identifier(&self, ident: &TypedIdent, args: &Arguments) -> Output {
        Output::default()
    }
    fn new_expr(&self, new_exp: &TypedNewExpr, args: &Arguments) -> Output {
        self.expr(&new_exp.value, args)
    }
    fn shorthand_var_decl(
        &self,
        var_decl: &TypedShorthandVariableDeclaration,
        args: &Arguments,
    ) -> Output {
        Output::default()
    }
    /// Visit a function node.
    fn function(&self, function: &TypedFunctionDeclaration, args: &Arguments) -> Output {
        let body = &function.body;
        for statement in &body.statements {
            self.statement(statement, args);
        }
        Output::default()
    }
    /// Visit an enum node.
    fn enum_decl(&self, enum_decl: &TypedEnumDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
    /// Visit a model node.
    fn model_decl(&self, model: &TypedModelDeclaration, args: &Arguments) -> Output {
        Output::default()
    }
}

#[allow(unused_variables)]
/// Ast visitor with no arguments.
pub trait TypedVisitorNoArgs<Output: Default = ()> {
    /// Visit a statement node.
    fn statement(&self, statement: &TypedStmnt) -> Output {
        match statement {
            TypedStmnt::FunctionDeclaration(f) => self.function(f),
            TypedStmnt::TypeDeclaration(t) => self.type_decl(t),
            TypedStmnt::EnumDeclaration(e) => self.enum_decl(e),
            TypedStmnt::ModelDeclaration(m) => self.model_decl(m),
            TypedStmnt::ShorthandVariableDeclaration(v) => self.shorthand_var_decl(v),
            TypedStmnt::ExpressionStatement(e) => self.expr_statement(e),
            TypedStmnt::FreeExpression(e) => self.free_expr(e),
            TypedStmnt::TraitDeclaration(t) => self.trait_declaration(t),
            TypedStmnt::ModuleDeclaration(m) => self.module_declaration(m),
            TypedStmnt::UseDeclaration(u) => self.use_declaration(u),
            TypedStmnt::ConstantDeclaration(c) => self.constant(c),
            TypedStmnt::TestDeclaration(t) => self.test_declaration(t),
            TypedStmnt::ReturnStatement(rettye) => self.return_statement(rettye),
            TypedStmnt::BreakStatement(brk) => self.break_statement(brk),
            TypedStmnt::ForStatement(for_stat) => self.for_statement(for_stat),
            TypedStmnt::WhileStatement(whilestat) => self.while_statement(whilestat),
            TypedStmnt::ContinueStatement(continue_) => self.continue_statement(continue_),
            TypedStmnt::VariableDeclaration(variable) => self.var_decl(variable),
            TypedStmnt::RecordDeclaration => todo!(),
        }
    }
    fn use_declaration(&self, use_decl: &TypedUseDeclaration) -> Output {
        Output::default()
    }
    fn module_declaration(&self, module: &TypedModuleDeclaration) -> Output {
        Output::default()
    }
    fn trait_declaration(&self, _trait: &TypedTraitDeclaration) -> Output {
        Output::default()
    }
    fn expr_statement(&self, exp: &TypedExpression) -> Output {
        self.expr(exp)
    }
    fn free_expr(&self, exp: &TypedExpression) -> Output {
        self.expr(exp)
    }
    fn expr(&self, exp: &TypedExpression) -> Output {
        match exp {
            TypedExpression::Identifier(i) => self.identifier(i),
            TypedExpression::Literal(l) => self.literal(l),
            TypedExpression::NewExpr(n) => self.new_expr(n),
            TypedExpression::ThisExpr(t) => self.this_expr(t),
            TypedExpression::CallExpr(c) => self.call_expr(c),
            TypedExpression::FnExpr(f) => self.function_expr(f),
            TypedExpression::IfExpr(i) => self.if_expr(i),
            TypedExpression::ArrayExpr(a) => self.array(a),
            TypedExpression::AccessExpr(a) => self.access(a),
            TypedExpression::IndexExpr(i) => self.index(i),
            TypedExpression::BinaryExpr(b) => self.bin_exp(b),
            TypedExpression::AssignmentExpr(a) => self.ass_exp(a),
            TypedExpression::UnaryExpr(u) => self.un_exp(u),
            TypedExpression::LogicExpr(l) => self.log_exp(l),
            TypedExpression::Block(b) => self.block(b),
            TypedExpression::UpdateExpr(u) => self.update(u),
        }
    }

    fn if_expr(&self, ifexp: &TypedIfExpr) -> Output {
        self.expr(&ifexp.condition);
        self.block(&ifexp.consequent);
        if let Some(el) = &ifexp.alternate {
            self.expr(&el.expression);
        }
        Output::default()
    }

    fn block(&self, block: &TypedBlock) -> Output {
        for stat in &block.statements {
            self.statement(stat);
        }
        Output::default()
    }

    fn log_exp(&self, logexp: &TypedLogicExpr) -> Output {
        self.expr(&logexp.left);
        self.expr(&logexp.right)
    }

    fn un_exp(&self, unexp: &TypedUnaryExpr) -> Output {
        self.expr(&unexp.operand)
    }

    fn update(&self, update: &TypedUpdateExpr) -> Output {
        self.expr(&update.operand)
    }

    fn ass_exp(&self, assexp: &TypedAssignmentExpr) -> Output {
        self.expr(&assexp.left);
        self.expr(&assexp.right)
    }

    fn bin_exp(&self, binexp: &TypedBinExpr) -> Output {
        self.expr(&binexp.left);
        self.expr(&binexp.right)
    }

    fn index(&self, index_expr: &TypedIndexExpr) -> Output {
        self.expr(&index_expr.object);
        self.expr(&index_expr.index)
    }

    fn access(&self, acces_expr: &TypedAccessExpr) -> Output {
        self.expr(&acces_expr.object);
        self.expr(&acces_expr.property)
    }

    fn array(&self, arr: &TypedArrayExpr) -> Output {
        for elem in &arr.elements {
            self.expr(elem);
        }
        Output::default()
    }

    fn function_expr(&self, function_expr: &TypedFnExpr) -> Output {
        self.expr(&function_expr.body)
    }
    fn call_expr(&self, call: &TypedCallExpr) -> Output {
        self.expr(&call.caller);
        for arg in &call.arguments {
            self.expr(arg);
        }
        Output::default()
    }
    fn type_decl(&self, type_decl: &TypedTypeDeclaration) -> Output {
        Output::default()
    }
    fn this_expr(&self, this: &TypedThisExpr) -> Output {
        Output::default()
    }
    fn identifier(&self, ident: &TypedIdent) -> Output {
        Output::default()
    }
    fn literal(&self, literal: &LiteralIndex) -> Output {
        Output::default()
    }
    fn new_expr(&self, new_exp: &TypedNewExpr) -> Output {
        self.expr(&new_exp.value)
    }
    fn shorthand_var_decl(&self, var_decl: &TypedShorthandVariableDeclaration) -> Output {
        Output::default()
    }
    fn var_decl(&self, var_decl: &TypedVariableDeclaration) -> Output {
        Output::default()
    }
    // fn variable_declaration(&self, var_decl: &TypedVariableDeclaration) -> Output {
    //     Output::default()
    // }
    fn constant(&self, constant: &TypedConstantDeclaration) -> Output {
        Output::default()
    }

    fn test_declaration(&self, test: &TypedTestDeclaration) -> Output {
        Output::default()
    }

    fn break_statement(&self, brk: &TypedBreakStatement) -> Output {
        Output::default()
    }

    fn for_statement(&self, forstat: &TypedForStatement) -> Output {
        Output::default()
    }

    fn while_statement(&self, _while: &TypedWhileStatement) -> Output {
        Output::default()
    }

    fn continue_statement(&self, cont: &TypedContinueStatement) -> Output {
        Output::default()
    }

    fn return_statement(&self, rettye: &TypedReturnStatement) -> Output {
        Output::default()
    }
    /// Visit a function node.
    fn function(&self, function: &TypedFunctionDeclaration) -> Output {
        let body = &function.body;
        for statement in &body.statements {
            self.statement(statement);
        }
        Output::default()
    }
    /// Visit an enum node.
    fn enum_decl(&self, enum_decl: &TypedEnumDeclaration) -> Output {
        Output::default()
    }
    /// Visit a model node.
    fn model_decl(&self, model: &TypedModelDeclaration) -> Output {
        Output::default()
    }
}

/// Mutable implementation of [`ASTVisitor`].
#[allow(unused)]
pub trait MutASTVisitor<Output: Default = ()> {
    fn statement(&mut self, statement: &mut TypedStmnt) {
        match statement {
            TypedStmnt::TestDeclaration(t) => self.test_declaration(t),
            TypedStmnt::UseDeclaration(u) => self.use_declaration(u),
            TypedStmnt::ShorthandVariableDeclaration(v) => self.shorthand_variable_declaration(v),
            TypedStmnt::FunctionDeclaration(f) => self.function_declaration(f),
            TypedStmnt::EnumDeclaration(e) => self.enum_declaration(e),
            TypedStmnt::TypeDeclaration(t) => self.type_declaration(t),
            // TypedStmnt::TraitDeclaration(t) => self.trait_declaration(t),
            TypedStmnt::ExpressionStatement(e) => {
                self.expr_statement(e);
            }
            TypedStmnt::FreeExpression(e) => {
                self.free_expr(e);
            }
            // TypedStmnt::VariableDeclaration(v) => self.variable_declaration(v),
            TypedStmnt::ConstantDeclaration(c) => self.constant(c),
            TypedStmnt::ModelDeclaration(_) => todo!(),
            TypedStmnt::ModuleDeclaration(_) => todo!(),
            // TypedStmnt::RecordDeclaration => todo!(),
            // TypedStmnt::TraitDeclaration(_) => todo!(),
            TypedStmnt::WhileStatement(w) => self.while_statement(w),
            TypedStmnt::ReturnStatement(_) => todo!(),
            TypedStmnt::ForStatement(f) => self.for_statement(f),
            TypedStmnt::ContinueStatement(c) => self.continue_statement(c),
            TypedStmnt::BreakStatement(b) => self.break_statement(b),
            TypedStmnt::RecordDeclaration => todo!(),
            TypedStmnt::VariableDeclaration(_) => todo!(),
            TypedStmnt::TraitDeclaration(_) => todo!(),
            // _ => {}
        }
    }

    fn for_statement(&mut self, f: &mut TypedForStatement) {}

    fn break_statement(&mut self, b: &mut crate::TypedBreakStatement) {}

    fn continue_statement(&mut self, c: &mut crate::TypedContinueStatement) {}

    fn expr_statement(&mut self, exp: &mut TypedExpression) {
        self.expression(exp);
    }
    fn free_expr(&mut self, exp: &mut TypedExpression) {
        self.expression(exp);
    }

    fn constant(&mut self, constant: &mut TypedConstantDeclaration) {}

    fn while_statement(&mut self, while_stat: &mut TypedWhileStatement) {}

    fn test_declaration(&mut self, test_decl: &mut TypedTestDeclaration) {}

    fn use_declaration(&mut self, use_decl: &mut TypedUseDeclaration) {}

    fn type_declaration(&mut self, type_decl: &mut TypedTypeDeclaration) {}

    fn shorthand_variable_declaration(
        &mut self,
        variable_decl: &mut TypedShorthandVariableDeclaration,
    ) {
    }

    fn variable_declaration(&mut self, variable_decl: &mut TypedVariableDeclaration) {}

    fn function_declaration(&mut self, function: &mut TypedFunctionDeclaration) {}

    fn enum_declaration(&mut self, enum_decl: &mut TypedEnumDeclaration) {}

    fn expression(&mut self, exp: &mut TypedExpression) -> Output {
        match exp {
            TypedExpression::Identifier(i) => self.identifier(i),
            TypedExpression::BinaryExpr(b) => self.binary_expr(b),
            _ => Output::default(),
        }
    }

    fn identifier(&mut self, ident: &mut TypedIdent) -> Output {
        Output::default()
    }

    fn binary_expr(&mut self, bin_exp: &mut TypedBinExpr) -> Output {
        Output::default()
    }
}

///  [`ASTVisitor`] with no arguments.
#[allow(unused)]
pub trait ASTVisitorExprOutputNoArgs<Output: Default = ()> {
    fn statement(&self, statement: &TypedStmnt) {
        match statement {
            TypedStmnt::TestDeclaration(t) => self.test_declaration(t),
            TypedStmnt::UseDeclaration(u) => self.use_declaration(u),
            TypedStmnt::ShorthandVariableDeclaration(v) => self.shorthand_var_decl(v),
            TypedStmnt::FunctionDeclaration(f) => self.function_declaration(f),
            TypedStmnt::EnumDeclaration(e) => self.enum_declaration(e),
            TypedStmnt::TypeDeclaration(t) => self.type_declaration(t),
            TypedStmnt::TraitDeclaration(t) => self.trait_declaration(t),
            TypedStmnt::ExpressionStatement(e) => self.expr_statement(e),
            TypedStmnt::FreeExpression(e) => self.free_expr(e),
            TypedStmnt::ModelDeclaration(m) => self.model_declaration(m),
            TypedStmnt::ModuleDeclaration(m) => self.module_declaration(m),
            TypedStmnt::RecordDeclaration => todo!(),
            TypedStmnt::WhileStatement(w) => self.while_statement(w),
            TypedStmnt::ReturnStatement(r) => self.return_statement(r),
            TypedStmnt::VariableDeclaration(v) => self.var_decl(v),
            TypedStmnt::ConstantDeclaration(c) => self.constant(c),
            TypedStmnt::ForStatement(f) => self.for_statement(f),
            TypedStmnt::ContinueStatement(c) => self.continue_statement(c),
            TypedStmnt::BreakStatement(b) => self.break_statement(b),
        }
    }

    fn for_statement(&self, f: &TypedForStatement) {}

    fn break_statement(&self, b: &crate::TypedBreakStatement) {}

    fn continue_statement(&self, c: &crate::TypedContinueStatement) {}

    fn free_expr(&self, exp: &TypedExpression) {
        self.expr(exp);
    }

    fn expr_statement(&self, exp: &TypedExpression) {
        self.expr(exp);
    }

    fn while_statement(&self, whil: &TypedWhileStatement) {}

    fn return_statement(&self, ret: &TypedReturnStatement) {}

    fn model_declaration(&self, model_decl: &TypedModelDeclaration) {}

    fn module_declaration(&self, module_decl: &TypedModuleDeclaration) {}

    fn trait_declaration(&self, trait_decl: &TypedTraitDeclaration) {}

    fn test_declaration(&self, test_decl: &TypedTestDeclaration) {}

    fn use_declaration(&self, use_decl: &TypedUseDeclaration) {}

    fn type_declaration(&self, type_decl: &TypedTypeDeclaration) {}

    fn shorthand_var_decl(&self, variable_decl: &TypedShorthandVariableDeclaration) {}

    fn var_decl(&self, variable_decl: &TypedVariableDeclaration) {}

    fn constant(&self, constant: &TypedConstantDeclaration) {}

    fn function_declaration(&self, function: &TypedFunctionDeclaration) {}

    fn enum_declaration(&self, enum_decl: &TypedEnumDeclaration) {}

    fn expr(&self, exp: &TypedExpression) -> Output {
        match exp {
            TypedExpression::Identifier(i) => self.identifier(i),
            TypedExpression::Literal(s) => self.literal(s),
            TypedExpression::BinaryExpr(b) => self.binary_expr(b),
            TypedExpression::NewExpr(n) => self.new_expr(n),
            TypedExpression::ThisExpr(t) => self.this_expr(t),
            TypedExpression::CallExpr(c) => self.call_expr(c),
            TypedExpression::FnExpr(f) => self.func_expr(f),
            TypedExpression::IfExpr(i) => self.if_expr(i),
            TypedExpression::ArrayExpr(a) => self.array(a),
            TypedExpression::AccessExpr(a) => self.access(a),
            TypedExpression::IndexExpr(i) => self.index(i),
            TypedExpression::AssignmentExpr(a) => self.assignment_expr(a),
            TypedExpression::UnaryExpr(u) => self.unary_expr(u),
            TypedExpression::LogicExpr(l) => self.logical_expr(l),
            TypedExpression::Block(b) => self.block(b),
            TypedExpression::UpdateExpr(u) => self.update(u),
        }
    }

    fn literal(&self, literal: &LiteralIndex) -> Output {
        Output::default()
    }

    fn logical_expr(&self, log_expr: &TypedLogicExpr) -> Output {
        self.expr(&log_expr.left);
        self.expr(&log_expr.right)
    }

    fn unary_expr(&self, unary_expr: &TypedUnaryExpr) -> Output {
        self.expr(&unary_expr.operand)
    }

    fn update(&self, updateexp: &TypedUpdateExpr) -> Output {
        self.expr(&updateexp.operand)
    }

    fn assignment_expr(&self, ass: &TypedAssignmentExpr) -> Output {
        self.expr(&ass.right);
        self.expr(&ass.left)
    }

    fn block(&self, block: &TypedBlock) -> Output {
        for statement in &block.statements {
            self.statement(statement);
        }
        Output::default()
    }

    fn identifier(&self, ident: &TypedIdent) -> Output {
        Output::default()
    }

    fn new_expr(&self, new_expr: &TypedNewExpr) -> Output {
        self.expr(&new_expr.value)
    }

    fn this_expr(&self, this_expr: &TypedThisExpr) -> Output {
        Output::default()
    }

    fn func_expr(&self, func_expr: &TypedFnExpr) -> Output {
        self.expr(&func_expr.body)
    }

    fn if_expr(&self, if_exp: &TypedIfExpr) -> Output {
        self.expr(&if_exp.condition);
        self.block(&if_exp.consequent);
        if let Some(else_) = &if_exp.alternate {
            self.expr(&else_.expression)
        } else {
            Output::default()
        }
    }

    fn array(&self, array: &TypedArrayExpr) -> Output {
        for element in &array.elements {
            self.expr(element);
        }
        Output::default()
    }

    fn access(&self, access_expr: &TypedAccessExpr) -> Output {
        self.expr(&access_expr.object);
        self.expr(&access_expr.property)
    }

    fn index(&self, index_expr: &TypedIndexExpr) -> Output {
        self.expr(&index_expr.object);
        self.expr(&index_expr.index)
    }

    fn call_expr(&self, call_expr: &TypedCallExpr) -> Output {
        self.expr(&call_expr.caller);
        for expr in &call_expr.arguments {
            self.expr(expr);
        }
        Output::default()
    }

    fn binary_expr(&self, bin_exp: &TypedBinExpr) -> Output {
        self.expr(&bin_exp.left);
        self.expr(&bin_exp.right)
    }
}
