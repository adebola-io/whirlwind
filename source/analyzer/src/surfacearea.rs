// use crate::{SymbolIndex, SymbolTable, TypedModule, TypedVisitorNoArgs};
// use std::cell::RefCell;

// pub struct SurfaceAreaCalculator<'a> {
//     symboltable: &'a SymbolTable,
//     surfacearea: RefCell<SurfaceArea>,
// }
// /// Ths surface area of a module is a container for the symbols
// /// declared and referenced within a module, gotten by traversing the module
// /// in its entirety, as opposed to iterating over the entire symboltable.
// #[derive(Default)]
// pub struct SurfaceArea {
//     declared_in_module: Vec<SymbolIndex>,
//     outer_symbols: Vec<SymbolIndex>,
// }

// impl SurfaceAreaCalculator<'_> {
//     pub fn gather_from_module(module: &TypedModule, symboltable: &SymbolTable) -> SurfaceArea {
//         let surfaceareacalculator = SurfaceAreaCalculator {
//             symboltable,
//             surfacearea: RefCell::new(SurfaceArea {
//                 declared_in_module: vec![],
//                 outer_symbols: vec![],
//             }),
//         };
//         surfaceareacalculator.surfacearea.take()
//     }
// }

// impl<'a> TypedVisitorNoArgs for SurfaceAreaCalculator<'a> {
//     fn statement(&self, statement: &crate::TypedStmnt) {
//         match statement {
//             crate::TypedStmnt::FunctionDeclaration(f) => self.function(f),
//             crate::TypedStmnt::TypeDeclaration(t) => self.type_decl(t),
//             crate::TypedStmnt::EnumDeclaration(e) => self.enum_decl(e),
//             crate::TypedStmnt::ModelDeclaration(m) => self.model_decl(m),
//             crate::TypedStmnt::ShorthandVariableDeclaration(v) => self.shorthand_var_decl(v),
//             crate::TypedStmnt::ExpressionStatement(e) => self.expr_statement(e),
//             crate::TypedStmnt::FreeExpression(e) => self.free_expr(e),
//             crate::TypedStmnt::TraitDeclaration(t) => self.trait_declaration(t),
//             crate::TypedStmnt::ModuleDeclaration(m) => self.module_declaration(m),
//             crate::TypedStmnt::UseDeclaration(u) => self.use_declaration(u),
//             crate::TypedStmnt::ConstantDeclaration(c) => self.constant(c),
//             crate::TypedStmnt::TestDeclaration(t) => self.test_declaration(t),
//             crate::TypedStmnt::ReturnStatement(rettye) => self.return_statement(rettye),
//             crate::TypedStmnt::BreakStatement(brk) => self.break_statement(brk),
//             crate::TypedStmnt::ForStatement(for_stat) => self.for_statement(for_stat),
//             crate::TypedStmnt::WhileStatement(whilestat) => self.while_statement(whilestat),
//             crate::TypedStmnt::ContinueStatement(continue_) => self.continue_statement(continue_),
//             crate::TypedStmnt::VariableDeclaration(variable) => self.var_decl(variable),
//             _ => {}
//         }
//     }

//     fn use_declaration(&self, use_decl: &crate::TypedUseDeclaration) {}

//     fn module_declaration(&self, module: &crate::TypedModuleDeclaration) {
//         <()>::default()
//     }

//     fn trait_declaration(&self, _trait: &crate::TypedTraitDeclaration) {}

//     fn expr_statement(&self, exp: &crate::TypedExpression) -> () {
//         self.expr(exp)
//     }

//     fn free_expr(&self, exp: &crate::TypedExpression) -> () {
//         self.expr(exp)
//     }

//     fn expr(&self, exp: &crate::TypedExpression) -> () {
//         match exp {
//             crate::TypedExpression::Identifier(i) => self.identifier(i),
//             crate::TypedExpression::Literal(l) => self.literal(l),
//             crate::TypedExpression::NewExpr(n) => self.new_expr(n),
//             crate::TypedExpression::ThisExpr(t) => self.this_expr(t),
//             crate::TypedExpression::CallExpr(c) => self.call_expr(c),
//             crate::TypedExpression::FnExpr(f) => self.function_expr(f),
//             crate::TypedExpression::IfExpr(i) => self.if_expr(i),
//             crate::TypedExpression::ArrayExpr(a) => self.array(a),
//             crate::TypedExpression::AccessExpr(a) => self.access(a),
//             crate::TypedExpression::IndexExpr(i) => self.index(i),
//             crate::TypedExpression::BinaryExpr(b) => self.bin_exp(b),
//             crate::TypedExpression::AssignmentExpr(a) => self.ass_exp(a),
//             crate::TypedExpression::UnaryExpr(u) => self.un_exp(u),
//             crate::TypedExpression::LogicExpr(l) => self.log_exp(l),
//             crate::TypedExpression::Block(b) => self.block(b),
//             crate::TypedExpression::UpdateExpr(u) => self.update(u),
//         }
//     }

//     fn if_expr(&self, ifexp: &crate::TypedIfExpr) -> () {
//         self.expr(&ifexp.condition);
//         self.block(&ifexp.consequent);
//         if let Some(el) = &ifexp.alternate {
//             self.expr(&el.expression);
//         }
//         <()>::default()
//     }

//     fn block(&self, block: &crate::TypedBlock) -> () {
//         for stat in &block.statements {
//             self.statement(stat);
//         }
//         <()>::default()
//     }

//     fn log_exp(&self, logexp: &crate::TypedLogicExpr) -> () {
//         self.expr(&logexp.left);
//         self.expr(&logexp.right)
//     }

//     fn un_exp(&self, unexp: &crate::TypedUnaryExpr) -> () {
//         self.expr(&unexp.operand)
//     }

//     fn update(&self, update: &crate::TypedUpdateExpr) -> () {
//         self.expr(&update.operand)
//     }

//     fn ass_exp(&self, assexp: &crate::TypedAssignmentExpr) -> () {
//         self.expr(&assexp.left);
//         self.expr(&assexp.right)
//     }

//     fn bin_exp(&self, binexp: &crate::TypedBinExpr) -> () {
//         self.expr(&binexp.left);
//         self.expr(&binexp.right)
//     }

//     fn index(&self, index_expr: &crate::TypedIndexExpr) -> () {
//         self.expr(&index_expr.object);
//         self.expr(&index_expr.index)
//     }

//     fn access(&self, acces_expr: &crate::TypedAccessExpr) -> () {
//         self.expr(&acces_expr.object);
//         self.expr(&acces_expr.property)
//     }

//     fn array(&self, arr: &crate::TypedArrayExpr) -> () {
//         for elem in &arr.elements {
//             self.expr(elem);
//         }
//         <()>::default()
//     }

//     fn function_expr(&self, function_expr: &crate::TypedFnExpr) -> () {
//         self.expr(&function_expr.body)
//     }

//     fn call_expr(&self, call: &crate::TypedCallExpr) -> () {
//         self.expr(&call.caller);
//         for arg in &call.arguments {
//             self.expr(arg);
//         }
//         <()>::default()
//     }

//     fn type_decl(&self, type_decl: &crate::TypedTypeDeclaration) -> () {
//         <()>::default()
//     }

//     fn this_expr(&self, this: &crate::TypedThisExpr) -> () {
//         <()>::default()
//     }

//     fn identifier(&self, ident: &crate::TypedIdent) -> () {
//         <()>::default()
//     }

//     fn literal(&self, literal: &crate::LiteralIndex) -> () {
//         <()>::default()
//     }

//     fn new_expr(&self, new_exp: &crate::TypedNewExpr) -> () {
//         self.expr(&new_exp.value)
//     }

//     fn shorthand_var_decl(&self, var_decl: &crate::TypedShorthandVariableDeclaration) -> () {
//         <()>::default()
//     }

//     fn var_decl(&self, var_decl: &crate::TypedVariableDeclaration) -> () {
//         <()>::default()
//     }

//     fn constant(&self, constant: &crate::TypedConstantDeclaration) -> () {
//         <()>::default()
//     }

//     fn test_declaration(&self, test: &crate::TypedTestDeclaration) -> () {
//         <()>::default()
//     }

//     fn break_statement(&self, brk: &crate::TypedBreakStatement) -> () {
//         <()>::default()
//     }

//     fn for_statement(&self, forstat: &crate::TypedForStatement) -> () {
//         <()>::default()
//     }

//     fn while_statement(&self, _while: &crate::TypedWhileStatement) -> () {
//         <()>::default()
//     }

//     fn continue_statement(&self, cont: &crate::TypedContinueStatement) -> () {
//         <()>::default()
//     }

//     fn return_statement(&self, rettye: &crate::TypedReturnStatement) -> () {
//         <()>::default()
//     }

//     fn function(&self, function: &crate::TypedFunctionDeclaration) -> () {
//         let body = &function.body;
//         for statement in &body.statements {
//             self.statement(statement);
//         }
//         <()>::default()
//     }

//     fn enum_decl(&self, enum_decl: &crate::TypedEnumDeclaration) -> () {
//         <()>::default()
//     }

//     fn model_decl(&self, model: &crate::TypedModelDeclaration) -> () {
//         <()>::default()
//     }
// }
