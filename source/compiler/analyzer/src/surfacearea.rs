use crate::{
    SemanticSymbolKind, Standpoint, SymbolIndex, SymbolLibrary, TypedModelPropertyType,
    TypedModule, TypedVisitorNoArgs,
};
use std::cell::RefCell;

pub struct SurfaceAreaCalculator<'a> {
    symbollib: &'a SymbolLibrary,
    module: &'a TypedModule,
    surfacearea: RefCell<SurfaceArea>,
}
/// Ths surface area of a module is a container for the symbols
/// declared and referenced within a module, gotten by traversing the module
/// in its entirety, as opposed to iterating over the symbollib.
#[derive(Default)]
pub struct SurfaceArea {
    pub declared_in_module: Vec<SymbolIndex>,
    pub outer_symbols: Vec<SymbolIndex>,
}

macro_rules! unwrap_or_return {
    ($expr: expr) => {{
        match $expr {
            Some(expr) => expr,
            None => return,
        }
    }};
}

impl SurfaceAreaCalculator<'_> {
    pub fn gather_from_module(module: &TypedModule, standpoint: &Standpoint) -> SurfaceArea {
        let symbollib = &standpoint.symbol_library;
        let surfaceareacalculator = SurfaceAreaCalculator {
            symbollib,
            module,
            surfacearea: RefCell::new(SurfaceArea {
                declared_in_module: vec![],
                outer_symbols: vec![],
            }),
        };
        for statement in &module.statements {
            surfaceareacalculator.statement(statement);
        }
        let mut surfacearea = surfaceareacalculator.surfacearea.take();
        surfacearea.declared_in_module.push(module.symbol_idx);
        // Add modules.
        for (_, other_module) in standpoint.module_map.paths() {
            if std::ptr::eq(module, other_module) {
                continue;
            }
            let module_symbol = match symbollib.get(other_module.symbol_idx) {
                Some(module_symbol) => module_symbol,
                None => continue,
            };
            if module_symbol
                .references
                .iter()
                .any(|reflist| reflist.module_path == module.path_idx)
            {
                surfacearea.outer_symbols.push(other_module.symbol_idx);
            }
        }
        surfacearea
    }

    pub fn surface_area(&self) -> &mut SurfaceArea {
        unsafe { &mut *self.surfacearea.as_ptr() }
    }
}

impl<'a> TypedVisitorNoArgs for SurfaceAreaCalculator<'a> {
    fn statement(&self, statement: &crate::TypedStmnt) {
        match statement {
            crate::TypedStmnt::FunctionDeclaration(f) => self.function(f),
            crate::TypedStmnt::TypedTypeEquation(t) => self.type_decl(t),
            crate::TypedStmnt::EnumDeclaration(e) => self.enum_decl(e),
            crate::TypedStmnt::ModelDeclaration(m) => self.model_decl(m),
            crate::TypedStmnt::ShorthandVariableDeclaration(v) => self.shorthand_var_decl(v),
            crate::TypedStmnt::ExpressionStatement(e) => self.expr_statement(e),
            crate::TypedStmnt::FreeExpression(e) => self.free_expr(e),
            crate::TypedStmnt::InterfaceDeclaration(t) => self.interface_declaration(t),
            crate::TypedStmnt::ModuleDeclaration(m) => self.module_declaration(m),
            crate::TypedStmnt::UseDeclaration(u) => self.use_declaration(u),
            crate::TypedStmnt::TestDeclaration(t) => self.test_declaration(t),
            crate::TypedStmnt::ReturnStatement(rettye) => self.return_statement(rettye),
            crate::TypedStmnt::BreakStatement(brk) => self.break_statement(brk),
            crate::TypedStmnt::ForStatement(for_stat) => self.for_statement(for_stat),
            crate::TypedStmnt::WhileStatement(whilestat) => self.while_statement(whilestat),
            crate::TypedStmnt::ContinueStatement(continue_) => self.continue_statement(continue_),
            crate::TypedStmnt::VariableDeclaration(variable) => self.var_decl(variable),
            _ => {}
        }
    }

    fn use_declaration(&self, use_decl: &crate::TypedUseDeclaration) {
        let surface_area = self.surface_area();
        for import_symbol in &use_decl.imports {
            surface_area.declared_in_module.push(*import_symbol);
            let symbol = self.symbollib.get(*import_symbol).unwrap();
            if let SemanticSymbolKind::Import { source, .. } = &symbol.kind {
                if let Some(source) = source {
                    surface_area.outer_symbols.push(*source);
                }
            }
        }
    }

    fn module_declaration(&self, _module: &crate::TypedModuleDeclaration) {}

    fn interface_declaration(&self, _interface: &crate::TypedInterfaceDeclaration) {
        let surface_area = self.surface_area();
        surface_area.declared_in_module.push(_interface.name);
        let symbol = unwrap_or_return!(self.symbollib.get(_interface.name));
        if let SemanticSymbolKind::Interface { generic_params, .. } = &symbol.kind {
            generic_params
                .iter()
                .for_each(|method_idx| surface_area.declared_in_module.push(*method_idx));
        }
        for property in &_interface.body.properties {
            let method_symbol = match self.symbollib.get(property.name) {
                Some(symbol) => symbol,
                None => continue,
            };
            if let SemanticSymbolKind::Method {
                params,
                generic_params,
                ..
            } = &method_symbol.kind
            {
                generic_params
                    .iter()
                    .for_each(|generic_param| surface_area.declared_in_module.push(*generic_param));
                params
                    .iter()
                    .for_each(|param| surface_area.declared_in_module.push(*param));
            }
            surface_area.declared_in_module.push(property.name);
            if let crate::TypedInterfacePropertyType::Method { body } = &property._type {
                self.block(body)
            }
        }
    }

    fn expr_statement(&self, exp: &crate::TypedExpression) {
        self.expr(exp)
    }

    fn free_expr(&self, exp: &crate::TypedExpression) {
        self.expr(exp)
    }

    fn expr(&self, exp: &crate::TypedExpression) {
        match exp {
            crate::TypedExpression::Identifier(i) => self.identifier(i),
            crate::TypedExpression::Literal(l) => self.literal(l),
            crate::TypedExpression::ThisExpr(t) => self.this_expr(t),
            crate::TypedExpression::CallExpr(c) => self.call_expr(c),
            crate::TypedExpression::FnExpr(f) => self.function_expr(f),
            crate::TypedExpression::IfExpr(i) => self.if_expr(i),
            crate::TypedExpression::ArrayExpr(a) => self.array(a),
            crate::TypedExpression::AccessExpr(a) => self.access(a),
            crate::TypedExpression::IndexExpr(i) => self.index(i),
            crate::TypedExpression::BinaryExpr(b) => self.bin_exp(b),
            crate::TypedExpression::AssignmentExpr(a) => self.ass_exp(a),
            crate::TypedExpression::UnaryExpr(u) => self.un_exp(u),
            crate::TypedExpression::LogicExpr(l) => self.log_exp(l),
            crate::TypedExpression::Block(b) => self.block(b),
            crate::TypedExpression::UpdateExpr(u) => self.update(u),
        }
    }

    fn if_expr(&self, ifexp: &crate::TypedIfExpr) {
        self.expr(&ifexp.condition);
        self.block(&ifexp.consequent);
        if let Some(el) = &ifexp.alternate {
            self.expr(&el.expression);
        }
    }

    fn block(&self, block: &crate::TypedBlock) {
        for stat in &block.statements {
            self.statement(stat);
        }
    }

    fn log_exp(&self, logexp: &crate::TypedLogicExpr) {
        self.expr(&logexp.left);
        self.expr(&logexp.right)
    }

    fn un_exp(&self, unexp: &crate::TypedUnaryExpr) {
        self.expr(&unexp.operand)
    }

    fn update(&self, update: &crate::TypedUpdateExpr) {
        self.expr(&update.operand)
    }

    fn ass_exp(&self, assexp: &crate::TypedAssignmentExpr) {
        self.expr(&assexp.left);
        self.expr(&assexp.right)
    }

    fn bin_exp(&self, binexp: &crate::TypedBinExpr) {
        self.expr(&binexp.left);
        self.expr(&binexp.right)
    }

    fn index(&self, index_expr: &crate::TypedIndexExpr) {
        self.expr(&index_expr.object);
        self.expr(&index_expr.index)
    }

    fn access(&self, acces_expr: &crate::TypedAccessExpr) {
        self.expr(&acces_expr.object);
        self.expr(&acces_expr.property);
    }

    fn array(&self, arr: &crate::TypedArrayExpr) {
        for elem in &arr.elements {
            self.expr(elem);
        }
    }

    fn function_expr(&self, function_expr: &crate::TypedFnExpr) {
        let surface_area = self.surface_area();
        function_expr
            .generic_params
            .iter()
            .for_each(|generic_param| surface_area.declared_in_module.push(*generic_param));
        function_expr
            .params
            .iter()
            .for_each(|param| surface_area.declared_in_module.push(*param));
        self.expr(&function_expr.body)
    }

    fn call_expr(&self, call: &crate::TypedCallExpr) {
        self.expr(&call.caller);
        for arg in &call.arguments {
            self.expr(arg);
        }
    }

    fn type_decl(&self, type_decl: &crate::TypedTypeEquation) {
        let surface_area = self.surface_area();
        surface_area.declared_in_module.push(type_decl.name);
        let symbol = unwrap_or_return!(self.symbollib.get(type_decl.name));
        if let SemanticSymbolKind::TypeName { generic_params, .. } = &symbol.kind {
            generic_params
                .iter()
                .for_each(|generic_param| surface_area.declared_in_module.push(*generic_param));
        }
    }

    fn this_expr(&self, _this: &crate::TypedThisExpr) {}

    fn identifier(&self, ident: &crate::TypedIdent) {
        let area = self.surface_area();
        let symbol = unwrap_or_return!(self.symbollib.get(ident.value));
        if !area.declared_in_module.contains(&ident.value) {
            if symbol.was_declared_in(self.module.path_idx) {
                area.declared_in_module.push(ident.value);
            } else {
                area.outer_symbols.push(ident.value);
            }
        }
    }

    fn literal(&self, _literal: &crate::LiteralIndex) {}

    fn shorthand_var_decl(&self, var_decl: &crate::TypedShorthandVariableDeclaration) {
        self.surface_area().declared_in_module.push(var_decl.name);
        self.expr(&var_decl.value);
    }

    fn var_decl(&self, var_decl: &crate::TypedVariableDeclaration) {
        for name in &var_decl.names {
            self.surface_area().declared_in_module.push(*name);
        }
        var_decl.value.as_ref().map(|expr| self.expr(expr));
    }

    fn test_declaration(&self, test: &crate::TypedTestDeclaration) {
        self.block(&test.body);
    }

    fn break_statement(&self, brk: &crate::TypedBreakStatement) {
        brk.label.as_ref().map(|ident| self.identifier(ident));
    }

    fn for_statement(&self, _forstat: &crate::TypedForStatement) {
        <()>::default()
    }

    fn while_statement(&self, _while: &crate::TypedWhileStatement) {
        self.expr(&_while.condition);
        self.block(&_while.body);
    }

    fn continue_statement(&self, cont: &crate::TypedContinueStatement) {
        cont.label.as_ref().map(|ident| self.identifier(ident));
    }

    fn return_statement(&self, rettye: &crate::TypedReturnStatement) {
        rettye.value.as_ref().map(|expr| self.expr(expr));
    }

    fn function(&self, function: &crate::TypedFunctionDeclaration) {
        let symbol = unwrap_or_return!(self.symbollib.get(function.name));
        let surface_area = self.surface_area();
        surface_area.declared_in_module.push(function.name);
        if let SemanticSymbolKind::Function {
            params,
            generic_params,
            ..
        } = &symbol.kind
        {
            generic_params
                .iter()
                .for_each(|generic_param| surface_area.declared_in_module.push(*generic_param));
            params
                .iter()
                .for_each(|param| surface_area.declared_in_module.push(*param));
        }
        let body = &function.body;
        for statement in &body.statements {
            self.statement(statement);
        }
    }

    fn enum_decl(&self, enum_decl: &crate::TypedEnumDeclaration) {
        let symbol = unwrap_or_return!(self.symbollib.get(enum_decl.name));
        let surface_area = self.surface_area();
        if let SemanticSymbolKind::Enum {
            generic_params,
            variants,
            ..
        } = &symbol.kind
        {
            generic_params
                .iter()
                .for_each(|generic_param| surface_area.declared_in_module.push(*generic_param));
            variants
                .iter()
                .for_each(|param| surface_area.declared_in_module.push(*param));
        }
    }

    fn model_decl(&self, model: &crate::TypedModelDeclaration) {
        let surface_area = self.surface_area();
        surface_area.declared_in_module.push(model.name);
        let symbol = unwrap_or_return!(self.symbollib.get(model.name));
        if let SemanticSymbolKind::Model { generic_params, .. } = &symbol.kind {
            generic_params
                .iter()
                .for_each(|method_idx| surface_area.declared_in_module.push(*method_idx));
        }
        for property in &model.body.properties {
            let symbol = match self.symbollib.get(property.name) {
                Some(symbol) => symbol,
                None => continue,
            };
            if let SemanticSymbolKind::Method {
                params,
                generic_params,
                ..
            } = &symbol.kind
            {
                generic_params
                    .iter()
                    .for_each(|generic_param| surface_area.declared_in_module.push(*generic_param));
                params
                    .iter()
                    .for_each(|param| surface_area.declared_in_module.push(*param));
            }
            match &property._type {
                TypedModelPropertyType::InterfaceImpl { body, .. } => self.block(body),
                TypedModelPropertyType::TypedMethod { body } => self.block(body),
                TypedModelPropertyType::TypedAttribute => {}
            }
            surface_area.declared_in_module.push(property.name);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{Module, Standpoint, SurfaceAreaCalculator, CORE_LIBRARY_PATH};

    #[test]
    fn compare_area_to_iteration() {
        let text = String::from(
            "
    module Test;
    use core.io.Fmt;

    public function main() {
        Print(\'Hello, world!\')
    }

    interface Vehicle {}

    model Car implements Vehicle {
        var currentSpeed: Float;
        public function Honk(): String {
            Todo()
        }
        public function Move(initSpeed: Float) {
            this.currentSpeed = initSpeed;
            this.currentSpeed = initSpeed;
        }
    }
    ",
        );
        let mut module = Module::from_text(&text);
        module.module_path = Some(PathBuf::from("testing:://Test.wrl"));
        let mut standpoint = Standpoint::new(true, Some(PathBuf::from(CORE_LIBRARY_PATH)));
        let idx = standpoint.add_module(module).unwrap();
        standpoint.validate();

        let time = std::time::Instant::now();
        let area = standpoint
            .symbol_library
            .in_module(idx)
            .map(|symbol| symbol.name.as_str())
            .collect::<Vec<_>>();
        println!(
        "There are {} symbols in the module, according to the iterator == {:#?} \n\n found in {:?} \n\n\n ========\n\n\n",
        area.len(),
        area,
        time.elapsed()
    );
        let time = std::time::Instant::now();
        let module = &standpoint.module_map.get(idx).unwrap();
        let area = SurfaceAreaCalculator::gather_from_module(module, &standpoint);
        println!(
        "There are {} symbols in the module, according to the gatherer == {:#?} \n\n, found in {:?}",
        area.declared_in_module.len(),
        area.declared_in_module
            .iter()
            .map(|idx| (standpoint.symbol_library.get(*idx).unwrap().name.as_str(), idx)).collect::<Vec<_>>(),
        time.elapsed()
    );
    }
}
