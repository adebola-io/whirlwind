use analyzer::{
    span_of_typed_statement, SemanticSymbol, ShortcircuitTypedVisitorNoArgs, Standpoint,
    TypedFunctionDeclaration, TypedStmnt,
};

/// Returns a function declaration that maps to a function symbol.
pub struct FunctionRetriever<'a> {
    standpoint: &'a Standpoint,
    symbol: &'a SemanticSymbol,
}

impl<'a> ShortcircuitTypedVisitorNoArgs<'a, &'a TypedFunctionDeclaration>
    for FunctionRetriever<'a>
{
    fn statement(&self, statement: &'a TypedStmnt) -> Option<&'a TypedFunctionDeclaration> {
        let symbollib = &self.standpoint.symbol_library;
        let literals = &self.standpoint.literals;
        let span = span_of_typed_statement(statement, symbollib, literals);
        if !span.encloses(self.symbol.origin_span) {
            return None;
        }
        match statement {
            TypedStmnt::TestDeclaration(test) => self.test_declaration(test),
            TypedStmnt::VariableDeclaration(var_decl) => self.var_decl(var_decl),
            TypedStmnt::ShorthandVariableDeclaration(shorthand) => {
                self.shorthand_var_decl(shorthand)
            }
            TypedStmnt::ConstantDeclaration(constant) => self.constant(constant),
            TypedStmnt::ModelDeclaration(model) => self.model_decl(model),
            TypedStmnt::FunctionDeclaration(function) => {
                let symbol = symbollib.get(function.name)?;
                if std::ptr::eq(symbol, self.symbol) {
                    return Some(function);
                }
                self.function(function)
            }
            TypedStmnt::InterfaceDeclaration(interface) => self.interface_declaration(interface),
            TypedStmnt::ExpressionStatement(expr) | TypedStmnt::FreeExpression(expr) => {
                self.expr(expr)
            }
            TypedStmnt::ReturnStatement(rettye) => self.return_statement(rettye),
            TypedStmnt::ForStatement(forstat) => self.for_statement(forstat),
            TypedStmnt::WhileStatement(whilestat) => self.while_statement(whilestat),
            _ => None,
        }
    }
}
impl FunctionRetriever<'_> {
    pub fn retrieve(&self) -> Option<&TypedFunctionDeclaration> {
        let module_idx = self.symbol.references.first()?.module_path;
        let module = self.standpoint.module_map.get(module_idx)?;
        for statement in &module.statements {
            ast::maybe!(self.statement(statement))
        }
        return None;
    }
}
