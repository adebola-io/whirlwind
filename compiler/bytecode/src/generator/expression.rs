use analyzer::{utils::distill_as_function_type, Literal, SemanticSymbolKind, TypedExpression};

use crate::{Callable, FunctionRetriever, RegisterGroup, RegisterId};

use super::BytecodeGenerator;

impl<'a> BytecodeGenerator<'a> {
    /// Emits an expression as bytecode.
    /// It returns the id of the register that contains the result
    /// of the expression evaluation.
    pub fn emit_expression(&mut self, expression: &'a TypedExpression) -> Option<RegisterId> {
        match expression {
            TypedExpression::Identifier(id) => self.emit_identifier(id),
            TypedExpression::Literal(literal) => self.emit_literal(literal),
            TypedExpression::NewExpr(_) => todo!(),
            TypedExpression::ThisExpr(_) => todo!(),
            TypedExpression::CallExpr(call) => self.emit_call_expression(call),
            TypedExpression::FnExpr(_) => todo!(),
            TypedExpression::Block(block) => self.emit_block(block, false),
            TypedExpression::IfExpr(_) => todo!(),
            TypedExpression::AccessExpr(_) => todo!(),
            TypedExpression::ArrayExpr(_) => todo!(),
            TypedExpression::IndexExpr(_) => todo!(),
            TypedExpression::BinaryExpr(binexp) => self.emit_binary_expression(binexp),
            TypedExpression::AssignmentExpr(_) => todo!(),
            TypedExpression::UnaryExpr(_) => todo!(),
            TypedExpression::LogicExpr(_) => todo!(),
            TypedExpression::UpdateExpr(_) => todo!(),
        }
    }

    fn emit_literal(&mut self, literal: &analyzer::LiteralIndex) -> Option<RegisterId> {
        let literalmap = &self.standpoint.literals;
        let literal_value = literalmap.get(*literal).unwrap();
        match literal_value {
            Literal::StringLiteral { module, value } => todo!(),
            Literal::NumericLiteral {
                value,
                inferred_type,
                ..
            } => {
                let symbollib = &self.standpoint.symbol_library;
                let register_group = RegisterGroup::of(inferred_type, symbollib);
                Some(
                    self.memory
                        .load_immediate_number(register_group, &value.value),
                )
            }
            Literal::BooleanLiteral { value, .. } => {
                Some(self.memory.load_immediate_boolean(*value))
            }
        }
    }

    /// Emits a call expression.
    ///
    /// It loads the parameters to the stack from left to right,
    /// then calls the function.
    pub fn emit_call_expression(
        &mut self,
        call: &'a analyzer::TypedCallExpr,
    ) -> Option<RegisterId> {
        // todo: load parameters.
        // todo: load optional parameters.
        let register_id = self
            .emit_expression(&call.caller)
            .expect("Internal bytecode error: Function caller amounts to void.");
        assert_eq!(register_id.1, RegisterGroup::FunctionPtr);
        // Adds the call instruction to the bytecode.
        self.memory.call_function_in(register_id);
        let (literals, symbollib, _) = self.standpoint.data();
        let caller_type = symbollib
            .get_expression_type(&call.caller, literals)
            .unwrap();
        let return_type = distill_as_function_type(&caller_type, symbollib)
            .unwrap()
            .return_type;
        if return_type.is_void() {
            return None;
        }
        let group = RegisterGroup::of(&return_type, symbollib);
        Some(self.memory.get_return_value(group))
    }

    /// Emits a binary expression.
    fn emit_binary_expression(&mut self, binexp: &'a analyzer::TypedBinExpr) -> Option<RegisterId> {
        let mut left_register = self
            .emit_expression(&binexp.left)
            .expect("Left side of binary amounts to void.");
        let mut right_register = self
            .emit_expression(&binexp.right)
            .expect("Right hand side amounts to void.");
        // Compare register groups for "never" type promotion.
        if left_register.1 != right_register.1 {
            if right_register.1 == RegisterGroup::Ether {
                right_register = self.memory.typecast_ether(right_register, left_register.1)
            } else if left_register.1 == RegisterGroup::Ether {
                left_register = self.memory.typecast_ether(left_register, right_register.1)
            }
        }
        assert_eq!(left_register.1, right_register.1);
        return Some(
            self.memory
                .condense_into_one(left_register, right_register, binexp.operator),
        );
    }

    /// Emits an identifier.
    fn emit_identifier(&mut self, id: &'a analyzer::TypedIdent) -> Option<RegisterId> {
        let (_, symbollib, _) = self.standpoint.data();
        let identifier_symbol = symbollib.get(id.value).unwrap();
        match &identifier_symbol.kind {
            /// Loads a function and enqueues its block.
            SemanticSymbolKind::Function {
                is_public,
                is_async,
                params,
                generic_params,
                return_type,
            } => self.emit_named_function_name(id, identifier_symbol),
            _ => todo!(),
        }
    }

    fn emit_named_function_name(
        &mut self,
        id: &'a analyzer::TypedIdent,
        identifier_symbol: &'a analyzer::SemanticSymbol,
    ) -> Option<RegisterId> {
        if !self.callables.has(id.value) {
            let function_declaration = FunctionRetriever::new(self.standpoint, identifier_symbol)
                .retrieve()
                .unwrap();
            self.queue
                .push_back(Callable::NamedFunction(function_declaration));
        }
        let callable_ptr_id = self.callables.get_or_create(id.value);
        let id = self.memory.load_immediate_function_ptr(callable_ptr_id);
        Some(id)
    }
}
