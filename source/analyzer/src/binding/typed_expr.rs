use super::LiteralIndex;
use crate::{EvaluatedType, IntermediateType, Literal, SymbolIndex, SymbolTable, TypedBlock};
use ast::{AssignOperator, BinOperator, LogicOperator, Span, UnaryOperator, UpdateOperator};

#[derive(Debug, PartialEq)]
pub enum TypedExpression {
    Identifier(TypedIdent),
    Literal(LiteralIndex),
    NewExpr(Box<TypedNewExpr>),
    ThisExpr(TypedThisExpr),
    CallExpr(Box<TypedCallExpr>),
    FnExpr(Box<TypedFnExpr>),
    Block(TypedBlock),
    IfExpr(Box<TypedIfExpr>),
    AccessExpr(Box<TypedAccessExpr>),
    ArrayExpr(TypedArrayExpr),
    IndexExpr(Box<TypedIndexExpr>),
    BinaryExpr(Box<TypedBinExpr>),
    AssignmentExpr(Box<TypedAssignmentExpr>),
    UnaryExpr(Box<TypedUnaryExpr>),
    LogicExpr(Box<TypedLogicExpr>),
    UpdateExpr(Box<TypedUpdateExpr>),
}
impl TypedExpression {
    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier(_))
    }
}

#[derive(Debug, PartialEq)]
pub struct TypedIdent {
    pub value: SymbolIndex,
    pub start: [u32; 2],
}

#[derive(Debug, PartialEq)]
pub struct TypedNewExpr {
    pub value: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedThisExpr {
    pub model_or_trait: Option<SymbolIndex>,
    pub start_line: u32,
    pub start_character: u32,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedCallExpr {
    pub caller: TypedExpression,
    pub arguments: Vec<TypedExpression>,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedFnExpr {
    pub is_async: bool,
    pub generic_params: Vec<SymbolIndex>,
    pub params: Vec<SymbolIndex>,
    pub return_type: Option<IntermediateType>,
    pub body: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedBinExpr {
    pub left: TypedExpression,
    pub operator: BinOperator,
    pub right: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedLogicExpr {
    pub left: TypedExpression,
    pub operator: LogicOperator,
    pub right: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedUnaryExpr {
    pub operator: UnaryOperator,
    pub operand: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedUpdateExpr {
    pub operator: UpdateOperator,
    pub operand: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedIfExpr {
    pub condition: TypedExpression,
    pub consequent: TypedBlock,
    pub alternate: Option<TypedElse>,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedAccessExpr {
    pub object: TypedExpression,
    pub property: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedElse {
    pub expression: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedArrayExpr {
    pub elements: Vec<TypedExpression>,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedIndexExpr {
    pub object: TypedExpression,
    pub index: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

#[derive(Debug, PartialEq)]
pub struct TypedAssignmentExpr {
    pub left: TypedExpression,
    pub operator: AssignOperator,
    pub right: TypedExpression,
    pub span: Span,
    pub inferred_type: EvaluatedType,
}

pub fn span_of_typed_expression(
    expression: &TypedExpression,
    symboltable: &SymbolTable,
    literals: &[Literal],
) -> Span {
    match expression {
        TypedExpression::Identifier(i) => {
            let symbol = symboltable.get(i.value).unwrap();
            Span::on_line(i.start, symbol.name.len() as u32)
        }
        TypedExpression::Literal(l) => match &literals[l.0] {
            Literal::StringLiteral { value, .. } => value.span,
            Literal::NumericLiteral { value, .. } => value.span,
            Literal::BooleanLiteral {
                value,
                start_line,
                start_character,
                ..
            } => Span {
                start: [*start_line, *start_character],
                end: [*start_line, *start_character + if *value { 4 } else { 5 }],
            },
        },
        TypedExpression::CallExpr(c) => c.span,
        TypedExpression::FnExpr(f) => f.span,
        TypedExpression::Block(b) => b.span,
        TypedExpression::IfExpr(i) => i.span,
        TypedExpression::ArrayExpr(a) => a.span,
        TypedExpression::IndexExpr(i) => i.span,
        TypedExpression::BinaryExpr(b) => b.span,
        TypedExpression::AssignmentExpr(a) => a.span,
        TypedExpression::UnaryExpr(u) => u.span,
        TypedExpression::LogicExpr(l) => l.span,
        TypedExpression::AccessExpr(a) => a.span,
        TypedExpression::ThisExpr(t) => Span {
            start: [t.start_line, t.start_character],
            end: [t.start_line, t.start_character + 4],
        },
        TypedExpression::NewExpr(n) => n.span,
        TypedExpression::UpdateExpr(u) => u.span,
    }
}
