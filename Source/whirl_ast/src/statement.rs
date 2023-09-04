use std::sync::{Arc, Mutex};

use crate::{Identifier, Span};

#[derive(Debug)]
pub enum Statement {
    // Declarations.
    TestDeclaration,
    UseDeclaration,
    VariableDeclaration,
    ConstantDeclaration,
    ClassDeclaration,
    FunctionDeclaration(FunctionDeclaration),
    RecordDeclaration,
    TraitDeclaration,
    EnumDeclaration,
    TypeDeclaration,
    PublicDeclaration(PublicDeclaration),
    // Control Statements.
    WhileStatement,
    ForStatement,

    ExpressionStatement,
}

#[derive(Debug)]
pub struct PublicDeclaration {
    pub declaration: Box<Statement>,
    pub span: Span,
}

/// A node for a function declaration in the AST.
#[derive(Debug)]
pub struct FunctionDeclaration {
    pub signature: Arc<Mutex<FunctionSignature>>,
    pub body: Block,
}

/// An entry to mark a function.
#[derive(Debug)]
pub struct FunctionSignature {
    /// Name of the function.
    pub name: Identifier,
    /// Whether or not the function is denoted by `async`.
    pub is_async: bool,
    /// The parameters of the function, if any.
    pub params: Vec<Parameter>,
    /// Doc comments annotating the function, if any.
    pub info: Option<Vec<String>>,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// Optional return type.
    pub return_type: Option<ParserType>,
    /// The full range of the function text.
    pub full_span: Span,
    /// Modules where this function is used or referenced.
    pub references: Vec<Location>,
}

#[derive(Debug)]
pub struct Location {
    pub module: String,
    pub instances: Vec<Span>,
}

#[derive(Debug)]
pub struct GenericParameter {}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug)]
pub struct SemanticType {}

impl std::fmt::Display for SemanticType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{unknown}}")
    }
}

impl Default for SemanticType {
    fn default() -> Self {
        Self {}
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Identifier,
    pub type_label: Option<ParserType>,
    pub inferred_type: SemanticType,
    pub is_optional: bool,
}
impl std::fmt::Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name.name, self.inferred_type)
    }
}

#[derive(Debug)]
pub struct ParserType {}

impl Statement {
    pub fn is_variable_declaration(&self) -> bool {
        matches!(self, Statement::VariableDeclaration)
    }

    pub fn span(&self) -> Span {
        match self {
            Statement::TestDeclaration => todo!(),
            Statement::UseDeclaration => todo!(),
            Statement::VariableDeclaration => todo!(),
            Statement::ConstantDeclaration => todo!(),
            Statement::ClassDeclaration => todo!(),
            Statement::FunctionDeclaration(f) => f.signature.lock().unwrap().full_span,
            Statement::RecordDeclaration => todo!(),
            Statement::TraitDeclaration => todo!(),
            Statement::EnumDeclaration => todo!(),
            Statement::TypeDeclaration => todo!(),
            Statement::PublicDeclaration(_) => todo!(),
            Statement::WhileStatement => todo!(),
            Statement::ForStatement => todo!(),
            Statement::ExpressionStatement => todo!(),
        }
    }
}
