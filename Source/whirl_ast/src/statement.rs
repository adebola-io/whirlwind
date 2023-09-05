use crate::{GenericParameter, Identifier, ScopeAddress, Span, Type, TypeExpression};

#[derive(Debug, PartialEq)]
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
    TypeDeclaration(TypeDeclaration),
    // Control Statements.
    WhileStatement,
    ForStatement,

    ExpressionStatement,
}

/// A node for a function declaration in the AST.
/// For efficiency most of its details are stored in the scope manager.
#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub address: ScopeAddress,
    pub body: Block,
    pub span: Span,
}

/// An entry to mark a function.
#[derive(Debug)]
pub struct FunctionSignature {
    /// Name of the function.
    pub name: Identifier,
    /// Doc comments annotating the function, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the function is denoted by `async`.
    pub is_async: bool,
    /// Whether or not the function is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The parameters of the function, if any.
    pub params: Vec<Parameter>,
    /// Optional return type.
    pub return_type: Type,
}

#[derive(Debug, PartialEq)]
pub struct Location {
    pub module: String,
    pub instances: Vec<Span>,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

/// A node for a type declaration.
/// As wih functions, most of its info is in the scope manager.
#[derive(Debug, PartialEq)]
pub struct TypeDeclaration {
    pub address: ScopeAddress,
    pub span: Span,
}

/// Entry to mark a type.
#[derive(Debug)]
pub struct TypeSignature {
    /// Type name.
    pub name: Identifier,
    /// Doc comments annotating the type, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the function is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    pub value: TypeExpression,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Identifier,
    pub type_label: Type,
    pub is_optional: bool,
}

impl Block {
    pub fn empty(span: Span) -> Self {
        Block {
            statements: vec![],
            span,
        }
    }
}

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
            Statement::FunctionDeclaration(f) => f.span,
            Statement::RecordDeclaration => todo!(),
            Statement::TraitDeclaration => todo!(),
            Statement::EnumDeclaration => todo!(),
            Statement::TypeDeclaration(t) => t.span,
            Statement::WhileStatement => todo!(),
            Statement::ForStatement => todo!(),
            Statement::ExpressionStatement => todo!(),
        }
    }
    /// Dynamically change the starting point of the statement.
    pub fn set_start(&mut self, start: [u32; 2]) {
        match self {
            Statement::TestDeclaration => todo!(),
            Statement::UseDeclaration => todo!(),
            Statement::VariableDeclaration => todo!(),
            Statement::ConstantDeclaration => todo!(),
            Statement::ClassDeclaration => todo!(),
            Statement::FunctionDeclaration(f) => f.span.start = start,
            Statement::RecordDeclaration => todo!(),
            Statement::TraitDeclaration => todo!(),
            Statement::EnumDeclaration => todo!(),
            Statement::TypeDeclaration(t) => t.span.start = start,
            Statement::WhileStatement => todo!(),
            Statement::ForStatement => todo!(),
            Statement::ExpressionStatement => todo!(),
        }
    }
}
