use crate::{GenericParameter, HoverFormatter, Identifier, ScopeAddress, Span, Type};

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
    TypeDeclaration,
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

impl HoverFormatter for FunctionSignature {
    fn to_formatted(&self) -> String {
        // Construct function signature.
        let mut string = String::new();

        if self.is_public {
            string.push_str("public ");
        }

        if self.is_async {
            string.push_str("async ");
        }
        string.push_str("function ");
        string.push_str(&self.name.name);

        // TODO: Generic Parameters.

        string.push('(');

        for (index, parameter) in self.params.iter().enumerate() {
            string.push_str(&parameter.to_formatted());
            if index < self.params.len() - 1 {
                string.push_str(", ");
            }
        }
        string.push(')');

        if let Some(ref rettype) = self.return_type.declared {
            string.push_str(": ");
            string.push_str(&rettype.to_formatted())
        }

        string
    }
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

impl Block {
    pub fn empty(span: Span) -> Self {
        Block {
            statements: vec![],
            span,
        }
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Identifier,
    pub type_label: Type,
    pub is_optional: bool,
}

impl HoverFormatter for Parameter {
    fn to_formatted(&self) -> String {
        let mut string = String::new();
        string.push_str(&self.name.name);
        string.push_str(": ");

        // Display given or inferred type.
        if self.is_optional {
            string.push_str("Maybe<")
        }
        let param_type_str = match self.type_label.declared {
            Some(ref declared) => declared.to_formatted(),
            None => match self.type_label.inferred {
                Some(_) => todo!(),
                None => format!("unknown"),
            },
        };
        string.push_str(&param_type_str);
        if self.is_optional {
            string.push_str(">")
        }
        string
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
            Statement::TypeDeclaration => todo!(),
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
            Statement::FunctionDeclaration(f) => {
                f.span.start = start;
            }
            Statement::RecordDeclaration => todo!(),
            Statement::TraitDeclaration => todo!(),
            Statement::EnumDeclaration => todo!(),
            Statement::TypeDeclaration => todo!(),
            Statement::WhileStatement => todo!(),
            Statement::ForStatement => todo!(),
            Statement::ExpressionStatement => todo!(),
        }
    }
}
