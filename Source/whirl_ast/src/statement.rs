use crate::{Expression, GenericParameter, Identifier, ScopeAddress, Span, Type, TypeExpression};

#[derive(Debug, PartialEq)]
pub enum Statement {
    // Declarations.
    TestDeclaration(TestDeclaration),
    UseDeclaration(UseDeclaration),
    VariableDeclaration,
    ShorthandVariableDeclaration(ShorthandVariableDeclaration),
    ConstantDeclaration,
    ModelDeclaration(ModelDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    RecordDeclaration,
    TraitDeclaration,
    EnumDeclaration(EnumDeclaration),
    TypeDeclaration(TypeDeclaration),
    // Control Statements.
    WhileStatement,
    ForStatement,
    // Expression statements.
    ExpressionStatement(Expression),
    /// An expression without the semicolon.
    FreeExpression(Expression),
}

/// A node for a use declaration in the AST.
#[derive(Debug, PartialEq)]
pub struct UseDeclaration {
    pub target: UseTarget,
    pub is_public: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum UsePath {
    /// Importing the module as a namespace. e.g. `use ExternalModule;`
    Me,
    /// Importing a single item. e.g. `use ExternalModule.Item;`
    Item(Box<UseTarget>),
    /// Importing a list of items. e.g. `use ExternalModule.{Item1, Item2};`
    List(Vec<UseTarget>),
}

#[derive(Debug, PartialEq)]
pub struct UseTarget {
    /// Name of the module imported.
    pub name: Identifier,
    /// Items imported.
    pub path: UsePath,
}

/// Entry for a use import target.
pub struct UseTargetSignature {
    /// Name of the import target.
    pub name: Identifier,
    /// Whether or not the import is reexported.
    pub is_public: bool,
}

/// A node in the AST for a shorthand `:=` variable declaration.
#[derive(Debug, PartialEq)]
pub struct ShorthandVariableDeclaration {
    pub address: ScopeAddress,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
/// Node in the AST for a model declaration.
pub struct ModelDeclaration {
    pub address: ScopeAddress,
    pub body: ModelBody,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ModelBody {
    pub properties: Vec<ModelProperty>,
    pub constructor: Option<Block>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ModelProperty {
    pub index: usize,
    pub _type: ModelPropertyType,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ModelPropertyType {
    /// Node for a property.
    Attribute,
    /// Node for a method.
    Method { body: Block },
    /// Node for a trait implementation.
    TraitImpl { owner_trait: Type, body: Block },
}

#[derive(Debug)]
pub struct ModelSignature {
    /// Name of the model.
    pub name: Identifier,
    /// Doc comments annotating the function, if any.
    pub info: Option<Vec<String>>,
    /// Whether it was denoted as public.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The constructor parameters.
    pub parameters: Vec<Parameter>,
    /// Implemented Traits.
    pub implementations: Vec<Type>,
    /// The properties of the model.
    pub attributes: Vec<AttributeSignature>,
    /// The methods of the model.
    pub methods: Vec<MethodSignature>,
}

/// Entry to mark an attribute.
#[derive(Debug)]
pub struct AttributeSignature {
    /// Name of the attribute.
    pub name: Identifier,
    /// Documentation about the attribute.
    pub info: Option<Vec<String>>,
    /// Whether or not it is denoted by public.
    pub is_public: bool,
    /// The variable type.
    pub var_type: Type,
}

/// Entry to mark a method.
#[derive(Debug)]
pub struct MethodSignature {
    /// Name of the method.
    pub name: Identifier,
    /// Doc comments annotating the method, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the method is declared as static.
    pub is_static: bool,
    /// Whether or not the method is denoted by `async`.
    pub is_async: bool,
    /// Whether or not the method is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The parameters of the function, if any.
    pub params: Vec<Parameter>,
    /// Optional return type.
    pub return_type: Type,
}

/// Entry to mark a variable.
#[derive(Debug)]
pub struct VariableSignature {
    /// Name of the variable.
    pub name: Identifier,
    /// Documentation about the variable, if any.
    pub info: Option<Vec<String>>,
    /// Whether it was declared with shorthand syntax or not.
    pub is_shorthand: bool,
    /// Whether or not the variable is denoted by `public`.
    pub is_public: bool,
    /// The variable's assigned type.
    pub var_type: Type,
}

/// A node for a test block.
#[derive(Debug, PartialEq)]
pub struct TestDeclaration {
    pub name: String,
    pub name_span: Span,
    pub body: Block,
    pub span: Span,
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

#[derive(Debug, PartialEq)]
pub struct Parameter {
    /// Name of the parameter.
    pub name: Identifier,
    /// Parameter type label.
    pub type_label: Type,
    /// Whether or not the parameter is optional.
    pub is_optional: bool,
    /// Doc comments annotating the parameter, if any.
    pub info: Option<Vec<String>>,
}

/// Node for an enumerated type.
#[derive(Debug, PartialEq)]
pub struct EnumDeclaration {
    pub address: ScopeAddress,
    pub span: Span,
}

/// Entry to mark an enum.
#[derive(Debug)]
pub struct EnumSignature {
    /// enum name.
    pub name: Identifier,
    /// Doc comments annotating the enum, if any.
    pub info: Option<Vec<String>>,
    /// Whether or not the function is denoted by `public`.
    pub is_public: bool,
    /// Generic Parameters of the function, if any.
    pub generic_params: Option<Vec<GenericParameter>>,
    /// The enum variants.
    pub variants: Vec<EnumVariant>,
}

/// Entry to mark an enum variant.
#[derive(Debug)]
pub struct EnumVariant {
    /// variant name.
    pub name: Identifier,
    /// Doc comments annotating the variant, if any.
    pub info: Option<Vec<String>>,
    pub tagged_type: Option<TypeExpression>,
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

impl Statement {
    pub fn is_variable_declaration(&self) -> bool {
        matches!(self, Statement::VariableDeclaration)
    }

    pub fn span(&self) -> Span {
        match self {
            Statement::TestDeclaration(t) => t.span,
            Statement::UseDeclaration(u) => u.span,
            Statement::VariableDeclaration => todo!(),
            Statement::ConstantDeclaration => todo!(),
            Statement::ModelDeclaration(c) => c.span,
            Statement::FunctionDeclaration(f) => f.span,
            Statement::RecordDeclaration => todo!(),
            Statement::TraitDeclaration => todo!(),
            Statement::EnumDeclaration(e) => e.span,
            Statement::TypeDeclaration(t) => t.span,
            Statement::WhileStatement => todo!(),
            Statement::ForStatement => todo!(),
            Statement::ExpressionStatement(e) | Statement::FreeExpression(e) => e.span(),
            Statement::ShorthandVariableDeclaration(v) => v.span,
        }
    }
    /// Dynamically change the starting point of the statement.
    pub fn set_start(&mut self, start: [u32; 2]) {
        match self {
            Statement::TestDeclaration(t) => t.span.start = start,
            Statement::UseDeclaration(u) => u.span.start = start,
            Statement::VariableDeclaration => todo!(),
            Statement::ConstantDeclaration => todo!(),
            Statement::ModelDeclaration(c) => c.span.start = start,
            Statement::FunctionDeclaration(f) => f.span.start = start,
            Statement::RecordDeclaration => todo!(),
            Statement::TraitDeclaration => todo!(),
            Statement::EnumDeclaration(e) => e.span.start = start,
            Statement::TypeDeclaration(t) => t.span.start = start,
            Statement::WhileStatement => todo!(),
            Statement::ForStatement => todo!(),
            Statement::ExpressionStatement(e) | Statement::FreeExpression(e) => e.set_start(start),
            Statement::ShorthandVariableDeclaration(v) => v.span.start = start,
        }
    }
}
