use crate::{
    DiscreteType, Expression, Identifier, Positioning, ScopeAddress, Span, Spannable,
    TypeExpression,
};

#[derive(Debug, PartialEq)]
pub enum Statement {
    RecordDeclaration,
    // Declarations.
    TestDeclaration(TestDeclaration),
    UseDeclaration(UseDeclaration),
    VariableDeclaration(VariableDeclaration),
    ShorthandVariableDeclaration(ShorthandVariableDeclaration),
    ConstantDeclaration(ConstantDeclaration),
    ModelDeclaration(ModelDeclaration),
    ModuleDeclaration(ModuleDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    InterfaceDeclaration(InterfaceDeclaration),
    EnumDeclaration(EnumDeclaration),
    TypeDeclaration(TypeDeclaration),
    // Control Statements.
    WhileStatement(WhileStatement),
    ReturnStatement(ReturnStatement),
    ForStatement(ForStatement),
    ContinueStatement(ContinueStatement),
    BreakStatement(BreakStatement),
    // Expression statements.
    ExpressionStatement(Expression),
    /// An expression without the semicolon.
    FreeExpression(Expression),
}

#[derive(Debug, PartialEq)]
/// A node for a module declaration.
pub struct ModuleDeclaration {
    pub span: Span,
}

/// A node for a use declaration in the AST.
#[derive(Debug, PartialEq)]
pub struct UseDeclaration {
    pub addresses: Vec<ScopeAddress>,
    pub target: UseTarget,
    pub is_public: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UsePath {
    /// Importing the module as a namespace. e.g. `use ExternalModule;`
    Me,
    /// Importing a single item. e.g. `use ExternalModule.Item;`
    Item(Box<UseTarget>),
    /// Importing a list of items. e.g. `use ExternalModule.{Item1, Item2};`
    List(Vec<UseTarget>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct UseTarget {
    /// Name of the module imported.
    pub name: Identifier,
    /// Items imported.
    pub path: UsePath,
}
impl UseTarget {
    /// Returns the ends of the target.
    pub fn leaves(&self) -> Vec<Identifier> {
        let mut leaves = vec![];
        match &self.path {
            UsePath::Me => leaves.push(self.name.clone()),
            UsePath::Item(target) => leaves.append(&mut target.leaves()),
            UsePath::List(list) => {
                for target in list {
                    leaves.append(&mut target.leaves())
                }
            }
        }
        leaves
    }
    /// Separate a single complex use target into multiple distinct single item targets.
    // todo: find more efficient way to do this.
    pub fn scatter(&self) -> Vec<UseTarget> {
        let mut targets = vec![];
        match &self.path {
            UsePath::Me => targets.push(self.clone()),
            UsePath::Item(sub_target) => {
                let sub_targets = sub_target.scatter();
                for sub_target in sub_targets {
                    let _self = UseTarget {
                        name: self.name.clone(),
                        path: UsePath::Item(Box::new(sub_target)),
                    };
                    targets.push(_self);
                }
            }
            UsePath::List(list) => {
                for sub_target in list {
                    let sub_targets = sub_target.scatter();
                    for sub_target in sub_targets {
                        let _self = UseTarget {
                            name: self.name.clone(),
                            path: UsePath::Item(Box::new(sub_target)),
                        };
                        targets.push(_self);
                    }
                }
            }
        }
        targets
    }
}

#[derive(Debug, Hash)]
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

/// A node in the AST for a constant variable declaration.
#[derive(Debug, PartialEq)]
pub struct ConstantDeclaration {
    pub address: ScopeAddress,
    /// The constant's assigned value.
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub addresses: Vec<ScopeAddress>,
    pub value: Option<Expression>,
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
    /// Node for a interface implementation.
    InterfaceImpl {
        interface_target: Vec<DiscreteType>,
        body: Block,
    },
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
/// For efficiency most of its details are stored in the module ambience.
#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub address: ScopeAddress,
    pub body: Block,
    pub span: Span,
}

/// A node for a interface declaration in the AST.
#[derive(Debug, PartialEq)]
pub struct InterfaceDeclaration {
    pub address: ScopeAddress,
    pub body: InterfaceBody,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct InterfaceBody {
    pub properties: Vec<InterfaceProperty>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct InterfaceProperty {
    pub index: usize,
    pub _type: InterfacePropertyType,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum InterfacePropertyType {
    /// A method that is to be implemented.
    Signature,
    /// A method with a default implementation.
    Method { body: Block },
}

#[derive(Debug, PartialEq)]
pub struct Location {
    pub module: String,
    pub instances: Vec<Span>,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub scope_id: usize,
    pub statements: Vec<Statement>,
    pub span: Span,
}

/// A node for a type declaration.
/// As wih functions, most of its info is in the module ambience.
#[derive(Debug, PartialEq)]
pub struct TypeDeclaration {
    pub address: ScopeAddress,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ForStatement {
    pub items: Vec<ScopeAddress>,
    pub iterator: Expression,
    pub label: Option<ScopeAddress>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ContinueStatement {
    pub label: Option<Identifier>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct BreakStatement {
    pub label: Option<Identifier>,
    pub span: Span,
}

/// Node for an enumerated type.
#[derive(Debug, PartialEq)]
pub struct EnumDeclaration {
    pub address: ScopeAddress,
    pub span: Span,
}

/// Entry to mark an enum variant.
#[derive(Debug, Hash)]
pub struct EnumVariant {
    /// variant name.
    pub name: Identifier,
    /// Doc comments annotating the variant, if any.
    pub info: Option<Vec<String>>,
    pub tagged_types: Vec<TypeExpression>,
    pub span: Span,
}

impl Block {
    pub fn empty(scope_id: usize, span: Span) -> Self {
        Block {
            scope_id,
            statements: vec![],
            span,
        }
    }
}

impl Statement {
    pub fn is_variable_declaration(&self) -> bool {
        matches!(self, Statement::VariableDeclaration(_))
    }

    pub fn is_import(&self) -> bool {
        matches!(self, Statement::UseDeclaration(_))
    }
}

impl Positioning for Statement {
    fn move_by_line(&mut self, _offset: i32) {
        todo!()
    }
    fn move_by_character(&mut self, _offset: i32) {
        todo!()
    }
    // Here be pure hackery.
    /// This function attempts to pinpoint the closest nodes in a statement's parse tree to a particular span.
    /// These are the statements that will reparsed by the module when there are text changes.
    fn closest_nodes_to(&self, span: Span) -> Vec<&Self> {
        let mut nodes = vec![];
        let statement_span = self.span();
        // The span is on the fringes of the statement.
        if statement_span.is_adjacent_to(span.start) || statement_span.is_adjacent_to(span.end) {
            nodes.push(self);
        } else if statement_span.encloses(span) {
            // The span is within the statement.
            match self {
                Statement::TestDeclaration(TestDeclaration { body, .. })
                | Statement::FunctionDeclaration(FunctionDeclaration { body, .. })
                | Statement::WhileStatement(WhileStatement { body, .. })
                | Statement::ForStatement(ForStatement { body, .. }) => {
                    if body.span.encloses(span) {
                        // The span is within the body of the statement, not just the statement itself.
                        nodes.append(&mut collect_closest_within_block(self, body, span));
                    }
                }
                // Within a interface declaration.
                Statement::InterfaceDeclaration(decl) => {
                    for prop in &decl.body.properties {
                        if prop.span.is_in_vicinity(span) {
                            match &prop._type {
                                InterfacePropertyType::Signature => nodes.push(self),
                                InterfacePropertyType::Method { body } => {
                                    if body.span.encloses(span) {
                                        // The span is within the body of a method, not just the interface itself.
                                        nodes.append(&mut collect_closest_within_block(
                                            self, body, span,
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
                // Within a model declaration.
                Statement::ModelDeclaration(decl) => {
                    for prop in &decl.body.properties {
                        if prop.span.is_in_vicinity(span) {
                            match &prop._type {
                                ModelPropertyType::Attribute => nodes.push(self),
                                ModelPropertyType::InterfaceImpl { body, .. }
                                | ModelPropertyType::Method { body } => {
                                    if body.span.encloses(span) {
                                        // The span is within the body of a method, not just the model itself.
                                        nodes.append(&mut collect_closest_within_block(
                                            self, body, span,
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
                // Within a shorthand variable declaration.
                Statement::ShorthandVariableDeclaration(ShorthandVariableDeclaration {
                    value,
                    ..
                })
                | Statement::VariableDeclaration(VariableDeclaration {
                    value: Some(value), ..
                }) => {
                    if value.span().encloses(span) {
                        // span is within the expression value of the node.
                        nodes.append(&mut collect_closest_within_expression(self, value, span))
                    }
                }
                // Within a shorthand variable declaration.
                Statement::ConstantDeclaration(const_decl) => {
                    let expression = &const_decl.value;
                    if expression.span().encloses(span) {
                        // span is within the expression value of the node.
                        nodes.append(&mut collect_closest_within_expression(
                            self, expression, span,
                        ))
                    }
                }
                Statement::ExpressionStatement(expression)
                | Statement::FreeExpression(expression) => nodes.append(
                    &mut collect_closest_within_expression(self, expression, span),
                ),
                // Statement::RecordDeclaration => todo!(),
                // Statement::VariableDeclaration => todo!(),
                _ => nodes.push(self),
            }
            if nodes.len() == 0 {
                nodes.push(self);
            }
        } else if statement_span.contains(span.start) || statement_span.contains(span.end) {
            // span is half in or out of the statement.
            nodes.push(self)
        }
        nodes
    }
}

impl Spannable for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::TestDeclaration(t) => t.span,
            Statement::UseDeclaration(u) => u.span,
            Statement::VariableDeclaration(v) => v.span,
            Statement::ConstantDeclaration(c) => c.span,
            Statement::ModelDeclaration(c) => c.span,
            Statement::FunctionDeclaration(f) => f.span,
            Statement::RecordDeclaration => todo!(),
            Statement::InterfaceDeclaration(t) => t.span,
            Statement::EnumDeclaration(e) => e.span,
            Statement::TypeDeclaration(t) => t.span,
            Statement::WhileStatement(w) => w.span,
            Statement::ForStatement(f) => f.span,
            Statement::ExpressionStatement(e) | Statement::FreeExpression(e) => e.span(),
            Statement::ShorthandVariableDeclaration(v) => v.span,
            Statement::ModuleDeclaration(m) => m.span,
            Statement::ReturnStatement(r) => r.span,
            Statement::ContinueStatement(c) => c.span,
            Statement::BreakStatement(b) => b.span,
        }
    }
    fn set_start(&mut self, start: [u32; 2]) {
        match self {
            Statement::TestDeclaration(t) => t.span.start = start,
            Statement::UseDeclaration(u) => u.span.start = start,
            Statement::VariableDeclaration(v) => v.span.start = start,
            Statement::ConstantDeclaration(c) => c.span.start = start,
            Statement::ModelDeclaration(c) => c.span.start = start,
            Statement::FunctionDeclaration(f) => f.span.start = start,
            Statement::RecordDeclaration => todo!(),
            Statement::InterfaceDeclaration(t) => t.span.start = start,
            Statement::EnumDeclaration(e) => e.span.start = start,
            Statement::TypeDeclaration(t) => t.span.start = start,
            Statement::WhileStatement(w) => w.span.start = start,
            Statement::ForStatement(f) => f.span.start = start,
            Statement::ExpressionStatement(e) | Statement::FreeExpression(e) => e.set_start(start),
            Statement::ShorthandVariableDeclaration(v) => v.span.start = start,
            Statement::ModuleDeclaration(m) => m.span.start = start,
            Statement::ReturnStatement(r) => r.span.start = start,
            Statement::ContinueStatement(c) => c.span.start = start,
            Statement::BreakStatement(b) => b.span.start = start,
        }
    }
    fn captured_scopes(&self) -> Vec<usize> {
        let mut nested = vec![];
        match self {
            Statement::TestDeclaration(TestDeclaration { body, .. })
            | Statement::FunctionDeclaration(FunctionDeclaration { body, .. })
            | Statement::WhileStatement(WhileStatement { body, .. }) => {
                nested.push(body.scope_id);
            }
            Statement::ModelDeclaration(_) => todo!(),
            Statement::InterfaceDeclaration(_) => todo!(),
            Statement::ShorthandVariableDeclaration(ShorthandVariableDeclaration {
                value: expression,
                ..
            })
            | Statement::ConstantDeclaration(ConstantDeclaration {
                value: expression, ..
            })
            | Statement::VariableDeclaration(VariableDeclaration {
                value: Some(expression),
                ..
            })
            | Statement::ExpressionStatement(expression)
            | Statement::FreeExpression(expression) => {
                nested.append(&mut expression.captured_scopes())
            }
            Statement::ForStatement(f) => nested.push(f.body.scope_id),
            // Statement::RecordDeclaration => todo!(),
            _ => {}
        }
        nested
    }
}

/// Collect the closest statements to a span within a block of statements.
pub fn collect_closest_within_block<'a>(
    owner_statement: &'a Statement,
    body: &'a Block,
    span: Span,
) -> Vec<&'a Statement> {
    let mut nodes = vec![];
    for statement in &body.statements {
        nodes.append(&mut statement.closest_nodes_to(span));
    }
    // Could not find any matches, parse the entire statement itsefl.
    if nodes.len() == 0 {
        nodes.push(owner_statement);
    }
    nodes
}

/// Macro rule to check within an expression for statements that are near a span, if the span is enclosed by the span of the expression. Does that make sense?
macro_rules! expr_enclose {
    ($owner: expr, $exp: expr, $span: expr) => {
        if $exp.span().encloses($span) {
            return collect_closest_within_expression($owner, &$exp, $span);
        }
    };
}

/// Collect the closest statements to a span within an expression.
pub fn collect_closest_within_expression<'a>(
    owner: &'a Statement,
    expression: &'a Expression,
    span: Span,
) -> Vec<&'a Statement> {
    match expression {
        Expression::UnaryExpr(u) => expr_enclose!(owner, u.operand, span),
        Expression::CallExpr(callexp) => {
            for argument in callexp.arguments.iter() {
                expr_enclose!(owner, argument, span)
            }
        }
        Expression::FnExpr(func_exp) => expr_enclose!(owner, func_exp.body, span),
        Expression::IfExpr(if_exp) => {
            expr_enclose!(owner, if_exp.condition, span);
            if if_exp.consequent.span.encloses(span) {
                return collect_closest_within_block(owner, &if_exp.consequent, span);
            }
            if let Some(_else) = &if_exp.alternate {
                expr_enclose!(owner, _else.expression, span)
            }
        }
        Expression::ArrayExpr(array) => {
            for element in array.elements.iter() {
                expr_enclose!(owner, element, span)
            }
        }
        Expression::AccessExpr(a) => {
            expr_enclose!(owner, a.object, span);
            expr_enclose!(owner, a.property, span);
        }
        Expression::IndexExpr(i) => {
            expr_enclose!(owner, i.object, span);
            expr_enclose!(owner, i.index, span);
        }
        Expression::BinaryExpr(b) => {
            expr_enclose!(owner, b.left, span);
            expr_enclose!(owner, b.right, span);
        }
        Expression::AssignmentExpr(a) => {
            expr_enclose!(owner, a.left, span);
            expr_enclose!(owner, a.right, span);
        }
        Expression::LogicExpr(l) => {
            expr_enclose!(owner, l.left, span);
            expr_enclose!(owner, l.right, span);
        }
        Expression::BlockExpr(body) => return collect_closest_within_block(owner, body, span),
        _ => return vec![owner],
    };
    return vec![owner];
}
