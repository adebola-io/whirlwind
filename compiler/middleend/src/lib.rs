pub struct Callable {
    name: String,
    statements: Vec<MiddleStatement>,
}

pub struct Symbols {}

pub enum MiddleStatement {
    Expression(MiddleExpression),
    WhileLoop(MiddleWhileLoop),
    VariableDeclaration(MiddleVariableDecl),
    ConstantDeclaration(MiddleConstantDecl),
    Break(MiddleBreak),
    Continue(MiddleContinue),
    Return(MiddleReturn),
}

pub struct MiddleConstantDecl {}

pub struct MiddleContinue {}

pub struct MiddleReturn {}

pub struct MiddleBreak {}

pub struct MiddleWhileLoop {
    condition: MiddleExpression,
    body: MiddleBlock,
}

pub struct MiddleVariableDecl {}

pub enum MiddleExpression {
    NumericOp(Box<NumericOp>),
    Block(MiddleBlock),
}

pub struct MiddleBlock {}

pub struct NumericOp {}

pub struct Intermediary {
    pub callables: Vec<Callable>,
}
