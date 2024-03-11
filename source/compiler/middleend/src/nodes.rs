pub struct Callable {
    pub name: String,
    pub statements: Vec<SimplifiedStatement>,
}

pub struct Symbols {}

pub enum SimplifiedStatement {
    Expression(SimplifiedExpression),
    WhileLoop(SimplifiedWhileLoop),
    VariableDeclaration(SimplifiedVariableDecl),
    Break(SimplifiedBreak),
    Continue(SimplifiedContinue),
    Return(SimplifiedReturn),
}

pub struct SimplifiedConstantDecl {}

pub struct SimplifiedContinue {}

pub struct SimplifiedReturn {}

pub struct SimplifiedBreak {}

pub struct SimplifiedWhileLoop {
    pub condition: SimplifiedExpression,
    pub body: SimplifiedBlock,
}

pub struct SimplifiedVariableDecl {}

pub enum SimplifiedExpression {
    NumericOp(Box<NumericOp>),
    Block(SimplifiedBlock),
}

pub struct SimplifiedBlock {}

pub struct NumericOp {}

pub struct Intermediary {
    pub callables: Vec<Callable>,
}
