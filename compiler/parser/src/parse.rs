use ast::{
    AccessExpr, ArrayExpr, ArrayType, AssignmentExpr, AttributeSignature, BinaryExpr, Block,
    BoundConstraintType, Bracket::*, BreakStatement, CallExpr, Comment, ConstantDeclaration,
    ConstantSignature, ContinueStatement, DiscreteType, EnumDeclaration, EnumSignature,
    EnumVariant, Expression, ExpressionPrecedence, ForStatement, FunctionDeclaration, FunctionExpr,
    FunctionSignature, FunctionalType, GenericParameter, Identifier, IfExpression, IndexExpr,
    InterfaceBody, InterfaceDeclaration, InterfaceProperty, InterfacePropertyType,
    InterfaceSignature, Keyword::*, LogicExpr, LoopLabel, LoopVariable, MaybeType, MemberType,
    MethodSignature, ModelBody, ModelDeclaration, ModelProperty, ModelPropertyType, ModelSignature,
    ModuleAmbience, ModuleDeclaration, Operator::*, Parameter, ReturnStatement, ScopeAddress,
    ScopeEntry, ScopeType, ShorthandVariableDeclaration, ShorthandVariableSignature, Span,
    Spannable, Statement, TernaryType, TestDeclaration, ThisExpr, Token, TokenType, TypeClause,
    TypeDeclaration, TypeExpression, TypeSignature, UnaryExpr, UnionType, UpdateExpr,
    UseDeclaration, UsePath, UseTarget, UseTargetSignature, VariableDeclaration, VariablePattern,
    VariableSignature, WhileStatement, WhirlBoolean, WhirlNumber, WhirlString,
};
use errors::{self as errors, expected, ParseError};
use lexer::Lexer;
use std::cell::RefCell;
use utils::Partial;

/// A recursive descent parser that reads tokens lazily and returns statements.
/// It keeps tracks of three tokens to allow for limited backtracking.
pub struct Parser<L: Lexer> {
    pub lexer: RefCell<L>,
    module_ambience: RefCell<ModuleAmbience>,
    present: RefCell<Option<Token>>,
    past: RefCell<Option<Token>>,
    future: RefCell<Option<Token>>,
    doc_comments: RefCell<Vec<String>>,
    precedence_stack: RefCell<Vec<ExpressionPrecedence>>,
    /// Handles ambiguity caused by nested generic arguments conflicting with bit right shift.
    waiting_for_second_angular_bracket: RefCell<bool>,
    pub debug_allow_global_expressions: bool,
    is_in_error_state: RefCell<bool>,
}

type Fallible<T> = Result<T, ParseError>;
type Imperfect<T> = Partial<T, ParseError>;

/// Shorthand for unconditional current token confirmation.<br>
///
/// Basically generates the rule:<br>
/// If the current token is none, or it has a token type not equal to X, return an expectation error.
macro_rules! expect {
    ($expected:expr, $s:expr) => {{
        match $s.token() {
            Some(token) => {
                if token._type != $expected {
                    return Err(errors::expected($expected, token.span));
                }
            }
            None => return Err(errors::expected($expected, $s.last_token_end())),
        }
    };};
}

/// Variant of `expect!` that returns an empty partial instead of a result.
macro_rules! expect_or_return {
    ($expected:expr, $s:expr) => {{
        match $s.token() {
            Some(token) => {
                if token._type != $expected {
                    return Partial::from_error(errors::expected($expected, token.span));
                }
            }
            None => return Partial::from_error(errors::expected($expected, $s.last_token_end())),
        }
    };};
}

/// Returns an empty partial if a result is an err.
macro_rules! check {
    ($x: expr) => {
        match $x {
            Ok(x) => x,
            Err(y) => return Partial::from_error(y),
        }
    };
}

/// shorthand to return an empty partial if the token stream has ended.
macro_rules! if_ended {
    ($error: expr, $self: ident) => {
        if let None = $self.token() {
            return Partial::from_tuple((None, vec![$error]));
        }
    };
}

impl<L: Lexer> Parser<L> {
    /// Creates a parser from a lexer.
    pub fn from_lexer(lexer: L) -> Self {
        let module_id = lexer.module_id();
        Self {
            lexer: RefCell::new(lexer),
            module_ambience: RefCell::new(ModuleAmbience::new(module_id)),
            present: RefCell::new(None),
            past: RefCell::new(None),
            future: RefCell::new(None),
            doc_comments: RefCell::new(vec![]),
            precedence_stack: RefCell::new(vec![]),
            waiting_for_second_angular_bracket: RefCell::new(false),
            debug_allow_global_expressions: false,
            is_in_error_state: RefCell::new(false),
        }
    }

    /// Returns the next token from the lexer.
    fn token(&self) -> Option<&mut Token> {
        unsafe { (*self.present.as_ptr()).as_mut() }
    }
    /// Advance to the next token.
    fn advance(&self) {
        self.past.replace(self.present.take());
        self.present
            .replace(self.future.take().or_else(|| self.next_useful_token()));
    }
    /// Keep track of documentation comments and returns the next syntactically useful token.
    fn next_useful_token(&self) -> Option<Token> {
        loop {
            let token = self.lexer.borrow_mut().next()?;
            match token._type {
                TokenType::Comment(comment) => {
                    if let Comment::DocComment(text) = comment {
                        self.doc_comments.borrow_mut().push(text)
                    }
                }
                TokenType::Invalid(_) => {}
                _ => return Some(token),
            }
        }
    }
    // // / Rewinds to the last token.
    // fn back(&self) {
    //     self.future.replace(self.present.take());
    //     self.present.replace(self.past.take());
    // }

    /// Push a precedence to the stack.
    fn push_precedence(&self, precedence: ExpressionPrecedence) {
        self.precedence_stack.borrow_mut().push(precedence);
    }

    /// Returns a reference to the module ambience.
    pub fn module_ambience(&self) -> &mut ModuleAmbience {
        unsafe { &mut *self.module_ambience.as_ptr() }
    }

    /// Returns the span of the token before.
    fn last_token_span(&self) -> Span {
        self.past
            .borrow()
            .as_ref()
            .map(|token| token.span)
            .unwrap_or(Span::default())
    }

    /// Returns the end of the span of the token before.
    fn last_token_end(&self) -> Span {
        self.past
            .borrow()
            .as_ref()
            .map(|token| Span::from([token.span.end, token.span.end]))
            .unwrap_or(Span::default())
    }

    /// Return an error if there is no longer a token in the stream.
    fn ended(&self, error: ParseError) -> Fallible<()> {
        match self.token() {
            Some(_) => Ok(()),
            None => Err(error),
        }
    }

    /// Pulls out the current counting documentation comments.
    fn get_doc_comment(&self) -> Option<Vec<String>> {
        let doc_comments = std::mem::take(unsafe { &mut *self.doc_comments.as_ptr() });
        if doc_comments.len() > 0 {
            Some(doc_comments)
        } else {
            None
        }
    }

    /// Check an expression against the precedence chart.
    fn is_lower_or_equal_precedence(&self, precedence: ExpressionPrecedence) -> bool {
        match (self.precedence_stack.borrow().last(), precedence) {
            // Right associativity for exponential.
            (Some(ExpressionPrecedence::PowerOf), ExpressionPrecedence::PowerOf) => false,
            // Right associativity for assignment.
            (Some(ExpressionPrecedence::Assignment), ExpressionPrecedence::Assignment) => false,
            (Some(p), precedence) => *p <= precedence,
            _ => false,
        }
    }

    /// Create a false, placeholder block if it is missing, so that parsing can continue normally.
    fn create_fake_block(&self, scopetype: ScopeType, span: Span) -> Block {
        let ambience = self.module_ambience();
        ambience.enter(scopetype);
        let block = Block {
            scope_id: ambience.current_scope(),
            statements: vec![],
            span,
        };
        ambience.leave_scope();
        block
    }
}

// Expressions
impl<L: Lexer> Parser<L> {
    /// Parses an expression to return either an expression statement or a free expression.
    fn expression_start(&self) -> Imperfect<Statement> {
        // Parse a variable declaration instead if:
        // - the current token is an identifier, and
        // - the next token is a colon or a colon-assign.
        let mut partial = if let Some(token) = self.token() {
            if matches!(token._type, TokenType::Ident(_)) {
                let name = check!(self.identifier());
                match self.token() {
                    Some(token)
                        if matches!(
                            token._type,
                            TokenType::Operator(Colon) | TokenType::Operator(ColonAssign),
                        ) =>
                    {
                        return self.shorthand_variable_declaration(name);
                    }
                    _ => self.spring(Partial::from_value(Expression::Identifier(name))),
                }
            } else {
                self.expression()
            }
        } else {
            self.expression()
        };

        if partial.is_some()
            && self.module_ambience().is_in_global_scope()
            && !self.debug_allow_global_expressions
        {
            partial.errors.push(errors::global_control(
                partial.value.as_ref().unwrap().span(),
            ))
        }

        // println!("Current token = {:?}", self.token());
        match self.token() {
            Some(t) => match t._type {
                TokenType::Operator(SemiColon) => {
                    self.advance(); // Move past ;
                    partial.map(|exp| Statement::ExpressionStatement(exp))
                }
                // TokenType::Ident(_) => partial
                //     .with_error(expected(
                //         TokenType::Operator(SemiColon),
                //         self.last_token_end(),
                //     ))
                //     .map(|expression| Statement::FreeExpression(expression)),
                _ => partial.map(|expression| Statement::FreeExpression(expression)),
            },
            None => partial.map(|expression| Statement::FreeExpression(expression)),
        }
    }

    /// Parses an expression.
    fn expression(&self) -> Imperfect<Expression> {
        if_ended!(errors::expression_expected(self.last_token_end()), self);

        let token = self.token().unwrap();

        let expression = match token._type {
            TokenType::Keyword(Fn) => self.function_expression(false, token.span.start),
            TokenType::Keyword(Async) => self.async_function_expression(),
            TokenType::Keyword(True | False) => self.spring(Partial::from(self.boolean_literal())),
            // TokenType::Keyword(New) => self.new_expression(),
            TokenType::Keyword(If) => self.if_expression(),
            TokenType::Keyword(_this) => self.this_expression(),
            TokenType::Operator(op @ (Exclamation | Not | Plus | Minus)) => {
                self.unary_expression(op)
            }
            TokenType::Ident(_) => {
                self.spring(Partial::from(self.identifier()).map(|i| Expression::Identifier(i)))
            }
            TokenType::String(_) => self.spring(Partial::from(self.string_literal())),
            TokenType::Number(_) => self.spring(Partial::from(self.number_literal())),
            TokenType::Bracket(LParens) => self.spring(self.grouped_expression()),
            TokenType::Bracket(LSquare) => self.array_expression(),
            TokenType::Bracket(LCurly) => self.spring(
                self.block(ScopeType::Local)
                    .map(|b| Expression::BlockExpr(b)),
            ),
            _ => Partial::from_error(expected(TokenType::Operator(SemiColon), token.span)),
        };
        expression
    }

    /// Parses a boolean literal.
    fn boolean_literal(&self) -> Fallible<Expression> {
        let token = self.token().unwrap();
        let value = match &mut token._type {
            TokenType::Keyword(ref mut s) => match s {
                False => false,
                True => true,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        let span = token.span;
        let boolean = WhirlBoolean { value, span };
        self.advance(); // Move past boolean.
        Ok(Expression::BooleanLiteral(boolean))
    }

    // /// Parses a new epxression.
    // fn new_expression(&self) -> Imperfect<Expression> {
    //     let start = self.token().unwrap().span.start;
    //     self.advance(); // Move past operator.
    //     self.push_precedence(ExpressionPrecedence::New);
    //     let (operand, errors) = self.expression().to_tuple();
    //     self.precedence_stack.borrow_mut().pop();
    //     if operand.is_none() {
    //         return Partial::from_errors(errors);
    //     }
    //     let value = operand.unwrap();
    //     let end = value.span().end;
    //     let span = Span::from([start, end]);
    //     let un_exp = NewExpr { value, span };
    //     let exp = Partial {
    //         value: Some(Expression::NewExpr(Box::new(un_exp))),
    //         errors,
    //     };
    //     self.spring(exp)
    // }

    /// Parses a string literal.
    fn string_literal(&self) -> Fallible<Expression> {
        let token = self.token().unwrap();
        let value = match &mut token._type {
            TokenType::String(ref mut s) => std::mem::take(s),
            _ => unreachable!(),
        };
        let span = token.span;
        let string = WhirlString { value, span };
        self.advance(); // Move past string.
        Ok(Expression::StringLiteral(string))
    }

    /// Parses a number literal.
    fn number_literal(&self) -> Fallible<Expression> {
        let token = self.token().unwrap();
        let value = match &mut token._type {
            TokenType::Number(ref mut n) => std::mem::take(n),
            _ => unreachable!(),
        };
        let span = token.span;
        let number = WhirlNumber { value, span };
        self.advance(); // Move past num.
        Ok(Expression::NumberLiteral(number))
    }

    /// Parses a function expression.
    fn function_expression(&self, is_async: bool, start: [u32; 2]) -> Imperfect<Expression> {
        expect_or_return!(TokenType::Keyword(Fn), self);
        self.advance(); // Move past fn.
        let generic_params = check!(self.maybe_generic_params());
        let params = if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(LParens))
        {
            Some(check!(self.parameters()))
        } else {
            None
        };
        let return_type = check!(self.maybe_return_type());

        if_ended!(
            errors::expected(TokenType::Bracket(LCurly), self.last_token_span(),),
            self
        );

        let token = self.token().unwrap();
        let (body, errors) = match token._type {
            // Parse block.
            TokenType::Bracket(LCurly) => self
                .block(ScopeType::Functional)
                .map(|b| Expression::BlockExpr(b)),
            _ => self.expression(),
        }
        .to_tuple();
        let body = body.unwrap_or_else(|| {
            Expression::BlockExpr(
                self.create_fake_block(
                    ScopeType::Functional,
                    errors
                        .last()
                        .map(|last| last.span)
                        .unwrap_or(self.last_token_end()),
                ),
            )
        });

        let end = body.span().end;
        let span = Span::from([start, end]);

        let function = FunctionExpr {
            is_async,
            generic_params,
            params,
            return_type,
            body,
            span,
        };
        let exp = Partial::from_tuple((Some(Expression::FnExpr(Box::new(function))), errors));
        self.spring(exp)
    }

    /// Parses an async function expression.
    fn async_function_expression(&self) -> Imperfect<Expression> {
        expect_or_return!(TokenType::Keyword(Async), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past async.
        self.function_expression(true, start)
    }

    /// Parses an if expression.
    fn if_expression(&self) -> Imperfect<Expression> {
        expect_or_return!(TokenType::Keyword(If), self);

        let start = self.token().unwrap().span.start;
        self.advance(); // Move past if.
        let mut errors = vec![];
        let (condition, mut condition_errors) = self.expression().to_tuple();
        if condition.is_none() {
            return Partial::from_errors(condition_errors);
        }
        let condition = condition.unwrap();
        errors.append(&mut condition_errors);
        let (consequent, mut consequent_errors) = self.block(ScopeType::IfConsequent).to_tuple();
        let consequent = consequent.unwrap_or_else(|| {
            self.create_fake_block(
                ScopeType::Local,
                errors
                    .last()
                    .map(|last| last.span)
                    .unwrap_or(Span::at(condition.span().end)),
            )
        });
        errors.append(&mut consequent_errors);
        let mut end = consequent.span.end;

        // Parses an else alternate.
        let alternate = (|| {
            if let Some(token) = self.token() {
                match token._type {
                    TokenType::Keyword(Else) => {
                        let start = self.token().unwrap().span.start;
                        self.advance(); // Move past else.
                        let (expression, mut exp_errors) = if self
                            .token()
                            .is_some_and(|token| token._type == TokenType::Bracket(LCurly))
                        {
                            self.block(ScopeType::IfAlternate)
                                .map(|block| Expression::BlockExpr(block))
                        } else {
                            self.expression()
                        }
                        .to_tuple();
                        errors.append(&mut exp_errors);
                        let expression = expression?;
                        let else_end = expression.span().end;
                        end = else_end;
                        let else_exp = ast::Else {
                            expression,
                            span: Span::from([start, else_end]),
                        };
                        Some(else_exp)
                    }
                    _ => None,
                }
            } else {
                None
            }
        })();

        let span = Span::from([start, end]);

        let if_expr = IfExpression {
            condition,
            consequent,
            alternate,
            span,
        };

        let expr = Partial::from_tuple((Some(Expression::IfExpr(Box::new(if_expr))), errors));

        self.spring(expr)
    }

    /// Parses a this expression.
    fn this_expression(&self) -> Imperfect<Expression> {
        expect_or_return!(TokenType::Keyword(_this), self);
        let span = self.token().unwrap().span;
        self.advance(); // Move past this.
        self.spring(Partial::from_value(Expression::ThisExpr(ThisExpr { span })))
    }

    /// Parses a grouped expression.
    fn grouped_expression(&self) -> Imperfect<Expression> {
        expect_or_return!(TokenType::Bracket(LParens), self);
        self.advance(); // move past (
        self.precedence_stack
            .borrow_mut()
            .push(ExpressionPrecedence::Pseudo);
        let expression = self.expression();
        self.precedence_stack.borrow_mut().pop();
        expect_or_return!(TokenType::Bracket(RParens), self);
        self.advance(); // Move past )
        self.spring(expression)
    }

    /// Parses an array expression.
    fn array_expression(&self) -> Imperfect<Expression> {
        expect_or_return!(TokenType::Bracket(LSquare), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past [

        let mut elements = vec![];
        let mut errors = vec![];

        while self
            .token()
            .is_some_and(|token| token._type != TokenType::Bracket(RSquare))
        {
            let (expression, mut exp_errors) = self.expression().to_tuple();
            errors.append(&mut exp_errors);
            if let Some(element) = expression {
                elements.push(element);
            }
            if self
                .token()
                .is_some_and(|t| t._type == TokenType::Operator(Comma))
            {
                self.advance(); // Move past ,
                continue;
            }
            break;
        }
        expect_or_return!(TokenType::Bracket(RSquare), self);
        let end = self.token().unwrap().span.end;
        self.advance(); // Move past ];
        let span = Span::from([start, end]);

        let array = ArrayExpr { elements, span };
        self.spring(Partial {
            value: Some(Expression::ArrayExpr(array)),
            errors,
        })
    }

    /// Reparses an expression to calculate associativity and precedence.
    fn spring(&self, exp: Imperfect<Expression>) -> Imperfect<Expression> {
        match self.token() {
            Some(token) => match token._type {
                TokenType::Bracket(LParens) => self.call_expression(exp),
                TokenType::Bracket(LSquare) => self.index_expression(exp),
                TokenType::Operator(Dot) => self.access_expression(exp),
                TokenType::Operator(
                    op @ (Asterisk | Divide | Carat | Ampersand | BitOr | Equal | NotEqual
                    | LesserThan | LeftShift | RightShift | GreaterThan | LesserThanOrEqual
                    | GreaterThanOrEqual | Percent | Plus | Minus | Range),
                ) => self.binary_expression(exp, op),
                TokenType::Operator(op @ (And | Or | LogicalAnd | LogicalOr)) => {
                    self.logical_expression(exp, op)
                }
                TokenType::Operator(op @ (Assign | PlusAssign | MinusAssign)) => {
                    self.assignment_expression(exp, op)
                }
                TokenType::Operator(op @ (QuestionMark | Exclamation)) => {
                    self.update_expression(exp, op)
                }
                _ => exp,
            },
            None => exp,
        }
    }

    /// Parses a call expression.
    fn call_expression(&self, caller: Imperfect<Expression>) -> Imperfect<Expression> {
        if self.is_lower_or_equal_precedence(ExpressionPrecedence::Call) || caller.is_none() {
            return caller;
        }
        let (caller, mut errors) = caller.to_tuple();
        let caller = caller.unwrap();
        expect_or_return!(TokenType::Bracket(LParens), self);
        self.advance(); // Move past (
        let start = caller.span().start;
        self.push_precedence(ExpressionPrecedence::Pseudo);

        let mut arguments = vec![];
        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Bracket(RParens))
        {
            let mut partial = self.expression();
            errors.append(&mut partial.errors);
            if let Some(argument) = partial.value {
                arguments.push(argument);
            }
            if self
                .token()
                .is_some_and(|t| t._type == TokenType::Operator(Comma))
            {
                self.advance(); // Move past ,
                continue;
            }
            break;
        }
        expect_or_return!(TokenType::Bracket(RParens), self);
        let end = self.token().unwrap().span.end;
        self.advance(); // Move past )

        self.precedence_stack.borrow_mut().pop();

        let span = Span::from([start, end]);
        let call_expression = CallExpr {
            caller,
            arguments,
            span,
        };
        let exp = Partial {
            value: Some(Expression::CallExpr(Box::new(call_expression))),
            errors,
        };
        self.spring(exp)
    }

    /// Parses an index expression.
    fn index_expression(&self, object: Imperfect<Expression>) -> Imperfect<Expression> {
        if self.is_lower_or_equal_precedence(ExpressionPrecedence::Index) || object.is_none() {
            return object;
        }

        let (object, mut errors) = object.to_tuple();
        let object = object.unwrap();
        expect_or_return!(TokenType::Bracket(LSquare), self);
        self.advance(); // Move past [
        let start = object.span().start;
        self.push_precedence(ExpressionPrecedence::Pseudo);
        let (index, mut index_errors) = self.expression().to_tuple();
        errors.append(&mut index_errors);
        if index.is_none() {
            self.precedence_stack.borrow_mut().pop();
            return Partial {
                value: Some(object),
                errors,
            };
        }
        let index = index.unwrap();
        self.precedence_stack.borrow_mut().pop();
        expect_or_return!(TokenType::Bracket(RSquare), self);
        let end = self.token().unwrap().span.end;
        self.advance(); // Move past ]

        let index_exp = IndexExpr {
            object,
            index,
            span: Span::from([start, end]),
        };
        let indexexpr = Partial {
            value: Some(Expression::IndexExpr(Box::new(index_exp))),
            errors,
        };
        self.spring(indexexpr)
    }

    /// Parses an access expression.
    fn access_expression(&self, object: Imperfect<Expression>) -> Imperfect<Expression> {
        if self.is_lower_or_equal_precedence(ExpressionPrecedence::Access) || object.is_none() {
            return object;
        }
        let (obj, mut errors) = object.to_tuple();
        let object = obj.unwrap();
        let start = object.span().start;
        self.advance(); // Move past .
        let property = match self.identifier() {
            Ok(property) => property,
            Err(error) => {
                errors.push(error);
                return Partial {
                    value: Some(object),
                    errors,
                };
            }
        };
        let property = Expression::Identifier(property);
        let end = property.span().end;
        let span = Span::from([start, end]);
        let access_exp = AccessExpr {
            object,
            property,
            span,
        };
        let exp = Partial {
            value: Some(Expression::AccessExpr(Box::new(access_exp))),
            errors,
        };
        self.spring(exp)
    }

    /// Parses a binary expression.
    fn binary_expression(
        &self,
        left: Imperfect<Expression>,
        op: ast::Operator,
    ) -> Imperfect<Expression> {
        let precedence = op.into();
        if self.is_lower_or_equal_precedence(precedence) || left.is_none() {
            return left;
        }
        let (left, mut errors) = left.to_tuple();
        let left = left.unwrap();
        let start = left.span().start;
        self.advance(); // Move past operator.
        self.push_precedence(precedence);
        let mut partial = self.expression();
        errors.append(&mut partial.errors);
        if partial.is_none() {
            self.precedence_stack.borrow_mut().pop();
            return Partial {
                value: Some(left),
                errors,
            };
        }
        let right = partial.unwrap();
        self.precedence_stack.borrow_mut().pop();
        let end = right.span().end;
        let span = Span::from([start, end]);
        let bin_exp = BinaryExpr {
            left,
            operator: op.into(),
            right,
            span,
        };
        let partial = Partial {
            value: Some(Expression::BinaryExpr(Box::new(bin_exp))),
            errors,
        };
        self.spring(partial)
    }

    /// Parses a logical expression.
    fn logical_expression(
        &self,
        left: Imperfect<Expression>,
        op: ast::Operator,
    ) -> Imperfect<Expression> {
        let precedence = op.into();
        if self.is_lower_or_equal_precedence(precedence) || left.is_none() {
            return left;
        }
        let (left, mut errors) = left.to_tuple();
        let left = left.unwrap();
        let start = left.span().start;
        self.advance(); // Move past operator.
        self.push_precedence(precedence);
        let mut partial = self.expression();
        errors.append(&mut partial.errors);
        if partial.is_none() {
            self.precedence_stack.borrow_mut().pop();
            return Partial {
                value: Some(left),
                errors,
            };
        }
        let right = partial.value.unwrap();
        self.precedence_stack.borrow_mut().pop();
        let end = right.span().end;
        let span = Span::from([start, end]);
        let log_exp = LogicExpr {
            left,
            operator: op.into(),
            right,
            span,
        };
        let partial = Partial {
            value: Some(Expression::LogicExpr(Box::new(log_exp))),
            errors,
        };
        self.spring(partial)
    }

    /// Parses an assignment expression.
    fn assignment_expression(
        &self,
        left: Imperfect<Expression>,
        op: ast::Operator,
    ) -> Imperfect<Expression> {
        let precedence = op.into();
        if self.is_lower_or_equal_precedence(precedence) || left.is_none() {
            return left;
        }
        let (left, mut errors) = left.to_tuple();
        let left = left.unwrap();
        let start = left.span().start;
        self.advance(); // Move past operator.
        self.push_precedence(precedence);
        let mut partial = self.expression();
        errors.append(&mut partial.errors);
        if partial.is_none() {
            self.precedence_stack.borrow_mut().pop();
            return Partial {
                value: Some(left),
                errors,
            };
        }
        let right = partial.value.unwrap();
        self.precedence_stack.borrow_mut().pop();
        let end = right.span().end;
        let span = Span::from([start, end]);
        let ass_exp = AssignmentExpr {
            left,
            operator: op.into(),
            right,
            span,
        };
        let partial = Partial {
            value: Some(Expression::AssignmentExpr(Box::new(ass_exp))),
            errors,
        };
        self.spring(partial)
    }

    /// Parses an update expression.
    fn update_expression(
        &self,
        operand: Imperfect<Expression>,
        operator: ast::Operator,
    ) -> Imperfect<Expression> {
        let precedence = match operator {
            QuestionMark | Exclamation => ExpressionPrecedence::AssertionOrTry,
            _ => unreachable!(),
        };
        if self.is_lower_or_equal_precedence(precedence) || operand.is_none() {
            return operand;
        }
        let (operand, errors) = operand.to_tuple();
        let operand = operand.unwrap();
        expect_or_return!(TokenType::Operator(operator), self);
        let start = operand.span().start;
        self.precedence_stack.borrow_mut().pop();
        let end = self.token().unwrap().span.end;
        self.advance(); // Move past operator.
        let span = Span::from([start, end]);
        let un_exp = UpdateExpr {
            operator: operator.into(),
            operand,
            span,
        };
        let exp = Partial {
            value: Some(Expression::UpdateExpr(Box::new(un_exp))),
            errors,
        };
        self.spring(exp)
    }

    /// Parses a unary expression.
    fn unary_expression(&self, operator: ast::Operator) -> Imperfect<Expression> {
        let precedence = match operator {
            Exclamation | Not => ExpressionPrecedence::Negation,
            Plus | Minus => ExpressionPrecedence::UnaryPlusOrMinus,
            _ => unreachable!("How did you end up parsing {operator:?} as a unary start?"),
        };
        expect_or_return!(TokenType::Operator(operator), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past operator.
        self.push_precedence(precedence);
        let (operand, errors) = self.expression().to_tuple();
        self.precedence_stack.borrow_mut().pop();
        if operand.is_none() {
            return Partial::from_errors(errors);
        }
        let operand = operand.unwrap();
        let end = operand.span().end;
        let span = Span::from([start, end]);
        let un_exp = UnaryExpr {
            operator: operator.into(),
            operand,
            span,
        };
        let exp = Partial {
            value: Some(Expression::UnaryExpr(Box::new(un_exp))),
            errors,
        };
        self.spring(exp)
    }
}

// Statements
impl<L: Lexer> Parser<L> {
    /// Parses a statement.
    fn statement(&self) -> Partial<Statement, ParseError> {
        if_ended!(
            errors::declaration_or_statement_expected(self.last_token_end(),),
            self
        );

        let statement = match self.token().unwrap()._type {
            // function...
            TokenType::Keyword(Function) => self
                .function(false, false)
                .map(|f| Statement::FunctionDeclaration(f)),
            // public...
            TokenType::Keyword(Public) => self.public_declaration(),
            // // async...
            TokenType::Keyword(Async) => self
                .async_function(false)
                .map(|f| Statement::FunctionDeclaration(f)),
            // type...
            TokenType::Keyword(ast::Keyword::Type) => self
                .type_declaration(false)
                .map(|t| Statement::TypeDeclaration(t)),
            // // test...
            TokenType::Keyword(Test) => self
                .test_declaration()
                .map(|t| Statement::TestDeclaration(t)),
            // // use...
            TokenType::Keyword(Use) => self
                .use_declaration(false)
                .map(|u| Statement::UseDeclaration(u)),
            // // enum...
            TokenType::Keyword(Enum) => self
                .enum_declaration(false)
                .map(|e| Statement::EnumDeclaration(e)),
            // // module...
            TokenType::Keyword(Module) => self
                .module_declaration()
                .map(|m| Statement::ModuleDeclaration(m)),
            // // model...
            TokenType::Keyword(Model) => self
                .model_declaration(false)
                .map(|m| Statement::ModelDeclaration(m)),
            // // var...
            TokenType::Keyword(Var) => self
                .variable_declaration(false)
                .map(|v| Statement::VariableDeclaration(v)),
            // // interface...
            TokenType::Keyword(Interface) => self
                .interface_declaration(false)
                .map(|t| Statement::InterfaceDeclaration(t)),
            // // const..
            TokenType::Keyword(Const) => self
                .constant_declaration(false)
                .map(|c| Statement::ConstantDeclaration(c)),
            TokenType::Keyword(While) => self.while_statement(),
            TokenType::Keyword(Return) => self.return_statement(),
            TokenType::Keyword(For) => self.for_statement(),
            TokenType::Keyword(Continue) => self.continue_statement(),
            TokenType::Keyword(Break) => self.break_statement(),
            // unimplemented!(
            //     "{:?} not implemented yet!. The last token was {:?}",
            //     self.token().unwrap(),
            //     self.past.borrow_mut()
            // )
            _ => self.expression_start(),
            // _ => todo!(),
        };

        // If an error is encountered, clear the precedence stack and skip all the next (likely corrupted) tokens until after a delimeter or boundary.
        // Then resume normal parsing.
        if statement.is_none() {
            self.precedence_stack.borrow_mut().clear();
            if *self.is_in_error_state.borrow() {
                self.advance();
                *self.is_in_error_state.borrow_mut() = false;
            } else {
                *self.is_in_error_state.borrow_mut() = true;
            }
            loop {
                match self.token() {
                    Some(token) => match token._type {
                        TokenType::Bracket(_)
                        | TokenType::Operator(
                            SemiColon | GreaterThan | RightShift | LesserThan | LeftShift,
                        )
                        | TokenType::Keyword(Model)
                        | TokenType::Keyword(For)
                        | TokenType::Keyword(Function)
                        | TokenType::Keyword(Interface)
                        | TokenType::Keyword(Public)
                        | TokenType::Keyword(Var) => break,
                        _ => self.advance(),
                    },
                    None => break,
                }
            }
        } else {
            *self.is_in_error_state.borrow_mut() = false;
        }
        statement
    }

    /// Parses a function. It assumes that `function` is the current token, and has already been checked.
    fn function(&self, is_async: bool, is_public: bool) -> Imperfect<FunctionDeclaration> {
        expect_or_return!(TokenType::Keyword(Function), self);
        let start = self.token().unwrap().span.start;

        let info = self.get_doc_comment();
        self.advance(); // Move past function.

        let mut errors = vec![];

        let name = check!(self.identifier());
        let generic_params = match self.maybe_generic_params() {
            Ok(param) => param,
            Err(error) => {
                errors.push(error);
                None
            }
        };
        let params = if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(LParens))
        {
            match self.parameters() {
                Ok(params) => params,
                Err(error) => {
                    errors.push(error);
                    vec![]
                }
            }
        } else {
            vec![]
        };
        let return_type = match self.maybe_return_type() {
            Ok(rettye) => rettye,
            Err(error) => {
                errors.push(error);
                None
            }
        };
        // Errors found in the body of the function.
        let (body, mut body_errors) = self.block(ScopeType::Functional).to_tuple();
        errors.append(&mut body_errors);
        // Having no function body is not fatal, so autocomplete can work.
        let body = body.unwrap_or_else(|| {
            let ambience = self.module_ambience();
            ambience.enter(ScopeType::Functional);
            let block = Block {
                scope_id: ambience.current_scope(),
                statements: vec![],
                span: errors.last().map(|last| last.span).unwrap_or(name.span),
            };
            ambience.leave_scope();
            block
        });

        let signature = FunctionSignature {
            name,
            is_async,
            info,
            params,
            generic_params,
            return_type,
            is_public,
        };

        let entry_no = self
            .module_ambience()
            .register(ScopeEntry::Function(signature));

        let function = FunctionDeclaration {
            address: ScopeAddress {
                module_id: self.module_ambience().id(),
                scope_id: self.module_ambience().current_scope(),
                entry_no,
            },
            span: Span::from([start, body.span.end]),
            body,
        };

        Partial::from_tuple((Some(function), errors))
    }

    /// Parses an async function. Assumes that `async` is the current token (and has already been checked).
    fn async_function(&self, is_public: bool) -> Imperfect<FunctionDeclaration> {
        expect_or_return!(TokenType::Keyword(Async), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past async.

        expect_or_return!(TokenType::Keyword(Function), self);

        let mut partial = self.function(true, is_public);
        if let Some(function) = &mut partial.value {
            function.span.start = start;
        }

        partial
    }

    /// Parses a public declaration. Assumes that `public` is the current token.
    fn public_declaration(&self) -> Imperfect<Statement> {
        expect_or_return!(TokenType::Keyword(Public), self);

        let start = self.token().unwrap().span.start;

        self.advance(); // Move past public.

        if_ended!(errors::declaration_expected(self.last_token_span()), self);

        let token = self.token().unwrap();

        let mut partial = match token._type {
            // Repeated.
            TokenType::Keyword(Public) => {
                Partial::from_error(errors::declaration_expected(token.span))
            }

            TokenType::Keyword(Function) => self
                .function(false, true)
                .map(|f| Statement::FunctionDeclaration(f)),
            TokenType::Keyword(Test) => Partial::from_error(errors::public_test(token.span)),
            TokenType::Keyword(Use) => self
                .use_declaration(true)
                .map(|u| Statement::UseDeclaration(u)),
            TokenType::Keyword(Async) => self
                .async_function(true)
                .map(|f| Statement::FunctionDeclaration(f)),
            TokenType::Keyword(ast::Keyword::Type) => self
                .type_declaration(true)
                .map(|t| Statement::TypeDeclaration(t)),
            TokenType::Keyword(Enum) => self
                .enum_declaration(true)
                .map(|e| Statement::EnumDeclaration(e)),
            TokenType::Keyword(Model) => self
                .model_declaration(true)
                .map(|m| Statement::ModelDeclaration(m)),
            TokenType::Keyword(Var) => self
                .variable_declaration(true)
                .map(|v| Statement::VariableDeclaration(v)),
            TokenType::Keyword(Interface) => self
                .interface_declaration(true)
                .map(|t| Statement::InterfaceDeclaration(t)),
            TokenType::Keyword(Const) => self
                .constant_declaration(true)
                .map(|c| Statement::ConstantDeclaration(c)),
            // Parse public shorthand variable declaration as syntax error.
            TokenType::Ident(_) => {
                let statement = self.statement();
                return if statement.exists_and(|s| s.is_variable_declaration()) {
                    Partial::from_error(errors::public_shorthand_var(
                        statement.value.unwrap().span(),
                    ))
                } else {
                    Partial::from_error(errors::declaration_expected(Span::from([start, start])))
                };
            }

            _ => Partial::from_error(errors::declaration_expected(token.span)),
        };

        if let Some(statement) = &mut partial.value {
            statement.set_start(start);
        }

        if !self.module_ambience().is_in_global_scope() && partial.is_some() {
            partial.errors.push(errors::public_in_non_global_scope(
                partial.value.as_ref().unwrap().span(),
            ))
        }

        partial
    }

    /// Parses a type declaration. Assumes that `type` is the current token.
    fn type_declaration(&self, is_public: bool) -> Imperfect<TypeDeclaration> {
        expect_or_return!(TokenType::Keyword(Type), self);
        let start = self.token().unwrap().span.start;
        let info = self.get_doc_comment();
        self.advance(); // move past type.

        let mut errors = vec![];

        let name = check!(self.identifier());
        let generic_params = match self.maybe_generic_params() {
            Ok(g) => g,
            Err(error) => {
                errors.push(error);
                None
            }
        };
        expect_or_return!(TokenType::Operator(Assign), self);
        self.advance(); // Move past =
        let value = check!(self.type_expression());
        let mut end = value.span().end;
        if self
            .token()
            .is_some_and(|token| token._type == TokenType::Operator(SemiColon))
        {
            end = self.token().unwrap().span.end;
            self.advance(); // Move past ;
        }
        //  else {
        //     errors.push(expected(
        //         TokenType::Operator(SemiColon),
        //         self.last_token_end(),
        //     ))
        // }
        let span = Span::from([start, end]);
        let signature = TypeSignature {
            name,
            info,
            is_public,
            generic_params,
            value,
        };
        let entry_no = self.module_ambience().register(ScopeEntry::Type(signature));
        let type_ = TypeDeclaration {
            address: ScopeAddress {
                module_id: self.module_ambience().id(),
                scope_id: self.module_ambience().current_scope(),
                entry_no,
            },
            span,
        };
        Partial::from_tuple((Some(type_), errors))
    }

    /// Parses a test declaration. Assumes that `test` is the current token.
    fn test_declaration(&self) -> Imperfect<TestDeclaration> {
        expect_or_return!(TokenType::Keyword(Test), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past test.

        if_ended!(errors::string_expected(self.last_token_end()), self);
        let token = self.token().unwrap();

        let mut partial = match token._type {
            TokenType::String(ref mut name) => {
                let name_span = token.span;
                let name = std::mem::take(name);

                self.advance(); // Move past string.

                let (body, errors) = self.block(ScopeType::Test).to_tuple();
                if body.is_none() {
                    return Partial::from_errors(errors);
                }
                let body = body.unwrap();
                let span = Span::from([start, body.span.end]);
                let test_decl = TestDeclaration {
                    name,
                    name_span,
                    body,
                    span,
                };
                Partial::from_tuple((Some(test_decl), errors))
            }
            _ => Partial::from_error(errors::string_expected(token.span)),
        };
        // Flag non global test.
        if partial.value.is_some() && !self.module_ambience().is_in_global_scope() {
            partial.errors.push(errors::test_in_non_global_scope(
                partial.value.as_ref().unwrap().span,
            ));
        }
        partial
    }

    /// Parses a use import. Assumes that `use` is the current token.
    fn use_declaration(&self, is_public: bool) -> Imperfect<UseDeclaration> {
        expect_or_return!(TokenType::Keyword(Use), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past use.
        let name = check!(self.identifier());
        let mut errors = vec![];
        // More tolerant to allow auto-complete.
        let (path, mut path_errors) = self.use_path().to_tuple();
        errors.append(&mut path_errors);
        let path = path.unwrap_or(UsePath::Me);
        let end;
        if self
            .token()
            .is_some_and(|token| token._type == TokenType::Operator(SemiColon))
        {
            end = self.token().unwrap().span.end;
            self.advance(); //
        } else {
            end = self.last_token_end().end;
            // errors.push(expected(
            //     TokenType::Operator(SemiColon),
            //     self.last_token_end(),
            // ));
        }
        let span = Span::from([start, end]);
        let target = UseTarget { name, path };
        // Collect addresses for each nested import.
        let mut addresses = vec![];
        for name in target.leaves() {
            let signature = UseTargetSignature { name, is_public };
            let entry_no = self
                .module_ambience()
                .register(ScopeEntry::UseImport(signature));
            addresses.push(ScopeAddress {
                module_id: self.module_ambience().id(),
                scope_id: self.module_ambience().current_scope(),
                entry_no,
            })
        }
        let use_decl = UseDeclaration {
            addresses,
            target,
            is_public,
            span,
        };

        if !(self.module_ambience().is_in_global_scope()
            || self.module_ambience().is_in_test_scope())
        {
            errors.push(errors::non_global_use(span));
        }
        Partial {
            value: Some(use_decl),
            errors,
        }
    }

    /// Parses a use path.
    fn use_path(&self) -> Imperfect<UsePath> {
        if self.token().is_none() {
            return Partial::from_errors(vec![]);
        }

        let token = self.token().unwrap();
        let mut errors = vec![];
        match token._type {
            TokenType::Operator(Dot) => {
                self.advance(); // Move past .
                if self.token().is_none() {
                    errors.push(errors::identifier_expected(self.last_token_span()));
                    return Partial::from_tuple((Some(UsePath::Me), errors));
                }
                let token = self.token().unwrap();
                match token._type {
                    // Importing a single item.
                    TokenType::Ident(_) => {
                        let path = UsePath::Item(Box::new(UseTarget {
                            name: check!(self.identifier()),
                            path: {
                                let (path, mut sub_errors) = self.use_path().to_tuple();
                                errors.append(&mut sub_errors);
                                path.unwrap_or(UsePath::Me)
                            },
                        }));
                        Partial::from_tuple((Some(path), errors))
                    }
                    // Importing a list of items.
                    TokenType::Bracket(LCurly) => {
                        let start = self.token().unwrap().span.start;
                        self.advance(); // Move past {
                        let (use_path, mut suberrors) = self.use_path_list().to_tuple();
                        let use_path = use_path.unwrap_or(UsePath::List(vec![]));
                        errors.append(&mut suberrors);
                        expect_or_return!(TokenType::Bracket(RCurly), self);
                        let end = self.token().unwrap().span.end;
                        if let UsePath::List(items) = &use_path {
                            if items.len() == 0 {
                                errors.push(errors::empty_path_list(Span::from([start, end])));
                            }
                        }
                        self.advance(); // Move past }
                        Partial::from_tuple((Some(use_path), errors))
                    }
                    _ => return Partial::from_error(errors::identifier_expected(token.span)),
                }
            }
            // Importing self.
            _ => Partial::from_value(UsePath::Me),
        }
    }

    /// Parses a use path list. Assumes that the first target is the current token.
    fn use_path_list(&self) -> Imperfect<UsePath> {
        let mut items = vec![];
        let mut errors = vec![];
        while self
            .token()
            .is_some_and(|token| token._type != TokenType::Bracket(RCurly))
        {
            let name = check!(self.identifier());
            let (path, mut suberrors) = self.use_path().to_tuple();
            let path = path.unwrap_or(UsePath::Me);
            errors.append(&mut suberrors);
            items.push(UseTarget { name, path });
            if self
                .token()
                .is_some_and(|token| token._type == TokenType::Operator(Comma))
            {
                self.advance(); // Move past ,
                continue;
            }
            break;
        }
        Partial::from_value(UsePath::List(items))
    }

    /// Parses an enum declaration. Assumes that `enum` is the current token.
    fn enum_declaration(&self, is_public: bool) -> Imperfect<EnumDeclaration> {
        expect_or_return!(TokenType::Keyword(Enum), self);
        let start = self.token().unwrap().span.start;
        let info = self.get_doc_comment();
        self.advance(); // Move past enum.
        let mut errors = vec![];
        let name = check!(self.identifier());
        let generic_params = match self.maybe_generic_params() {
            Ok(g) => g,
            Err(error) => {
                errors.push(error);
                None
            }
        };
        let (variants, mut variant_errors) = self.enum_variants().to_tuple();
        errors.append(&mut variant_errors);
        if variants.is_none() {
            return Partial::from_errors(errors);
        }
        let (variants, end) = variants.unwrap();
        let signature = EnumSignature {
            name,
            info,
            is_public,
            generic_params,
            variants,
        };
        let entry_no = self.module_ambience().register(ScopeEntry::Enum(signature));
        let address = ScopeAddress {
            module_id: self.module_ambience().id(),
            scope_id: self.module_ambience().current_scope(),
            entry_no,
        };
        let span = Span::from([start, end]);
        let enum_ = EnumDeclaration { address, span };
        Partial::from_tuple((Some(enum_), errors))
    }

    /// Parses an enum variant.
    fn enum_variants(&self) -> Imperfect<(Vec<EnumVariant>, [u32; 2])> {
        expect_or_return!(TokenType::Bracket(LCurly), self);
        self.advance(); // Move past {

        let mut variants = vec![];
        let mut errors = vec![];
        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Bracket(RCurly))
        {
            let mut partial = self.enum_variant();
            errors.append(&mut partial.errors);
            if let Some(variant) = partial.value {
                variants.push(variant);
            }
            if self.token().unwrap()._type == TokenType::Operator(Comma) {
                self.advance();
                continue;
            }
            break;
        }

        expect_or_return!(TokenType::Bracket(RCurly), self);
        let end = self.token().unwrap().span.end;
        self.advance(); // Close }

        Partial::from_tuple((Some((variants, end)), errors))
    }

    /// Parses an enum variant. Assumes that the name is the current token.
    fn enum_variant(&self) -> Imperfect<EnumVariant> {
        let info = self.get_doc_comment();
        let name = check!(self.identifier());
        let start = name.span.start;
        let end;
        let mut has_tagged_type = false;
        let mut tagged_types = vec![];
        // Parsing a tagged type.
        if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(LParens))
        {
            has_tagged_type = true;
            self.advance(); // Move past (
            while !self
                .token()
                .is_some_and(|token| token._type == TokenType::Bracket(RParens))
            {
                tagged_types.push(check!(self.type_expression()));
                if self
                    .token()
                    .is_some_and(|token| token._type == TokenType::Operator(Comma))
                {
                    self.advance(); // Move past ,
                    continue;
                }
                break;
            }
            expect_or_return!(TokenType::Bracket(RParens), self);
            end = self.token().unwrap().span.end;
            self.advance(); // Move past )
        } else {
            end = name.span.end;
        }

        let span = Span::from([start, end]);
        let mut errors = vec![];
        if tagged_types.len() == 0 && has_tagged_type {
            errors.push(errors::empty_enum_tag(span));
        }

        let variant = EnumVariant {
            name,
            info,
            tagged_types,
            span,
        };

        Partial::from_tuple((Some(variant), errors))
    }

    /// Parses a module declaration. Assumes that `module` is the current token.
    fn module_declaration(&self) -> Imperfect<ModuleDeclaration> {
        expect_or_return!(TokenType::Keyword(Module), self);
        let info = self.get_doc_comment();
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past module.
        let name = check!(self.identifier());
        let mut errors = vec![];

        let end = match self.token() {
            Some(token) if token._type == TokenType::Operator(SemiColon) => {
                let span = token.span;
                self.advance(); // Move past ;
                span.end
            }
            _ => {
                // errors.push(expected(
                //     TokenType::Operator(SemiColon),
                //     self.last_token_end(),
                // ));
                name.span.end
            }
        };

        let module = ModuleDeclaration {
            span: Span { start, end },
        };

        // Module already has a name.
        if self.module_ambience().get_module_name().is_some() {
            errors.push(errors::duplicate_module_name(module.span))
        } else
        // Module is not in global scope.
        if !self.module_ambience().is_in_global_scope() {
            errors.push(errors::module_declaration_not_global(module.span))
        } else {
            self.module_ambience().set_module_name(name);
            self.module_ambience().module_info = info;
        }
        Partial {
            value: Some(module),
            errors,
        }
    }

    /// Parses a model declaration. Assumes that `model` is the current token.
    fn model_declaration(&self, is_public: bool) -> Imperfect<ModelDeclaration> {
        expect_or_return!(TokenType::Keyword(Model), self);
        let start = self.token().unwrap().span.start;
        let info = self.get_doc_comment();
        self.advance(); // Move past model.
        let name = check!(self.identifier());
        let mut errors = vec![];
        let generic_params = match self.maybe_generic_params() {
            Ok(params) => params,
            Err(error) => {
                errors.push(error);
                None
            }
        };
        let entry_no = self.module_ambience().reserve_entry_space();
        let address = ScopeAddress {
            module_id: self.module_ambience().id(),
            scope_id: self.module_ambience().current_scope(),
            entry_no,
        };
        let (body, signature) = if self.token().is_none() {
            errors.push(errors::expected(
                TokenType::Bracket(LCurly),
                self.last_token_end(),
            ));
            (
                ModelBody {
                    properties: vec![],
                    constructor: None,
                    span: self.last_token_end(),
                },
                ModelSignature {
                    name,
                    info,
                    is_public,
                    generic_params,
                    parameters: None,
                    implementations: vec![],
                    attributes: vec![],
                    methods: vec![],
                },
            )
        } else {
            let implementations = match self.maybe_interface_implementations() {
                Ok(impls) => impls,
                Err(error) => {
                    errors.push(error);
                    vec![]
                }
            };
            let (body, signature) = if !self
                .token()
                .is_some_and(|token| token._type == TokenType::Bracket(LCurly))
            {
                (
                    ModelBody {
                        properties: vec![],
                        constructor: None,
                        span: self.last_token_end(),
                    },
                    ModelSignature {
                        name,
                        info,
                        is_public,
                        generic_params,
                        parameters: None,
                        implementations,
                        attributes: vec![],
                        methods: vec![],
                    },
                )
            } else {
                let (results, mut body_errors) = self.model_body(&address).to_tuple();
                errors.append(&mut body_errors);
                if results.is_none() {
                    (
                        ModelBody {
                            properties: vec![],
                            constructor: None,
                            span: self.last_token_end(),
                        },
                        ModelSignature {
                            name,
                            info,
                            is_public,
                            generic_params,
                            parameters: None,
                            implementations,
                            attributes: vec![],
                            methods: vec![],
                        },
                    )
                } else {
                    let (body, properties, methods, parameters) = results.unwrap();
                    let signature = ModelSignature {
                        name,
                        info,
                        is_public,
                        generic_params,
                        parameters,
                        implementations,
                        attributes: properties,
                        methods,
                    };
                    (body, signature)
                }
            };
            (body, signature)
        };
        let end = body.span.end;
        self.module_ambience()
            .register_at(entry_no, ScopeEntry::Model(signature));
        let span = Span::from([start, end]);
        let model = ModelDeclaration {
            address,
            body,
            span,
        };
        Partial {
            value: Some(model),
            errors,
        }
    }

    /// Maybe parses a list of interface implementations.
    fn maybe_interface_implementations(&self) -> Fallible<Vec<TypeExpression>> {
        let mut interfaces = vec![];
        self.ended(expected(TokenType::Bracket(LCurly), self.last_token_end()))?;
        let token = self.token().unwrap();
        // Parse interface impls if they exist.
        if let TokenType::Keyword(Implements) = token._type {
            self.advance(); // Move past implements.
            loop {
                let interface_ = self.type_expression()?;
                match interface_ {
                    TypeExpression::Union(_)
                    | TypeExpression::Functional(_)
                    | TypeExpression::This { .. }
                    | TypeExpression::Invalid => {
                        return Err(errors::type_in_interface_position(interface_))
                    }
                    _ => {}
                }
                interfaces.push(interface_);
                if self
                    .token()
                    .is_some_and(|t| t._type == TokenType::Operator(Plus))
                {
                    self.advance(); // Move past +
                    continue;
                }
                break;
            }
        }
        Ok(interfaces)
    }

    /// Rambly function to parse a model body.
    fn model_body(
        &self,
        model_address: &ScopeAddress,
    ) -> Imperfect<(
        ModelBody,
        Vec<AttributeSignature>,
        Vec<MethodSignature>,
        Option<Vec<Parameter>>,
    )> {
        expect_or_return!(TokenType::Bracket(LCurly), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past {

        let mut body_errors = vec![];

        let mut attribute_signatures = vec![];
        let mut method_signatures = vec![];
        let mut parameters = vec![];
        let mut has_constructor = false;
        let mut constructor = None;
        let mut properties = vec![];
        let mut attribute_properties = vec![];

        // Helper closure to quickly generate methods.
        let mut generate_method = |start, is_public, is_static, is_async| {
            expect_or_return!(TokenType::Keyword(Function), self);
            let partial = self.method(is_public, is_static, is_async, model_address);
            if partial.is_none() {
                return partial.map(|_| ());
            };
            let (tuple, errors) = partial.to_tuple();
            let (signature, _type) = tuple.unwrap();
            let method = ModelProperty {
                index: method_signatures.len(),
                _type,
                span: Span::from([start, self.last_token_span().end]),
            };
            method_signatures.push(signature);
            properties.push(method);
            Partial {
                value: Some(()),
                errors,
            }
        };

        // Helper closure to quickly generate attributes.
        let mut generate_attrib = |is_public, start| {
            let partial = self.attribute(is_public);
            let (tuple, errors) = partial.to_tuple();
            if tuple.is_none() {
                return Partial::from_errors(errors);
            }
            let (signature, _type) = tuple.unwrap();
            // Build model property enum from type.
            let attribute = ModelProperty {
                index: attribute_signatures.len(),
                _type,
                span: Span::from([start, self.last_token_span().end]),
            };
            attribute_signatures.push(signature);
            attribute_properties.push(attribute);
            Partial {
                value: Some(()),
                errors,
            }
        };

        let maybe_async = || {
            self.token().is_some_and(|t| {
                if t._type == TokenType::Keyword(Async) {
                    self.advance(); // Move past async.
                    true
                } else {
                    false
                }
            })
        };

        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Bracket(RCurly))
        {
            let token = self.token().unwrap();
            let start = token.span.start;
            match token._type {
                // parse new constructor.
                TokenType::Keyword(New) => {
                    self.advance(); // move past new.
                    let mut partial = Partial::from(self.parameters());
                    if has_constructor {
                        body_errors.push(errors::duplicate_constructor(token.span));
                    }
                    if partial.is_none() {
                        body_errors.append(&mut partial.errors);
                        self.advance();
                        continue;
                    }
                    if !has_constructor {
                        parameters = partial.value.unwrap();
                    }
                    let mut bloc_partial = self.block(ScopeType::ModelConstructorOf {
                        model: *model_address,
                    });
                    body_errors.append(&mut bloc_partial.errors);
                    if bloc_partial.is_none() {
                        self.advance();
                        continue;
                    }
                    constructor = Some(bloc_partial.value.unwrap());
                    has_constructor = true
                }
                // parse public property.
                TokenType::Keyword(Public) => {
                    let start = token.span.start;
                    self.advance(); // Move past public.
                    if self.token().is_none() {
                        body_errors.push(errors::expected_attribute(self.last_token_span()));
                        self.advance();
                        continue;
                    }
                    let token = self.token().unwrap();
                    match token._type {
                        TokenType::Keyword(New) => {
                            body_errors.push(errors::public_on_new(self.last_token_span()));
                            self.advance();
                            continue;
                        }
                        // parse public static method.
                        TokenType::Keyword(Static) => {
                            self.advance(); // Move past static.
                            let is_public = true;
                            let is_static = true;
                            let is_async = maybe_async();
                            body_errors.append(
                                &mut generate_method(start, is_public, is_static, is_async).errors,
                            );
                        }
                        TokenType::Keyword(Async) => {
                            self.advance(); // Move past async.
                            let is_public = true;
                            let is_static = false;
                            let is_async = true;
                            body_errors.append(
                                &mut generate_method(start, is_public, is_static, is_async).errors,
                            );
                        }
                        // parse public non-async method.
                        TokenType::Keyword(Function) => {
                            let is_public = true;
                            let is_static = false;
                            let is_async = false;
                            body_errors.append(
                                &mut generate_method(start, is_public, is_static, is_async).errors,
                            );
                        }
                        // parse a public attribute.
                        TokenType::Keyword(Var) => {
                            body_errors.append(&mut generate_attrib(true, start).errors);
                        }
                        _ => {
                            body_errors.push(errors::expected_attribute(token.span));
                            self.advance();
                        }
                    }
                }
                // parse static private method.
                TokenType::Keyword(Static) => {
                    let start = token.span.start;
                    self.advance(); // Move past static.
                    let is_public = false;
                    let is_static = true;
                    let is_async = maybe_async();
                    body_errors
                        .append(&mut generate_method(start, is_public, is_static, is_async).errors);
                }
                // parse private method.
                TokenType::Keyword(Function) => {
                    let is_public = false;
                    let is_async = false;
                    let is_static = false;
                    body_errors
                        .append(&mut generate_method(start, is_public, is_static, is_async).errors);
                }
                // parse a private attribute.
                TokenType::Keyword(Var) => {
                    body_errors.append(&mut generate_attrib(false, start).errors);
                }
                _ => {
                    body_errors.push(errors::expected_attribute(token.span));

                    self.advance();
                }
            }
        }
        // Persist body if it is unclosed.
        let end = if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(RCurly))
        {
            let end = self.token().unwrap().span.end;
            self.advance(); // Move past }
            end
        } else {
            properties
                .last()
                .map(|property| property.span.end)
                .unwrap_or(start)
        };
        let span = Span::from([start, end]);
        properties.append(&mut attribute_properties);
        properties.sort_by(|a, b| a.span.start.partial_cmp(&b.span.start).unwrap());
        let body = ModelBody {
            properties,
            constructor,
            span,
        };
        Partial {
            value: Some((
                body,
                attribute_signatures,
                method_signatures,
                if has_constructor {
                    Some(parameters)
                } else {
                    None
                },
            )),
            errors: body_errors,
        }
    }

    /// Parses an attribute in a model.
    fn attribute(&self, is_public: bool) -> Imperfect<(AttributeSignature, ModelPropertyType)> {
        expect_or_return!(TokenType::Keyword(Var), self);
        self.advance(); // Move past var.
        let info = self.get_doc_comment();
        let name = check!(self.identifier());
        let var_type = check!(self.type_label());
        if self
            .token()
            .is_some_and(|token| token._type == TokenType::Operator(SemiColon))
        {
            self.advance(); // Move past ;.
        }

        let signature = AttributeSignature {
            name,
            info,
            is_public,
            var_type,
        };

        let property_type = ModelPropertyType::Attribute;
        Partial {
            value: Some((signature, property_type)),
            errors: vec![],
        }
    }

    /// Parses a method in a model.
    fn method(
        &self,
        is_public: bool,
        is_static: bool,
        is_async: bool,
        model_address: &ScopeAddress,
    ) -> Imperfect<(MethodSignature, ModelPropertyType)> {
        expect_or_return!(TokenType::Keyword(Function), self);
        self.advance(); // Move past function.
        let info = self.get_doc_comment();
        if_ended!(errors::identifier_expected(self.last_token_end()), self);
        let token = self.token().unwrap();
        // If [ is the next token, parse a interface impl.
        if token._type == TokenType::Bracket(LSquare) {
            return self.interface_impl_method(is_public, is_static, is_async, info, model_address);
        }
        // else parse normal function.
        let name = check!(self.identifier());
        let generic_params = check!(self.maybe_generic_params());
        let constraint = check!(self.maybe_type_constraint());
        let params = if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(LParens))
        {
            check!(self.parameters())
        } else {
            vec![]
        };
        let return_type = check!(self.maybe_return_type());
        let (body, errors) = self
            .block(ScopeType::ModelMethodOf {
                model: *model_address,
            })
            .to_tuple();
        if body.is_none() {
            return Partial::from_errors(errors);
        }
        let body = body.unwrap();
        let signature = MethodSignature {
            name,
            info,
            is_static,
            constraint,
            is_async,
            is_public,
            generic_params,
            params,
            return_type,
        };
        let _type = ModelPropertyType::Method { body };
        Partial {
            value: Some((signature, _type)),
            errors,
        }
    }

    /// Parses a interface implementation.
    fn interface_impl_method(
        &self,
        is_public: bool,
        is_static: bool,
        is_async: bool,
        info: Option<Vec<String>>,
        model_address: &ScopeAddress,
    ) -> Imperfect<(MethodSignature, ModelPropertyType)> {
        expect_or_return!(TokenType::Bracket(LSquare), self);
        self.advance(); // Move past [
        let mut interface_target = vec![];
        let mut errors = vec![];
        while self
            .token()
            .is_some_and(|token| token._type != TokenType::Bracket(RSquare))
        {
            let target = check!(self.type_expression());
            match target {
                TypeExpression::Member(membertype) => match unroll_to_discrete_types(membertype) {
                    Ok(mut types) => interface_target.append(&mut types),
                    Err(error) => errors.push(error),
                },
                TypeExpression::Discrete(discretetype) => {
                    interface_target.push(discretetype);
                }
                _ => errors.push(errors::type_in_interface_position(target)),
            }
            if self
                .token()
                .is_some_and(|t| t._type == TokenType::Operator(Dot))
            {
                self.advance(); // move past .
                continue;
            }
            break;
        }
        expect_or_return!(TokenType::Bracket(RSquare), self);
        if interface_target.len() == 0 {
            errors.push(errors::identifier_expected(self.token().unwrap().span));
            return Partial::from_errors(errors);
        }
        // println!("{:?}\n\n\n", interface_target);
        self.advance(); // Move past ]
        let generic_params = check!(self.maybe_generic_params());
        let constraint = check!(self.maybe_type_constraint());
        let params = if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(LParens))
        {
            check!(self.parameters())
        } else {
            vec![]
        };
        let return_type = check!(self.maybe_return_type());
        let (body, mut body_errors) = self
            .block(ScopeType::ModelMethodOf {
                model: *model_address,
            })
            .to_tuple();
        errors.append(&mut body_errors);
        if body.is_none() {
            return Partial::from_errors(errors);
        }
        let body = body.unwrap();
        let tuple = (
            MethodSignature {
                name: interface_target.last().unwrap().name.clone(),
                info,
                is_static,
                is_async,
                is_public,
                generic_params,
                constraint,
                params,
                return_type,
            },
            ModelPropertyType::InterfaceImpl {
                interface_target,
                body,
            },
        );
        Partial {
            value: Some(tuple),
            errors,
        }
    }

    /// Parses a variable declaration.
    fn variable_declaration(&self, is_public: bool) -> Imperfect<VariableDeclaration> {
        expect_or_return!(TokenType::Keyword(Var), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past var.

        let mut errors = vec![];

        if_ended!(errors::identifier_expected(self.last_token_end()), self);
        let token = self.token().unwrap();
        let names = match token._type {
            // Parse destructured pattern.
            // Model destructure.
            TokenType::Bracket(LCurly) => check!(self.object_pattern()),
            // Array destructure.
            TokenType::Bracket(LSquare) => check!(self.array_pattern()),
            // Normal.
            _ => {
                vec![(
                    VariablePattern::Identifier(check!(self.identifier())),
                    self.get_doc_comment(),
                )]
            }
        };

        let var_type = check!(self.maybe_type_label());
        let end;
        let mut value = None;

        // Parse value, if it exists.
        match self.token() {
            // value is assigned.
            Some(Token {
                _type: TokenType::Operator(Assign),
                ..
            }) => {
                self.advance(); // move past =
                let (expression_opt, mut expr_errors) = self.expression().to_tuple();
                errors.append(&mut expr_errors);
                value = expression_opt;
                // expect semicolon.
                match self.token() {
                    Some(Token {
                        _type: TokenType::Operator(SemiColon),
                        ..
                    }) => {
                        end = token.span.end;
                        self.advance(); // move past ;
                    }
                    _ => {
                        // errors.push(expected(
                        //     TokenType::Operator(SemiColon),
                        //     self.last_token_end(),
                        // ));
                        end = token.span.end;
                    }
                }
            }
            // values are not assigned.
            Some(Token {
                _type: TokenType::Operator(SemiColon),
                span,
            }) => {
                end = span.end;
                self.advance(); // Move past ;
            }
            // Some other variation.
            _ => {
                // errors.push(expected(
                //     TokenType::Operator(SemiColon),
                //     self.last_token_end(),
                // ));
                end = self.last_token_span().end;
            }
        }

        let mut addresses = vec![];

        for (name, info) in names {
            let signature = VariableSignature {
                name,
                info,
                is_public,
                var_type: var_type.to_owned(), // todo. find way to reference all destructured variables without cloning.
            };
            let entry_no = self
                .module_ambience()
                .register(ScopeEntry::Variable(signature));
            addresses.push(ScopeAddress {
                module_id: self.module_ambience().id(),
                scope_id: self.module_ambience().current_scope(),
                entry_no,
            });
        }

        let declaration = VariableDeclaration {
            addresses,
            value,
            span: Span { start, end },
        };

        Partial {
            value: Some(declaration),
            errors,
        }
    }

    /// Parses an object pattern. Assumes that `{` is the current token.
    fn object_pattern(&self) -> Fallible<Vec<(VariablePattern, Option<Vec<String>>)>> {
        expect!(TokenType::Bracket(LCurly), self);
        self.advance(); // Move past {
        let mut pattern_items = vec![];
        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Bracket(RCurly))
        {
            let info = self.get_doc_comment();
            let pattern_item = self.object_pattern_item()?;
            pattern_items.push((pattern_item, info));
            if self
                .token()
                .is_some_and(|t| t._type == TokenType::Operator(Comma))
            {
                self.advance(); // Move past ,
                continue;
            }
            break;
        }
        expect!(TokenType::Bracket(RCurly), self);
        self.advance(); // Move past }
        Ok(pattern_items)
    }

    /// Parses an object pattern item. Assumes that the real name is the first token.
    fn object_pattern_item(&self) -> Fallible<VariablePattern> {
        let real_name = self.identifier()?;
        let start = real_name.span.start;
        let alias = match self.token() {
            Some(Token {
                _type: TokenType::Keyword(As),
                ..
            }) => {
                self.advance(); // Move past as
                Some(self.identifier()?)
            }
            None => return Err(expected(TokenType::Bracket(RCurly), self.last_token_end())),
            _ => None,
        };
        let end = match alias {
            Some(ref alias) => alias.span.end,
            None => real_name.span.end,
        };
        let span = Span { start, end };
        Ok(VariablePattern::ObjectPattern {
            real_name,
            alias,
            span,
        })
    }

    /// Parses an array pattern. Assumes that `[` is the current token.
    fn array_pattern(&self) -> Fallible<Vec<(VariablePattern, Option<Vec<String>>)>> {
        expect!(TokenType::Bracket(LSquare), self);
        self.advance(); // Move past [
        let mut pattern_items = vec![];
        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Bracket(RSquare))
        {
            let info = self.get_doc_comment();
            let p_item = VariablePattern::ArrayPattern(self.identifier()?);
            pattern_items.push((p_item, info));
            if self
                .token()
                .is_some_and(|t| t._type == TokenType::Operator(Comma))
            {
                self.advance(); // Move past ,
                continue;
            }
            break;
        }
        expect!(TokenType::Bracket(RSquare), self);
        self.advance(); // Move past ]
        Ok(pattern_items)
    }

    /// Parses a interface declaration. Assumes that `interface` is the current token.
    fn interface_declaration(&self, is_public: bool) -> Imperfect<InterfaceDeclaration> {
        expect_or_return!(TokenType::Keyword(Interface), self);
        let start = self.token().unwrap().span.start;
        let info = self.get_doc_comment();
        self.advance(); // Move past interface.
        let name = check!(self.identifier());
        let generic_params = check!(self.maybe_generic_params());
        let mut errors = vec![];
        if_ended!(
            errors::expected(TokenType::Bracket(LCurly), self.last_token_end(),),
            self
        );
        let entry_no = self.module_ambience().reserve_entry_space();
        let address = ScopeAddress {
            module_id: self.module_ambience().id(),
            scope_id: self.module_ambience().current_scope(),
            entry_no,
        };
        let implementations = match self.maybe_interface_implementations() {
            Ok(impls) => impls,
            Err(error) => {
                errors.push(error);
                vec![]
            }
        };
        let (results, mut body_errors) = self.interface_body(&address).to_tuple();
        errors.append(&mut body_errors);
        if results.is_none() {
            self.module_ambience().register_at(
                entry_no,
                ScopeEntry::Interface(InterfaceSignature {
                    name,
                    info,
                    is_public,
                    generic_params,
                    methods: vec![],
                    implementations,
                }),
            );
            return Partial::from_errors(errors);
        }
        let (body, methods) = results.unwrap();
        let signature = InterfaceSignature {
            name,
            info,
            is_public,
            generic_params,
            implementations,
            methods,
        };
        let end = body.span.end;
        self.module_ambience()
            .register_at(entry_no, ScopeEntry::Interface(signature));
        let span = Span::from([start, end]);
        let interface = InterfaceDeclaration {
            address,
            body,
            span,
        };
        Partial {
            value: Some(interface),
            errors,
        }
    }

    /// Parses a interface body.
    fn interface_body(
        &self,
        interface_address: &ScopeAddress,
    ) -> Imperfect<(InterfaceBody, Vec<MethodSignature>)> {
        expect_or_return!(TokenType::Bracket(LCurly), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past {

        let mut body_errors = vec![];
        let mut properties = vec![];

        let mut method_signatures = vec![];

        // Helper closure to quickly generate methods.
        let mut generate_method = |start, is_public, is_static, is_async| {
            expect_or_return!(TokenType::Keyword(Function), self);
            let partial = self.interface_method(is_public, is_static, is_async, interface_address);
            if partial.is_none() {
                return partial.map(|_| ());
            };
            let (tuple, errors) = partial.to_tuple();
            let (signature, _type) = tuple.unwrap();
            let method = InterfaceProperty {
                index: method_signatures.len(),
                _type,
                span: Span::from([start, self.last_token_span().end]),
            };
            method_signatures.push(signature);
            properties.push(method);
            Partial {
                value: Some(()),
                errors,
            }
        };

        let maybe_async = || {
            self.token().is_some_and(|t| {
                if t._type == TokenType::Keyword(Async) {
                    self.advance(); // Move past async.
                    true
                } else {
                    false
                }
            })
        };

        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Bracket(RCurly))
        {
            let token = self.token().unwrap();
            let start = token.span.start;
            match token._type {
                // parse public property.
                TokenType::Keyword(Public) => {
                    let start = token.span.start;
                    self.advance(); // Move past public.
                    if self.token().is_none() {
                        body_errors.push(errors::expected_attribute(self.last_token_span()));
                        self.advance();
                        continue;
                    }
                    let token = self.token().unwrap();
                    match token._type {
                        // parse public static method.
                        TokenType::Keyword(Static) => {
                            self.advance(); // Move past static.
                            let is_public = true;
                            let is_static = true;
                            let is_async = maybe_async();
                            body_errors.append(
                                &mut generate_method(start, is_public, is_static, is_async).errors,
                            );
                        }
                        TokenType::Keyword(Async) => {
                            self.advance(); // Move past async.
                            let is_public = true;
                            let is_static = false;
                            let is_async = true;
                            body_errors.append(
                                &mut generate_method(start, is_public, is_static, is_async).errors,
                            );
                        }
                        // parse public non-async method.
                        TokenType::Keyword(Function) => {
                            let is_public = true;
                            let is_static = false;
                            let is_async = false;
                            body_errors.append(
                                &mut generate_method(start, is_public, is_static, is_async).errors,
                            );
                        }
                        _ => {
                            body_errors.push(expected(TokenType::Keyword(Function), token.span));
                            self.advance();
                        }
                    }
                }
                // parse static private method.
                TokenType::Keyword(Static) => {
                    let start = token.span.start;
                    self.advance(); // Move past static.
                    let is_public = false;
                    let is_static = true;
                    let is_async = maybe_async();
                    body_errors
                        .append(&mut generate_method(start, is_public, is_static, is_async).errors);
                }
                // parse private method.
                TokenType::Keyword(Function) => {
                    let is_public = false;
                    let is_async = false;
                    let is_static = false;
                    body_errors
                        .append(&mut generate_method(start, is_public, is_static, is_async).errors);
                }
                _ => {
                    body_errors.push(expected(TokenType::Keyword(Function), token.span));
                    self.advance();
                }
            }
        }
        // Persist body if it is unclosed.
        let end = if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(RCurly))
        {
            let end = self.token().unwrap().span.end;
            self.advance(); // Move past }
            end
        } else {
            properties
                .last()
                .map(|property| property.span.end)
                .unwrap_or(start)
        };
        let span = Span::from([start, end]);
        let body = InterfaceBody { properties, span };
        Partial {
            value: Some((body, method_signatures)),
            errors: body_errors,
        }
    }

    /// Parses a interface method.
    fn interface_method(
        &self,
        is_public: bool,
        is_static: bool,
        is_async: bool,
        interface_address: &ScopeAddress,
    ) -> Imperfect<(MethodSignature, InterfacePropertyType)> {
        expect_or_return!(TokenType::Keyword(Function), self);
        self.advance(); // Move past function.
        let info = self.get_doc_comment();
        let name = check!(self.identifier());
        let generic_params = check!(self.maybe_generic_params());
        let constraint = check!(self.maybe_type_constraint());
        let params = if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(LParens))
        {
            check!(self.parameters())
        } else {
            vec![]
        };
        let return_type = check!(self.maybe_return_type());
        if_ended!(
            errors::expected(TokenType::Operator(SemiColon), self.last_token_end()),
            self
        );
        let token = self.token().unwrap();
        let signature = MethodSignature {
            name,
            info,
            is_static,
            is_async,
            is_public,
            generic_params,
            constraint,
            params,
            return_type,
        };
        // If there is no body, it is a function signature.
        if token._type != TokenType::Bracket(LCurly) {
            let _type = InterfacePropertyType::Signature;
            if token._type == TokenType::Operator(SemiColon) {
                self.advance(); // Move past ;
            }
            return Partial::from_value((signature, _type));
        }
        // Else it is a method.
        let (body, errors) = self
            .block(ScopeType::InterfaceMethodOf {
                _interface: *interface_address,
            })
            .to_tuple();
        if body.is_none() {
            return Partial::from_errors(errors);
        }
        let body = body.unwrap();
        let _type = InterfacePropertyType::Method { body };
        Partial {
            value: Some((signature, _type)),
            errors,
        }
    }

    /// Parses a while statement.
    fn while_statement(&self) -> Imperfect<Statement> {
        expect_or_return!(TokenType::Keyword(While), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past while.
        let mut errors = vec![];
        let mut condition = self.expression();
        errors.append(&mut condition.errors);
        if condition.is_none() {
            return Partial::from_errors(errors);
        }
        let condition = condition.value.unwrap();
        let mut body = self.block(ScopeType::WhileLoop);
        errors.append(&mut body.errors);

        let body = body
            .value
            .unwrap_or_else(|| self.create_fake_block(ScopeType::WhileLoop, self.last_token_end()));
        let span = Span::from([start, body.span.end]);
        let while_statement = Statement::WhileStatement(WhileStatement {
            condition,
            body,
            span,
        });
        Partial {
            value: Some(while_statement),
            errors,
        }
    }

    /// Parses a return statement.
    fn return_statement(&self) -> Imperfect<Statement> {
        expect_or_return!(TokenType::Keyword(Return), self);
        let Span {
            start,
            end: ret_end,
        } = self.token().unwrap().span;
        self.advance(); // move past return
        let mut errors = vec![];

        let mut partial = if self.token().is_some_and(|t| {
            t._type == TokenType::Operator(SemiColon) || t._type == TokenType::Bracket(RCurly)
        }) {
            let end = if self.token().unwrap()._type == TokenType::Operator(SemiColon) {
                let end = self.token().unwrap().span.end;
                self.advance(); // move past ;
                end
            } else {
                ret_end
            };
            Partial::from_value(Statement::ReturnStatement(ReturnStatement {
                value: None,
                span: Span { start, end },
            }))
        } else {
            let mut expression = self.expression();
            errors.append(&mut expression.errors);

            let end = if !self
                .token()
                .is_some_and(|t| t._type == TokenType::Operator(SemiColon))
            {
                // errors.push(expected(
                //     TokenType::Operator(SemiColon),
                //     self.last_token_span(),
                // ));
                expression
                    .value
                    .as_ref()
                    .map(|v| v.span().end)
                    .unwrap_or(ret_end)
            } else {
                let end = self.token().unwrap().span.end;
                self.advance(); // move past
                end
            };

            let return_ = ReturnStatement {
                value: expression.value,
                span: Span { start, end },
            };

            let statement = Statement::ReturnStatement(return_);
            Partial::from_tuple((Some(statement), errors))
        };

        if !self.module_ambience().is_in_function_context()
            && partial.is_some()
            && !self.debug_allow_global_expressions
        {
            partial.errors.push(errors::invalid_return(
                partial.value.as_ref().unwrap().span(),
            ));
        }

        partial
    }

    /// Parses a for statement,
    fn for_statement(&self) -> Imperfect<Statement> {
        expect_or_return!(TokenType::Keyword(For), self);
        let start = self.token().unwrap().span.start;
        let mut errors = vec![];

        self.advance(); // Move past for.
        if_ended!(errors::identifier_expected(self.last_token_end()), self);
        let token = self.token().unwrap();
        let names = match token._type {
            // Parse destructured pattern.
            // Model destructure.
            TokenType::Bracket(LCurly) => check!(self.object_pattern()),
            // Array destructure.
            TokenType::Bracket(LSquare) => check!(self.array_pattern()),
            // Normal.
            _ => {
                vec![(
                    VariablePattern::Identifier(check!(self.identifier())),
                    self.get_doc_comment(),
                )]
            }
        };
        expect_or_return!(TokenType::Keyword(In), self);
        self.advance(); // move past in
        let (iterator_opt, mut iterator_errors) = self.expression().to_tuple();
        errors.append(&mut iterator_errors);
        let iterator = match iterator_opt {
            Some(iterator) => iterator,
            None => return Partial::from_errors(errors),
        };
        if_ended!(
            expected(TokenType::Bracket(LCurly), self.last_token_end()),
            self
        );
        let token = self.token().unwrap();
        // parse for label.
        let label_name_opt = if token._type == TokenType::Keyword(As) {
            self.advance(); // Move past as
            match self.identifier() {
                Ok(ident) => Some(ident),
                Err(error) => {
                    errors.push(error);
                    None
                }
            }
        } else {
            None
        };

        let (body_opt, mut body_errors) = self.block(ScopeType::ForLoop).to_tuple();
        errors.append(&mut body_errors);

        let body = match body_opt {
            Some(block) => block,
            None => self.create_fake_block(ScopeType::ForLoop, self.last_token_end()),
        };

        let end = body.span.end;
        let span = Span { start, end };

        // register label and item(s) inside block.
        let ambience = self.module_ambience();
        ambience.jump_to_scope(body.scope_id);
        let items = names
            .into_iter()
            .map(|(name, _)| ScopeAddress {
                module_id: ambience.id(),
                scope_id: ambience.current_scope(),
                entry_no: ambience.register(ScopeEntry::LoopVariable(LoopVariable { name })),
            })
            .collect::<Vec<_>>();
        let label = label_name_opt.map(|label_name| ScopeAddress {
            module_id: ambience.id(),
            scope_id: ambience.current_scope(),
            entry_no: ambience.register(ScopeEntry::LoopLabel(LoopLabel(label_name))),
        });
        ambience.leave_scope();

        let statement = Statement::ForStatement(ForStatement {
            items,
            iterator,
            label,
            body,
            span,
        });
        Partial::from_tuple((Some(statement), errors))
    }

    /// Parses a continue statement.
    fn continue_statement(&self) -> Imperfect<Statement> {
        expect_or_return!(TokenType::Keyword(Continue), self);
        let start = self.token().unwrap().span.start;
        let mut end = self.token().unwrap().span.end;
        self.advance(); // move past continue.
        let mut errors = vec![];

        let label = if self
            .token()
            .is_some_and(|token| matches!(token._type, TokenType::Ident(_)))
        {
            match self.identifier() {
                Ok(ident) => Some(ident),
                Err(error) => {
                    errors.push(error);
                    None
                }
            }
        } else {
            None
        };

        match self.token() {
            Some(Token {
                _type: TokenType::Operator(o),
                span,
            }) if *o == SemiColon => {
                end = span.end;
                self.advance(); // move past ;
            }
            _ => {
                // errors.push(expected(
                //     TokenType::Operator(SemiColon),
                //     self.last_token_end(),
                // ));
            }
        };
        let span = Span { start, end };
        if !self.module_ambience().is_in_loop_context() && !self.debug_allow_global_expressions {
            errors.push(errors::continue_outside_loop(span));
        }
        let continuestatement = Statement::ContinueStatement(ContinueStatement { label, span });

        Partial::from_tuple((Some(continuestatement), errors))
    }

    /// Parses a break statement.
    fn break_statement(&self) -> Imperfect<Statement> {
        expect_or_return!(TokenType::Keyword(Break), self);
        let start = self.token().unwrap().span.start;
        let mut end = self.token().unwrap().span.end;
        self.advance(); // move past break.
        let mut errors = vec![];

        let label = if self
            .token()
            .is_some_and(|token| matches!(token._type, TokenType::Ident(_)))
        {
            match self.identifier() {
                Ok(ident) => Some(ident),
                Err(error) => {
                    errors.push(error);
                    None
                }
            }
        } else {
            None
        };

        match self.token() {
            Some(Token {
                _type: TokenType::Operator(o),
                span,
            }) if *o == SemiColon => {
                end = span.end;
                self.advance(); // move past ;
            }
            _ => {
                // errors.push(expected(
                //     TokenType::Operator(SemiColon),
                //     self.last_token_end(),
                // ));
            }
        };
        let span = Span { start, end };
        if !self.module_ambience().is_in_loop_context() && !self.debug_allow_global_expressions {
            errors.push(errors::break_outside_loop(span));
        }
        let breakstatement = Statement::BreakStatement(BreakStatement { label, span });

        Partial::from_tuple((Some(breakstatement), errors))
    }

    /// Parses an identifier and advances. It assumes that the identifier is the current token.
    fn identifier(&self) -> Fallible<Identifier> {
        self.ended(errors::identifier_expected(self.last_token_end()))?;

        let token = self.token().unwrap();

        if let TokenType::Ident(ref mut name) = token._type {
            let identifier = Identifier {
                name: std::mem::take(name),
                span: token.span,
            };
            self.advance();
            Ok(identifier)
        } else {
            Err(errors::identifier_expected(self.last_token_end()))
        }
    }

    /// Parses generic parameters. Assumes that `<` is maybe the current token.
    fn maybe_generic_params(&self) -> Fallible<Option<Vec<GenericParameter>>> {
        if !self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(LesserThan))
        {
            return Ok(None);
        }
        self.advance(); // Move past <
        let mut generic_params = vec![];

        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Operator(GreaterThan))
        {
            let parameter = self.generic_param()?;
            generic_params.push(parameter);
            if self
                .token()
                .is_some_and(|t| t._type == TokenType::Operator(Comma))
            {
                self.advance(); // Move past ,
                continue;
            }
            break;
        }
        if self
            .token()
            .is_some_and(|token| token._type == TokenType::Operator(RightShift))
            && !generic_params.is_empty()
        {
            self.advance(); // Move past >>
        } else {
            expect!(TokenType::Operator(GreaterThan), self);
            self.advance(); // Move past >
        }
        Ok(Some(generic_params))
    }

    /// Parses a generic parameter.
    fn generic_param(&self) -> Fallible<GenericParameter> {
        let name = self.identifier()?;
        let start = name.span.start;
        let mut interfaces = vec![];
        // Parse assigned interfaces.
        if self
            .token()
            .is_some_and(|t| t._type == TokenType::Keyword(Implements))
        {
            self.advance(); // Move past implements
            loop {
                let r#interface = self.type_expression()?;
                if let TypeExpression::Union(_)
                | TypeExpression::Functional(_)
                | TypeExpression::This { .. }
                | TypeExpression::Invalid = r#interface
                {
                    return Err(errors::type_in_interface_position(r#interface));
                }
                interfaces.push(r#interface);
                if self
                    .token()
                    .is_some_and(|t| t._type == TokenType::Operator(Plus))
                {
                    self.advance();
                    continue;
                }
                break;
            }
        }
        // Parse default value.
        let (default, end) = if self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(Assign))
        {
            self.advance(); // Move past =
            let default = self.type_expression()?;
            let end = default.span().end;
            (Some(default), end)
        } else {
            (
                None,
                interfaces
                    .last()
                    .map(|_interface| _interface.span().end)
                    .unwrap_or(name.span.end),
            )
        };

        let generic_param = GenericParameter {
            name,
            interfaces,
            default,
            span: Span { start, end },
        };
        Ok(generic_param)
    }

    /// Parses the parameters of a function. It assumes that `(` should be the current token.
    fn parameters(&self) -> Fallible<Vec<Parameter>> {
        expect!(TokenType::Bracket(LParens), self);
        self.advance(); // Move past (

        let mut parameters = vec![];
        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Bracket(RParens))
        {
            parameters.push(self.parameter()?);
            if self.token().unwrap()._type == TokenType::Operator(Comma) {
                self.advance();
                continue;
            }
            break;
        }

        expect!(TokenType::Bracket(RParens), self);
        self.advance(); // Close )

        Ok(parameters)
    }

    /// Parses a type label. It assumes that `:` is maybe the current token.
    fn maybe_type_label(&self) -> Fallible<Option<TypeExpression>> {
        if !self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(Colon))
        {
            return Ok(None);
        }
        Ok(Some(self.type_label()?))
    }

    /// Parses a return_type. It supports the `:` or `->` syntax.
    fn maybe_return_type(&self) -> Fallible<Option<TypeExpression>> {
        if self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(Colon))
        {
            return Ok(Some(self.type_label()?));
        }
        if self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(Arrow))
        {
            self.advance(); // Move past ->
            let expression = self.type_expression()?;
            return Ok(Some(expression));
        }
        return Ok(None);
    }

    /// Parses a block of statements. It assumes that `{` is the current token.
    fn block(&self, scope_type: ScopeType) -> Imperfect<Block> {
        expect_or_return!(TokenType::Bracket(LCurly), self);

        let start = self.token().unwrap().span.start;
        self.advance(); // Move past {

        self.module_ambience().enter(scope_type);

        let mut statements = vec![];
        // Collects any errors found so that parsing can continue.
        let mut errors = vec![];
        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Bracket(RCurly))
        {
            let mut partial = self.statement();
            // Collect errors encountered.
            errors.append(&mut partial.errors);
            // Collect the statement in parsing was successful.
            if let Some(statement) = partial.value {
                statements.push(statement);
            }
        }

        expect_or_return!(TokenType::Bracket(RCurly), self);
        let end = self.token().unwrap().span.end;
        self.advance(); // Close }

        let scope_id = self.module_ambience().current_scope();
        self.module_ambience().leave_scope();

        let block = Block {
            scope_id,
            statements,
            span: Span { start, end },
        };

        Partial {
            value: Some(block),
            errors,
        }
    }

    /// Parses a parameter. Assumes that the parameter name is the current token.
    fn parameter(&self) -> Fallible<Parameter> {
        let info = self.get_doc_comment();
        let name = self.identifier()?;
        let start = name.span.start;
        let mut end = None;
        let is_optional = if self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(QuestionMark))
        {
            end = Some(self.token().unwrap().span.end);
            self.advance();
            true
        } else {
            false
        };

        let type_label = if self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(Colon))
        {
            let type_exp = self.type_label()?;
            end = Some(type_exp.span().end);
            Some(type_exp)
        } else {
            None
        };

        if end.is_none() {
            end = Some(name.span.end);
        }

        let parameter = Parameter {
            name,
            info,
            type_label,
            is_optional,
            span: Span {
                start,
                end: end.unwrap(),
            },
        };

        Ok(parameter)
    }

    /// Parses a shorthand variable declaration. Assumes that the name is the current token.
    fn shorthand_variable_declaration(&self, name: Identifier) -> Imperfect<Statement> {
        let start = name.span.start;
        let info = self.get_doc_comment();
        let var_type = check!(self.maybe_type_label());

        expect_or_return!(TokenType::Operator(ColonAssign), self);

        self.advance(); // Move past :=

        let (expression, expr_errors) = self.expression().to_tuple();
        if expression.is_none() {
            return Partial::from_errors(expr_errors);
        }
        let value = expression.unwrap();

        let signature = ShorthandVariableSignature {
            name,
            info,
            var_type,
        };

        let entry_no = self
            .module_ambience()
            .register(ScopeEntry::ShorthandVariable(signature));

        // Make semicolon error less fatal.
        let end = if self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(SemiColon))
        {
            let end = self.token().unwrap().span.end;
            self.advance(); // Move past ;
            end
        } else {
            // expr_errors.push(expected(
            //     TokenType::Operator(SemiColon),
            //     self.last_token_end(),
            // ));
            value.span().end
        };

        let statement = Statement::ShorthandVariableDeclaration(ShorthandVariableDeclaration {
            address: [
                self.module_ambience().id(),
                self.module_ambience().current_scope(),
                entry_no,
            ]
            .into(),
            value,
            span: Span::from([start, end]),
        });
        Partial::from_tuple((Some(statement), expr_errors))
    }

    /// Parse a constant declaration.
    fn constant_declaration(&self, is_public: bool) -> Imperfect<ConstantDeclaration> {
        expect_or_return!(TokenType::Keyword(Const), self);
        let start = self.token().unwrap().span.start;
        let info = self.get_doc_comment();
        self.advance(); // Move past const.
        let name = check!(self.identifier());
        let var_type = check!(self.type_label());
        let mut errors = vec![];

        let signature = ConstantSignature {
            name,
            info,
            is_public,
            var_type,
        };
        let entry_no = self
            .module_ambience()
            .register(ScopeEntry::Constant(signature));

        let address = ScopeAddress {
            module_id: self.module_ambience().module_id,
            scope_id: self.module_ambience().current_scope(),
            entry_no,
        };

        let end;
        let partial = if !self
            .token()
            .is_some_and(|token| token._type == TokenType::Operator(Assign))
        {
            errors.push(expected(TokenType::Operator(Assign), self.last_token_end()));
            Partial::from_errors(errors)
        } else {
            self.advance(); // Move past =
            let (value_value, mut value_errors) = self.expression().to_tuple();
            errors.append(&mut value_errors);
            if value_value.is_none() {
                return Partial::from_errors(errors);
            }
            let value = value_value.unwrap();
            if self
                .token()
                .is_some_and(|token| token._type == TokenType::Operator(SemiColon))
            {
                end = self.token().unwrap().span.end;
                self.advance();
            } else {
                // errors.push(expected(
                //     TokenType::Operator(SemiColon),
                //     self.last_token_end(),
                // ));
                end = self.last_token_end().end;
            }
            Partial {
                value: Some(ConstantDeclaration {
                    address,
                    value,
                    span: Span { start, end },
                }),
                errors,
            }
        };

        partial
    }
}

// TYPES.
impl<L: Lexer> Parser<L> {
    /// Parses a type label. Assumes that `:` is unconditionally the current token.
    fn type_label(&self) -> Fallible<ast::TypeExpression> {
        expect!(TokenType::Operator(Colon), self);
        self.advance(); // Move past :
        let expression = self.type_expression()?;
        Ok(expression)
    }

    /// Parses a type expression. Assumes that the first identifier or the function keyword is the current token.
    fn type_expression(&self) -> Fallible<TypeExpression> {
        self.ended(errors::identifier_expected(self.last_token_span()))?;

        let token = self.token().unwrap();

        let type_expr = match token._type {
            TokenType::Keyword(Fn) => self.functional_type()?,
            TokenType::Keyword(If) => self.ternary_type()?,
            TokenType::Keyword(Async) => return Err(errors::async_type(token.span)),
            // `This` type.
            TokenType::Keyword(This) => self.this_type()?,
            // Support alternate syntax for union, where | is preceeding.
            TokenType::Operator(BitOr) => {
                self.push_precedence(ExpressionPrecedence::TypeUnion);
                self.advance(); // Move past |
                let first_exp = self.type_expression()?;
                self.precedence_stack.borrow_mut().pop();
                let union = self.union_type(first_exp)?;
                union
            }
            // Grouping of type expressions.
            TokenType::Bracket(LParens) => {
                self.advance(); // Move past (
                let node = self.type_expression()?;
                expect!(TokenType::Bracket(RParens), self);
                self.advance(); // Move past )
                self.type_reparse(node)?
            }
            TokenType::Ident(_) => self.regular_type_or_union()?,
            // ?optional types.
            TokenType::Operator(QuestionMark) => self.optional_type()?,
            // Support for an array type.
            TokenType::Bracket(LSquare) => self.array_type()?,
            _ => return Err(errors::identifier_expected(token.span)),
        };

        Ok(self.type_reparse(type_expr)?)
    }

    /// Parses a functional type. Assumes that `fn` is the current token.
    fn functional_type(&self) -> Fallible<TypeExpression> {
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past fn.
        let generic_params = self.maybe_generic_params()?;
        let params = if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(LParens))
        {
            self.parameters()?
        } else {
            vec![]
        };
        let return_type = self.maybe_return_type()?.map(|exp| Box::new(exp));
        let span = Span::from([start, self.last_token_span().end]);

        let functype = TypeExpression::Functional(FunctionalType {
            params,
            generic_params,
            return_type,
            span,
        });

        Ok(self.type_reparse(functype)?)
    }

    /// Parses a conditional type. Assumes that `if` is the current token.
    fn ternary_type(&self) -> Fallible<TypeExpression> {
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past if.
        let clause = self.type_clause()?;
        let consequent = Box::new(self.type_expression()?);
        expect!(TokenType::Keyword(Else), self);
        self.advance(); // Move past else.
        let alternate = Box::new(self.type_expression()?);

        let span = Span {
            start,
            end: alternate.span().end,
        };
        Ok(TypeExpression::Ternary(TernaryType {
            clause: Box::new(clause),
            consequent,
            alternate,
            span,
        }))
    }

    /// Parses a discrete, member or union type. Assumes that identifier is the current token.
    fn regular_type_or_union(&self) -> Fallible<TypeExpression> {
        let name = self.identifier()?;
        let generic_args = self.maybe_generic_args()?;
        let start = name.span.start;
        let end = self.last_token_span().end;
        let span = Span::from([start, end]);

        let consequent = DiscreteType {
            name,
            generic_args,
            span,
        };

        // Parse constraint.
        if self.is_lower_or_equal_precedence(ExpressionPrecedence::Access) {
            return Ok(TypeExpression::Discrete(consequent));
        }
        let expression = if let Some((clause, clause_span)) = self.maybe_type_constraint()? {
            TypeExpression::Constraint(BoundConstraintType {
                consequent,
                clause: Box::new(clause),
                span: Span {
                    start,
                    end: clause_span.end,
                },
            })
        } else {
            TypeExpression::Discrete(consequent)
        };
        Ok(self.type_reparse(expression)?)
    }

    /// Optionally parses a type constraint. It will advance if `|=` is the next token.
    fn maybe_type_constraint(&self) -> Fallible<Option<(TypeClause, Span)>> {
        match self.token() {
            Some(token) if token._type == TokenType::Operator(Constraint) => {
                let start = token.span.start;
                self.advance(); // Move past |=
                let clause = self.type_clause()?;
                let end = self.last_token_end().end;
                let span = Span { start, end };
                return Ok(Some((clause, span)));
            }
            _ => return Ok(None),
        };
    }

    /// Parses a type clause.
    fn type_clause(&self) -> Fallible<TypeClause> {
        // grouped type clause.
        if self
            .token()
            .is_some_and(|token| token._type == TokenType::Bracket(LParens))
        {
            self.advance(); // Move past (
            let clause = self.type_clause()?;
            expect!(TokenType::Bracket(RParens), self);
            self.advance(); // Move past )
            return Ok(clause);
        }
        let base = self.identifier()?;
        self.ended(errors::expected(
            TokenType::Keyword(Implements),
            self.last_token_end(),
        ))?;
        let token = self.token().unwrap();
        let clause = match token._type {
            TokenType::Keyword(Implements) => {
                let interfaces = self.maybe_interface_implementations()?;
                TypeClause::Implementations { base, interfaces }
            }
            _ => return Err(errors::expected(TokenType::Keyword(Implements), token.span)),
        };
        Ok(self.type_clause_spring(clause)?)
    }

    /// Continues a type clause.
    fn type_clause_spring(&self, clause: TypeClause) -> Fallible<TypeClause> {
        if self.token().is_none() {
            return Ok(clause);
        }
        match self.token().unwrap()._type {
            TokenType::Operator(And) => {
                self.advance(); // Move past and.
                return Ok(TypeClause::Binary {
                    left: Box::new(clause),
                    operator: ast::LogicOperator::AndLiteral,
                    right: Box::new(self.type_clause()?),
                });
            }
            TokenType::Operator(Or) => {
                self.advance(); // Move past or.
                return Ok(TypeClause::Binary {
                    left: Box::new(clause),
                    operator: ast::LogicOperator::OrLiteral,
                    right: Box::new(self.type_clause()?),
                });
            }
            TokenType::Operator(LogicalAnd) => {
                self.advance(); // Move past &&.
                return Ok(TypeClause::Binary {
                    left: Box::new(clause),
                    operator: ast::LogicOperator::And,
                    right: Box::new(self.type_clause()?),
                });
            }
            TokenType::Operator(LogicalOr) => {
                self.advance(); // Move past &&.
                return Ok(TypeClause::Binary {
                    left: Box::new(clause),
                    operator: ast::LogicOperator::Or,
                    right: Box::new(self.type_clause()?),
                });
            }
            _ => return Ok(clause),
        }
    }

    /// Parses an optional type. Assumes that `?` is the current token.
    fn optional_type(&self) -> Fallible<TypeExpression> {
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past &
        self.push_precedence(ExpressionPrecedence::Option); // todo: not the correct precedence.
        let value = Box::new(self.type_expression()?);
        let end = value.span().end;
        let span = Span { start, end };
        self.precedence_stack.borrow_mut().pop();
        let node = TypeExpression::Optional(MaybeType { value, span });
        Ok(self.type_reparse(node)?)
    }

    /// Parses an array type. Assumes that `]` is the next token.
    /// todo: Should numeric values be usable?
    fn array_type(&self) -> Fallible<TypeExpression> {
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past [].
        let mut error_on_numeric_type = None;
        if let Some(Token {
            _type: TokenType::Number(_),
            span,
        }) = self.token()
        {
            error_on_numeric_type = Some(span);
            self.advance(); // Move past number.
        }
        expect!(TokenType::Bracket(RSquare), self);
        self.advance(); // Move past ]
        let element_type = self.type_expression()?;
        if let Some(span) = error_on_numeric_type {
            return Err(errors::numeric_value_in_array_type(*span));
        }
        let end = element_type.span().end;
        let span = Span { start, end };
        let node = TypeExpression::Array(ArrayType {
            element_type: Box::new(element_type),
            span,
        });
        Ok(self.type_reparse(node)?)
    }
    /// Looks ahead to determing how to parse type precedence.
    fn type_reparse(&self, node: TypeExpression) -> Fallible<TypeExpression> {
        match self.token() {
            Some(t) if t._type == TokenType::Operator(Dot) => self.member_type(node),
            Some(t) if t._type == TokenType::Operator(BitOr) => self.union_type(node),
            _ => return Ok(node),
        }
    }

    /// Parse a member type.
    fn member_type(&self, namespace: TypeExpression) -> Fallible<TypeExpression> {
        if self.is_lower_or_equal_precedence(ExpressionPrecedence::Access) {
            return Ok(namespace);
        }
        //Move past .
        self.advance();
        // Namespaces should not have generic arguments.
        if let TypeExpression::Discrete(ref d) = namespace {
            if d.generic_args.is_some() {
                return Err(errors::generic_args_in_namespace(d.span));
            }
        }
        // Only discrete types and other member types can serve as namespaces.
        if !matches!(
            namespace,
            TypeExpression::Discrete(_) | TypeExpression::Member(_)
        ) {
            return Err(errors::unexpected(self.last_token_span()));
        }
        self.push_precedence(ExpressionPrecedence::Access);
        let property = Box::new(self.regular_type_or_union()?);
        let span = Span::from([namespace.span().start, property.span().end]);
        let member_type = MemberType {
            namespace: Box::new(namespace),
            property,
            span,
        };
        self.precedence_stack.borrow_mut().pop();
        Ok(self.type_reparse(TypeExpression::Member(member_type))?)
    }

    /// Parses union type.
    fn union_type(&self, mut first_type: TypeExpression) -> Fallible<TypeExpression> {
        if self.is_lower_or_equal_precedence(ExpressionPrecedence::TypeUnion) {
            return Ok(first_type);
        }
        self.advance(); // Move past |
        self.push_precedence(ExpressionPrecedence::TypeUnion);
        let start = first_type.span().start;
        let mut types = vec![];

        // Concatenate incoming union types as one union type.
        if let TypeExpression::Union(ref mut u) = first_type {
            types.append(&mut u.types)
        } else {
            types.push(first_type);
        }

        let mut second_type = self.type_expression()?;
        self.precedence_stack.borrow_mut().pop();

        // Concatenate parsed union types as one union type.
        if let TypeExpression::Union(ref mut u) = second_type {
            types.append(&mut u.types)
        } else {
            types.push(second_type)
        }

        let span = Span {
            start,
            end: types.last().unwrap().span().end,
        };
        let node = TypeExpression::Union(UnionType { types, span });

        Ok(self.type_reparse(node)?)
    }

    /// Parses `This` type. Assumes that the current token is `This`.
    fn this_type(&self) -> Fallible<TypeExpression> {
        expect!(TokenType::Keyword(This), self);
        let span = self.token().unwrap().span;
        self.advance(); // Move past This.
        return Ok(self.type_reparse(TypeExpression::This { span })?);
    }

    /// Parses generic arguments if they exist.
    fn maybe_generic_args(&self) -> Fallible<Option<Vec<TypeExpression>>> {
        if !self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(LesserThan))
        {
            return Ok(None);
        }
        Ok(Some(self.generic_args()?))
    }

    /// Parser generic arguments. Assumes that `<` is the current token.
    fn generic_args(&self) -> Fallible<Vec<TypeExpression>> {
        expect!(TokenType::Operator(LesserThan), self);
        self.advance(); // Move past <
        let mut arguments = vec![];
        while self.token().is_some_and(|t| {
            ![
                TokenType::Operator(GreaterThan),
                TokenType::Operator(RightShift),
            ]
            .contains(&t._type)
        }) {
            let argument = self.type_expression()?;
            arguments.push(argument);
            if self
                .token()
                .is_some_and(|t| t._type == TokenType::Operator(Comma))
            {
                self.advance(); // move past ,
            } else {
                break; // Unexpected or empty token.
            }
        }
        if self.token().is_none() {
            return Err(expected(
                TokenType::Operator(GreaterThan),
                self.last_token_span(),
            ));
        }
        let token = self.token().unwrap();
        match token._type {
            TokenType::Operator(GreaterThan) => {
                self.advance(); // Move past >
                *self.waiting_for_second_angular_bracket.borrow_mut() = false;
            }
            TokenType::Operator(RightShift) => {
                // Wait and advance the second time.
                if *self.waiting_for_second_angular_bracket.borrow() {
                    self.advance(); // Move past >>
                    *self.waiting_for_second_angular_bracket.borrow_mut() = false;
                } else {
                    *self.waiting_for_second_angular_bracket.borrow_mut() = true;
                }
            }
            // Unexpected token.
            _ => return Err(expected(TokenType::Operator(GreaterThan), token.span)),
        }
        Ok(arguments)
    }
}

impl<L: Lexer> Iterator for Parser<L> {
    type Item = Partial<Statement, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Kickstart the parsing with the first token.
        if self.past.borrow().is_none() {
            self.advance();
        }
        self.token()?;
        Some(self.statement())
    }
}

fn unroll_to_discrete_types(membertype: MemberType) -> Result<Vec<DiscreteType>, ParseError> {
    let mut types = vec![];
    match *membertype.namespace {
        TypeExpression::Member(membertype) => {
            types.append(&mut unroll_to_discrete_types(membertype)?)
        }
        TypeExpression::Discrete(discretetype) => types.push(discretetype),
        type_ => return Err(errors::type_in_interface_position(type_)),
    }
    match *membertype.property {
        TypeExpression::Member(membertype) => {
            types.append(&mut unroll_to_discrete_types(membertype)?)
        }
        TypeExpression::Discrete(discretetype) => types.push(discretetype),
        type_ => return Err(errors::type_in_interface_position(type_)),
    }
    Ok(types)
}
