use std::cell::RefCell;

use crate::errors::{self, ParseError};
use whirl_ast::{
    Block, DiscreteType, Expression, ExpressionPrecedence, FunctionDeclaration, FunctionSignature,
    FunctionalType, GenericParameter, Identifier, MemberType, Parameter, ScopeAddress,
    ScopeManager, ScopeType, Span, Statement, Type, TypeDeclaration, TypeExpression, TypeSignature,
    UnionType,
};
use whirl_lexer::{Bracket::*, Comment, Keyword::*, Lexer, Operator::*, Token, TokenType};

/// A recursive descent parser that reads tokens lazily and returns statements.
/// It keeps tracks of three tokens to allow for limited backtracking.
pub struct Parser<L: Lexer> {
    pub lexer: RefCell<L>,
    scope_manager: RefCell<ScopeManager>,
    present: RefCell<Option<Token>>,
    past: RefCell<Option<Token>>,
    future: RefCell<Option<Token>>,
    doc_comments: RefCell<Vec<String>>,
    precedence_stack: RefCell<Vec<ExpressionPrecedence>>,
    /// Handles ambiguity caused by nested generic arguments conflicting with bit right shift.
    waiting_for_second_angular_bracket: RefCell<bool>,
}

type Fallible<T> = Result<T, ParseError>;

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

impl<L: Lexer> Parser<L> {
    /// Creates a parser from a lexer.
    pub fn from_lexer(lexer: L) -> Self {
        Self {
            lexer: RefCell::new(lexer),
            scope_manager: RefCell::new(ScopeManager::new()),
            present: RefCell::new(None),
            past: RefCell::new(None),
            future: RefCell::new(None),
            doc_comments: RefCell::new(vec![]),
            precedence_stack: RefCell::new(vec![]),
            waiting_for_second_angular_bracket: RefCell::new(false),
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
            .replace(self.future.take().or(self.next_useful_token()));
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
    /// Rewinds to the last token.
    fn _back(&self) {
        self.future.replace(self.present.take());
        self.present.replace(self.past.take());
    }

    /// Push a precedence to the stack.
    fn push_precedence(&self, precedence: ExpressionPrecedence) {
        self.precedence_stack.borrow_mut().push(precedence);
    }

    /// Returns a reference to the scope manager.
    pub fn scope_manager(&self) -> &mut ScopeManager {
        unsafe { &mut *self.scope_manager.as_ptr() }
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
        match self.precedence_stack.borrow().last() {
            Some(p) => *p <= precedence,
            None => false,
        }
    }
}

impl<L: Lexer> Parser<L> {
    /// Parses an expression.
    fn expression(&self) -> Fallible<Expression> {
        todo!()
    }
}

// Statements
impl<L: Lexer> Parser<L> {
    /// Parses a statement.
    fn statement(&self) -> Result<Statement, ParseError> {
        self.ended(errors::declaration_or_statement_expected(
            self.last_token_end(),
        ))?;

        let token = match self.token().unwrap()._type {
            // function...
            TokenType::Keyword(Function) => self
                .function(false, false)
                .map(|f| Statement::FunctionDeclaration(f)),
            // public...
            TokenType::Keyword(Public) => self.public_declaration(),
            // async...
            TokenType::Keyword(Async) => self
                .async_function(false)
                .map(|f| Statement::FunctionDeclaration(f)),
            // type...
            TokenType::Keyword(whirl_lexer::Keyword::Type) => self
                .type_declaration(false)
                .map(|t| Statement::TypeDeclaration(t)),
            _ => {
                unimplemented!(
                    "{:?} not implemented yet!. The last token was {:?}",
                    self.token().unwrap(),
                    self.past.borrow_mut()
                )
            }
        };

        // If an error is encountered, clear the precedence stack and skip all the next (likely corrupted) tokens until after a right delimeter or boundary.
        // Then resume normal parsing.
        if token.is_ok() {
            return token;
        }
        self.precedence_stack.borrow_mut().clear();
        loop {
            match self.token() {
                Some(token) => match token._type {
                    TokenType::Bracket(RCurly | RParens | RSquare)
                    | TokenType::Operator(SemiColon | GreaterThan | RightShift) => break,
                    _ => self.advance(),
                },
                None => break,
            }
        }
        token
    }

    /// Parses a function. It assumes that `function` is the current token, and has already been checked.
    fn function(&self, is_async: bool, is_public: bool) -> Fallible<FunctionDeclaration> {
        expect!(TokenType::Keyword(Function), self);
        let start = self.token().unwrap().span.start;

        let info = self.get_doc_comment();
        self.advance(); // Move past function.

        let name = self.identifier()?;
        let generic_params = self.maybe_generic_params()?;
        let params = self.parameters()?;
        let return_type = self.maybe_return_type()?;
        let body = self.block(ScopeType::Functional)?;

        let signature = FunctionSignature {
            name,
            is_async,
            info,
            params,
            generic_params,
            return_type,
            is_public,
        };

        let entry_no = self.scope_manager().register_function(signature);

        let function = FunctionDeclaration {
            address: ScopeAddress {
                scope_id: self.scope_manager().current(),
                entry_no,
            },
            span: Span::from([start, body.span.end]),
            body,
        };

        Ok(function)
    }

    /// Parses an async function. Assumes that `async` is the current token (and has already been checked).
    fn async_function(&self, is_public: bool) -> Fallible<FunctionDeclaration> {
        expect!(TokenType::Keyword(Async), self);
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past async.

        expect!(TokenType::Keyword(Function), self);

        let mut function = self.function(true, is_public)?;
        function.span.start = start;

        Ok(function)
    }

    /// Parses a public declaration. Assumes that `public` is the current token.
    fn public_declaration(&self) -> Fallible<Statement> {
        expect!(TokenType::Keyword(Public), self);

        let start = self.token().unwrap().span.start;

        self.advance(); // Move past public.

        self.ended(errors::declaration_expected(self.last_token_span()))?;

        let token = self.token().unwrap();

        let mut statement = match token._type {
            // Repeated.
            TokenType::Keyword(Public) => return Err(errors::declaration_expected(token.span)),

            TokenType::Keyword(Function) => {
                Statement::FunctionDeclaration(self.function(false, true)?)
            }
            TokenType::Keyword(Test) => return Err(errors::public_test(token.span)),
            TokenType::Keyword(Async) => Statement::FunctionDeclaration(self.async_function(true)?),
            TokenType::Keyword(whirl_lexer::Keyword::Type) => {
                Statement::TypeDeclaration(self.type_declaration(true)?)
            }
            // Parse public shorthand variable declaration as syntax error.
            TokenType::Ident(_) => {
                let statement = self.statement()?;
                return if statement.is_variable_declaration() {
                    Err(errors::public_shorthand_var(statement.span()))
                } else {
                    Err(errors::declaration_expected(Span::from([start, start])))
                };
            }
            _ => return Err(errors::declaration_expected(token.span)),
        };

        statement.set_start(start);

        Ok(statement)
    }

    /// Parses a type declaration. Assumes that `type` is the current token.
    fn type_declaration(&self, is_public: bool) -> Fallible<TypeDeclaration> {
        expect!(TokenType::Keyword(Type), self);
        let start = self.token().unwrap().span.start;

        let info = self.get_doc_comment();
        self.advance(); // move past type.

        let name = self.identifier()?;
        let generic_params = self.maybe_generic_params()?;
        expect!(TokenType::Operator(Assign), self);
        self.advance(); // Move past =

        let value = self.type_expression()?;

        expect!(TokenType::Operator(SemiColon), self);
        let end = value.span().end;

        self.advance(); // Move past ;

        let span = Span::from([start, end]);

        let signature = TypeSignature {
            name,
            info,
            is_public,
            generic_params,
            value,
        };

        let entry_no = self.scope_manager().register_type(signature);

        let type_ = TypeDeclaration {
            address: ScopeAddress {
                scope_id: self.scope_manager().current(),
                entry_no,
            },
            span,
        };

        Ok(type_)
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
            Err(errors::identifier_expected(token.span))
        }
    }

    /// Parses a class' or function's generic parameters. Assumes that `<` is maybe the current token.
    fn maybe_generic_params(&self) -> Fallible<Option<Vec<GenericParameter>>> {
        if !self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(Colon))
        {
            return Ok(None);
        }
        todo!()
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

    /// Parses a function's return type. It assumes that `:` is maybe the current token.
    fn maybe_return_type(&self) -> Fallible<Type> {
        if !self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(Colon))
        {
            return Ok(whirl_ast::Type::empty());
        }
        self.type_label()
    }

    /// Parses a block of statements. It assumes that `{` is the current token.
    fn block(&self, scope_type: ScopeType) -> Fallible<Block> {
        expect!(TokenType::Bracket(LCurly), self);

        let start = self.token().unwrap().span.start;
        self.advance(); // Move past {

        self.scope_manager().enter(scope_type);

        let mut statements = vec![];
        while self
            .token()
            .is_some_and(|t| t._type != TokenType::Bracket(RCurly))
        {
            statements.push(self.statement()?)
        }

        expect!(TokenType::Bracket(RCurly), self);
        let end = self.token().unwrap().span.end;
        self.advance(); // Close }

        self.scope_manager().leave();

        let block = Block {
            statements,
            span: Span { start, end },
        };

        Ok(block)
    }

    /// Parses a parameter. Assumes that the parameter name is the current token.
    fn parameter(&self) -> Fallible<Parameter> {
        let name = self.identifier()?;

        let is_optional = if self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(QuestionMark))
        {
            self.advance();
            true
        } else {
            false
        };

        let type_label = if self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(Colon))
        {
            self.type_label()?
        } else {
            Type::empty()
        };

        let parameter = Parameter {
            name,
            type_label,
            is_optional,
        };

        Ok(parameter)
    }
}

// TYPES.
impl<L: Lexer> Parser<L> {
    /// Parses a type label. Assumes that `:` is unconditionally the current token.
    fn type_label(&self) -> Fallible<whirl_ast::Type> {
        expect!(TokenType::Operator(Colon), self);
        self.advance(); // Move past :
        let expression = self.type_expression()?;
        Ok(Type::from_expression(expression))
    }

    /// Parses a type expression. Assumes that the first identifier or the function keyword is the current token.
    fn type_expression(&self) -> Fallible<TypeExpression> {
        self.ended(errors::identifier_expected(self.last_token_span()))?;

        let token = self.token().unwrap();

        let type_expr = match token._type {
            TokenType::Keyword(Fn) => self.functional_type()?,
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
            TokenType::Ident(_) => self.regular_type_or_union()?,
            _ => return Err(errors::identifier_expected(token.span)),
        };

        Ok(self.type_reparse(type_expr)?)
    }

    /// Parses a functional type. Assumes that `fn` is the current token.
    fn functional_type(&self) -> Fallible<TypeExpression> {
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past fn.
        let generic_params = self.maybe_generic_params()?;
        let params = self.parameters()?;
        let return_type = self.maybe_return_type()?.declared.map(|exp| Box::new(exp));
        let span = Span::from([start, self.last_token_span().end]);

        let functype = TypeExpression::Functional(FunctionalType {
            params,
            generic_params,
            return_type,
            span,
        });

        Ok(self.type_reparse(functype)?)
    }

    /// Parses a discrete, member or union type. Assumes that identifier is the current token.
    fn regular_type_or_union(&self) -> Fallible<TypeExpression> {
        let name = self.identifier()?;
        let generic_args = self.maybe_generic_args()?;
        let span = name.span;

        let discrete = TypeExpression::Discrete(DiscreteType {
            name,
            generic_args,
            span,
        });

        Ok(self.type_reparse(discrete)?)
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
        // Functional types cant serve as namespaces.
        if let TypeExpression::Functional(_) = namespace {
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
            return Err(errors::expected(
                TokenType::Operator(GreaterThan),
                self.last_token_span(),
            ));
        }
        let token = self.token().unwrap();
        match token._type {
            TokenType::Operator(GreaterThan) => self.advance(), // Move past >
            TokenType::Operator(RightShift) => {
                // Wait and advance the second time.
                if self.waiting_for_second_angular_bracket.take() {
                    self.advance(); // Move past >>
                } else {
                    *self.waiting_for_second_angular_bracket.borrow_mut() = true;
                }
            }
            // Unexpected token.
            _ => {
                return Err(errors::expected(
                    TokenType::Operator(GreaterThan),
                    token.span,
                ))
            }
        }
        Ok(arguments)
    }
}

impl<L: Lexer> Iterator for Parser<L> {
    type Item = Fallible<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        // Kickstart the parsing with the first token.
        if self.past.borrow().is_none() {
            self.advance();
        }
        self.token()?;
        let statement_or_error = self.statement();
        Some(statement_or_error)
    }
}
