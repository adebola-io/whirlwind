use std::cell::RefCell;
use std::sync::{Arc, Mutex};

use crate::errors::{self, ParseError};
use crate::scope::{ScopeManager, ScopeType};
use whirl_ast::{
    Block, FunctionDeclaration, FunctionSignature, GenericParameter, Identifier, Parameter,
    ParserType, Span, Statement,
};
use whirl_lexer::{Bracket::*, Keyword, Lexer, Operator::*, Token, TokenType};

pub struct Parser<L: Lexer> {
    pub lexer: RefCell<L>,
    _scope_manager: RefCell<ScopeManager>,
    present: RefCell<Option<Token>>,
    past: RefCell<Option<Token>>,
    future: RefCell<Option<Token>>,
}

type Fallible<T> = Result<T, ParseError>;

/// Shorthand for current token confirmation.
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
            _scope_manager: RefCell::new(ScopeManager::new()),
            present: RefCell::new(None),
            past: RefCell::new(None),
            future: RefCell::new(None),
        }
    }

    /// Returns the next token from the lexer.
    fn token(&self) -> Option<&mut Token> {
        unsafe { (*self.present.as_ptr()).as_mut() }
    }
    /// Advance to the next token.
    fn advance(&self) {
        self.past.replace(self.present.take());
        self.present.replace(
            self.future
                .take()
                .or(self.lexer.borrow_mut().next_useful_token()),
        );
    }
    /// Rewinds to the last token.
    fn _back(&self) {
        self.future.replace(self.present.take());
        self.present.replace(self.past.take());
    }

    /// Returns a reference to the scope manager.
    pub fn scope_manager(&self) -> &mut ScopeManager {
        unsafe { &mut *self._scope_manager.as_ptr() }
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
}

impl<L: Lexer> Parser<L> {
    /// Parses a statement.
    fn statement(&self) -> Result<Statement, ParseError> {
        self.ended(errors::declaration_or_statement_expected(
            self.last_token_end(),
        ))?;

        match self.token().unwrap()._type {
            TokenType::Keyword(Keyword::Function) => self.function(false),
            _ => unreachable!(),
        }
    }

    fn function(&self, is_async: bool) -> Result<Statement, ParseError> {
        let start = self.token().unwrap().span.start;

        let name = self.identifier()?;
        let generic_params = self.maybe_generic_params()?;
        let params = self.parameters()?;
        let return_type = self.maybe_return_type()?;
        let body = self.block(ScopeType::Functional)?;

        let signature = Arc::new(Mutex::new(FunctionSignature {
            name,
            is_async,
            params,
            generic_params,
            return_type,
            full_span: Span::from([start, body.span.end]),
            references: vec![],
        }));

        self.scope_manager().register_function(signature.clone());

        Ok(Statement::FunctionDeclaration(FunctionDeclaration {
            signature,
            body,
        }))
    }

    /// Parses an identifier and advances. It assumes that the identifier is curreently the next token.
    fn identifier(&self) -> Fallible<Identifier> {
        self.advance();
        self.ended(errors::identifier_expected(self.last_token_end()))?;

        let token = self.token().unwrap();

        match token._type {
            TokenType::Ident(ref mut name) => {
                let identifier = Identifier {
                    name: std::mem::take(name),
                    span: token.span,
                };
                self.advance();
                Ok(identifier)
            }
            _ => Err(errors::identifier_expected(self.last_token_span())),
        }
    }

    /// Parses a class' or function's generic parameters. Assumes that `<` (or `<<`) is maybe the current token.
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
        }

        expect!(TokenType::Bracket(RParens), self);
        self.advance(); // Close )

        Ok(parameters)
    }

    /// Parses a function's return type. It assumes that `:` is maybe the current token.
    fn maybe_return_type(&self) -> Fallible<Option<ParserType>> {
        if !self
            .token()
            .is_some_and(|t| t._type == TokenType::Operator(Colon))
        {
            return Ok(None);
        }
        todo!()
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

    fn parameter(&self) -> Fallible<Parameter> {
        todo!()
    }
}

impl<L: Lexer> Iterator for Parser<L> {
    type Item = Fallible<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance();
        self.token()?;
        let statement_or_error = self.statement();
        Some(statement_or_error)
    }
}

// impl<Lexer: whirl_lexer::Lexer> Parser<Lexer> {
//     /// Parses the next statement in a stream.
//     pub fn parse_top_level_statement(&mut self) -> Option<Fallible<Statement>> {
//         let token = self.next_token()?;
//         Some(self.statement(token))
//     }

//     fn next_token(&mut self) -> Option<Token> {
//         self.lexer.skip_comments_to_next_token().map(|token| {
//             self.last_span = token.span;
//             token
//         })
//     }

//     /// Parse a public declaration.
//     pub fn public_declaration(&mut self, token: Token) -> Fallible<Statement> {
//         match self.next_token() {
//             Some(token) => match token.token_type {
//                 TokenType::Keyword(
//                     Keyword::Function
//                     | Keyword::Class
//                     | Keyword::Type
//                     | Keyword::Use
//                     | Keyword::Trait
//                     | Keyword::Record
//                     | Keyword::Const
//                     | Keyword::Var,
//                 ) => {
//                     let statement = self.declaration()?;
//                     Ok(Statement::PublicDeclaration(PublicDeclaration {
//                         span: Span::from([token.span.start, statement.span().end]),
//                         declaration: Box::new(statement),
//                     }))
//                 }
//                 // Parse shorthand variable declarations but mark them as syntax errors.
//                 TokenType::Ident(_) => {
//                     let statement = self.expression_statement(token)?;
//                     if statement.is_variable_declaration() {
//                         Err(errors::public_shorthand_var(statement.span()))
//                     } else {
//                         Err(errors::declaration_expected(statement.span()))
//                     }
//                 }
//                 _ => todo!(),
//             },
//             None => Err(errors::declaration_or_statement_expected(token.span)),
//         }
//     }

//     pub fn statement(&mut self, token: Token) -> Fallible<Statement> {
//         match token.token_type {
//             TokenType::Comment(_) => todo!(),
//             TokenType::Keyword(Keyword::Public) => self.public_declaration(token),
//             TokenType::Keyword(Keyword::Function) => self.function(token.span, false),
//             TokenType::Keyword(Keyword::Async) => self.async_function(),
//             _ => self.statement(token),
//         }
//     }

//     fn expression_statement(&self, token: Token) -> Fallible<Statement> {
//         todo!()
//     }

//     fn declaration(&mut self) -> Fallible<Statement> {
//         todo!()
//     }

//     /// Parse an identifier.
//     fn identifier(&mut self) -> Fallible<Identifier> {
//         let previous_span = self.last_span;
//         match self.next_token() {
//             Some(token) => match token.token_type {
//                 TokenType::Ident(name) => Ok(Identifier {
//                     name,
//                     span: token.span,
//                 }),
//                 _ => Err(errors::identifier_expected(token.span)),
//             },
//             None => Err(errors::identifier_expected(previous_span)),
//         }
//     }

//     /// Parse a function.
//     fn function(&mut self, start: Span, is_async: bool) -> Fallible<Statement> {
//         let name = self.identifier()?;
//         let generic_params = self.maybe_generic_parameters()?;

//         self.expect(TokenType::lparen())?;

//         let params = self.parameterlist()?;
//         let return_type = self.maybe_type_label()?;
//         let body = self.block()?;

//         let signature = Rc::new(RefCell::new(FunctionSignature {
//             name,
//             is_async,
//             params,
//             generic_params,
//             return_type,
//             full_span: Span::from([start.start, body.span.end]),
//         }));

//         Ok(Statement::FunctionDeclaration(FunctionDeclaration {
//             signature,
//             body,
//         }))
//     }

//     /// Parse an async function.
//     fn async_function(&mut self) -> Result<Statement, ParserError> {
//         let start = self.last_span;
//         match self.next_token() {
//             Some(Token { token_type, span })
//                 if token_type == TokenType::Keyword(Keyword::Function) =>
//             {
//                 self.function(span, true)
//             }
//             Some(Token { span, .. }) => Err(errors::expected(
//                 TokenType::Keyword(Keyword::Function),
//                 span,
//             )),
//             None => Err(errors::expected_after(
//                 TokenType::Keyword(Keyword::Function),
//                 start,
//             )),
//         }
//     }

//     #[inline(always)]
//     /// Unconditionally expect a token.
//     fn expect(&mut self, token_type: TokenType) -> Fallible<()> {
//         match self.next_token() {
//             Some(token) => match token.token_type {
//                 tok if tok == token_type => Ok(()),
//                 _ => return Err(errors::expected(token_type, token.span)),
//             },
//             None => return Err(errors::expected_after(token_type, self.last_span)),
//         }
//     }

//     fn block(&self) -> Fallible<Block> {
//         todo!()
//     }

//     /// Optionally parses generic parameters of a function or class.
//     fn maybe_generic_parameters(&mut self) -> Fallible<Option<Vec<GenericParameter>>> {
//         Ok(None)
//     }

//     /// Parse a parameter list. It takes in the span of the first `(`.
//     fn parameterlist(&mut self) -> Fallible<Vec<Parameter>> {
//         let mut parameters = vec![];
//         loop {
//             match self.next_token().map(|token| token.token_type) {
//                 Some(token_type) => match token_type {
//                     TokenType::Bracket(RParens) => break,
//                     TokenType::Ident(name) => match self.next_token().map(|token| token.token_type) {
//                         Some(token) => {

//                         },
//                         None => return Err(errors::expected(TokenType::rparens(), token_type.span)),
//                     },
//                     _ => return Err(errors::expected(TokenType::rparens(), token_type.span)),
//                 },
//                 None => return Err(errors::expected_after(TokenType::rparens(), self.last_span)),
//             }
//         }
//         Ok(parameters)
//     }

//     fn maybe_type_label(&self) -> Fallible<Option<Type>> {
//         todo!()
//     }
// }
