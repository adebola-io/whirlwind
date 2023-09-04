use std::cell::RefCell;

use crate::errors::{self, ParseError};
use whirl_ast::{
    Block, FunctionDeclaration, FunctionSignature, GenericParameter, Identifier, Parameter,
    ParserType, ScopeAddress, ScopeManager, ScopeType, SemanticType, Span, Statement,
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
}

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
            ref tokentype => {
                println!("{:?} not implemented yet!", tokentype);
                unimplemented!()
            }
        };

        // If an error is encountered, skip all the next (likely corrupted) tokens until after a right delimeter or boundary.
        // Then resume normal parsing.
        if token.is_ok() {
            return token;
        }
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
            references: vec![],
        };

        let entry_no = self.scope_manager().register_function(signature);

        let function = FunctionDeclaration {
            address: ScopeAddress {
                scope: self.scope_manager().current(),
                entry_no,
            },
            span: Span::from([start, body.span.end]),
            body,
        };

        Ok(function)
    }

    /// Parses an async function. Assumes that `async` is the current token (and has already been checked).
    fn async_function(&self, is_public: bool) -> Fallible<FunctionDeclaration> {
        let start = self.token().unwrap().span.start;
        self.advance(); // Move past async.

        expect!(TokenType::Keyword(Function), self);

        let mut function = self.function(true, is_public)?;
        function.span.start = start;

        Ok(function)
    }

    /// Parses a public declaration. Assumes that `public` is the current token.
    fn public_declaration(&self) -> Fallible<Statement> {
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

    fn type_label(&self) -> Fallible<ParserType> {
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

    /// Parses a parameter. Assumes that the parameter name is the current token.
    fn parameter(&self) -> Fallible<Parameter> {
        let name = self.identifier()?;
        let mut type_label = None;
        let mut is_optional = false;

        // Handle scenario where the next token is a ?, a comma, a colon for a type label or a ).
        let mut is_expecting_comma = false;
        loop {
            match self.token() {
                None => {
                    return Err(errors::expected(
                        TokenType::Bracket(RParens),
                        self.last_token_span(),
                    ))
                }
                Some(token) => match token._type {
                    TokenType::Operator(Colon) => {
                        type_label = Some(self.type_label()?);
                        is_expecting_comma = true;
                    }
                    TokenType::Operator(QuestionMark) => {
                        if is_expecting_comma {
                            return Err(errors::expected(TokenType::Bracket(RParens), token.span));
                        }
                        is_optional = true;
                    }
                    TokenType::Operator(Comma) => {
                        break;
                    }
                    TokenType::Bracket(RParens) => {
                        break;
                    }
                    _ => return Err(errors::expected(TokenType::Bracket(RParens), token.span)),
                },
            }
            self.advance();
        }

        if self.token().unwrap()._type == TokenType::Operator(Comma) {
            self.advance();
        } else {
        }

        let parameter = Parameter {
            name,
            type_label,
            is_optional,
            inferred_type: SemanticType::default(),
        };

        Ok(parameter)
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
