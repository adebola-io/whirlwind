use std::cell::RefCell;
use std::sync::{Arc, Mutex};

use crate::errors::{self, ParseError};

use whirl_ast::{
    Block, FunctionDeclaration, FunctionSignature, GenericParameter, Identifier, Parameter,
    ParserType, ScopeManager, ScopeType, Span, Statement,
};
use whirl_lexer::{Bracket::*, Comment, Keyword, Lexer, Operator::*, Token, TokenType};

pub struct Parser<L: Lexer> {
    pub lexer: RefCell<L>,
    _scope_manager: RefCell<ScopeManager>,
    present: RefCell<Option<Token>>,
    past: RefCell<Option<Token>>,
    future: RefCell<Option<Token>>,
    doc_comments: RefCell<Vec<String>>,
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
            match self.lexer.borrow_mut().next() {
                Some(token) => match token._type {
                    TokenType::Comment(comment) => {
                        if let Comment::DocComment(text) = comment {
                            self.doc_comments.borrow_mut().push(text)
                        }
                    }
                    TokenType::Invalid(_) => {}
                    _ => return Some(token),
                },
                None => return None,
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

    /// Returns the documentation comments before a position.
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

        match self.token().unwrap()._type {
            TokenType::Keyword(Keyword::Function) => self.function(false),
            ref tokentype => {
                println!("{:?} not implemented yet!", tokentype);
                unimplemented!()
            }
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
            info: self.get_doc_comment(),
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
