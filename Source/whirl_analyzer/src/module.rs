use crate::analyze_text;
use std::{mem::take, path::PathBuf, slice::Iter};
use whirl_ast::{Change, Positioning, ScopeManager, Spannable, Statement};
use whirl_errors::ProgramError;
use whirl_lexer::TextLexer;
use whirl_parser::Parser;
use whirl_utils::{StringEditor, StringMutation};

/// A completely parsed program.
/// This struct presents higher level operations for the frontend representation of source code.
#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub source: ModuleSource,
    pub scopes: ScopeManager,
    statements: Vec<Statement>,
    built: bool,
    errors: Vec<ProgramError>,
    line_lens: Vec<u32>,
}

#[derive(Debug)]
/// The origin of a module.
pub enum ModuleSource {
    PlainText(String),
    FilePath {
        /// Whether or not the current representation is a 1:1 match with the source file.
        sync: FilePathSync,
        /// Path to the source file.
        path: PathBuf,
    },
}

/// Values representing states of the module in regards to its source file.
#[derive(Debug)]
pub enum FilePathSync {
    /// The module is more current than the source file.
    Forward,
    /// The module is less current than the source file.
    Backward,
    /// The module is a 1:1 match with the source file.
    Stable,
}

impl Module {
    /// Build a module from its source.
    pub fn build(&mut self) {
        if self.built {
            return;
        }
        match self.source {
            ModuleSource::PlainText(ref source_code) => {
                let mut checker = analyze_text(source_code);

                loop {
                    if let Some(statement_errors) = checker.next() {
                        for error in statement_errors {
                            self.errors.push(ProgramError::TypeError(error))
                        }
                    } else {
                        break;
                    }
                }
                for syntax_error in take(&mut checker.syntax_errors) {
                    self.errors.push(ProgramError::ParserError(syntax_error))
                }

                for lex_error in take(&mut checker.lexical_errors) {
                    self.errors.push(ProgramError::LexerError(lex_error))
                }
                self.scopes = take(checker.scope_manager());
                self.statements = take(&mut checker.statements);
                self.line_lens = take(&mut checker.parser.lexer.borrow_mut().line_lengths);
            }
            ModuleSource::FilePath { .. } => todo!(),
        }
    }

    /// Creates a module from Whirl source code text.
    /// It does not start parsing or analysis until [`Module::build()`] is called.
    pub fn from_text(value: String) -> Self {
        Module {
            name: String::new(),
            scopes: ScopeManager::new(),
            statements: vec![],
            errors: vec![],
            built: false,
            source: ModuleSource::PlainText(value),
            line_lens: vec![],
        }
    }

    /// Returns the errors found in the module.
    pub fn errors(&self) -> Iter<'_, ProgramError> {
        self.errors.iter()
    }

    /// Returns the statements parsed from the module.
    pub fn statements(&self) -> Iter<'_, Statement> {
        self.statements.iter()
    }

    /// Change parts of the module without rebuilding the entire representation.
    pub fn update(&mut self, changes: &[Change]) {
        // Create the representation if it does not already exist.
        if !self.built {
            self.build();
        }
        for change in changes {
            self.reconcile(change)
        }
    }

    pub fn refresh_with_text(&mut self, new_text: String) {
        self.errors.clear();
        self.scopes = ScopeManager::new();
        self.statements.clear();
        self.source = ModuleSource::PlainText(new_text);
        self.build();
    }

    /// Reconcile a text change with the module.
    /// TODO: The new statement generation breaks when the text is behind the original statement and overwrites a part of it.
    fn reconcile(&mut self, change: &Change) {
        let nodes: Vec<&Statement> = self
            .statements()
            .map(|s| s.closest_nodes_to(change.span))
            .flatten()
            .collect();

        let change_range = change.span.to_range(&self.line_lens);

        // TODO: Incrementally change.
        let parsing_bounds = if nodes.len() > 0 {
            let first_span = nodes[0].span();
            let last_span = nodes.last().unwrap().span();
            let full_span = first_span + last_span;
            let mut node_range = full_span.to_range(&self.line_lens);
            // Enlarge span to accomodate the changes.
            let original_length = change_range.end - change_range.start;
            let expected_length = change.new_text.len();
            if expected_length > original_length {
                node_range.end += expected_length - original_length;
            }
            Some(node_range)
        } else {
            None
        };

        match &self.source {
            ModuleSource::PlainText(plaintext) => {
                let mutation =
                    StringMutation::new(&change.new_text, change_range.start, change_range.end);
                let reviser = StringEditor::new(&plaintext, mutation, parsing_bounds);
                let lexer = TextLexer::from(reviser);
                let mut parser = Parser::from_lexer(lexer);

                // Remove invalidated scopes.
                let scope_ids = get_affected_scopes(nodes);
                for scope_id in scope_ids {
                    self.scopes.remove(scope_id);
                }

                let mut new_statements = vec![];
                loop {
                    match parser.next() {
                        Some(statement_partial) => new_statements.push(statement_partial),
                        None => break,
                    }
                }
                self.scopes.merge(parser.scope_manager());
                println!("{:#?}", self.scopes);

                // for statement in parser {
                //     parser.set_scope_manager()
                //     if statement.is_none() {
                //         continue;
                //     }
                //     let statement = statement.unwrap();

                // }
            }
            ModuleSource::FilePath { .. } => todo!(),
        };
    }
}

pub fn get_affected_scopes(nodes: Vec<&Statement>) -> Vec<usize> {
    let mut affected_scopes = vec![];
    for node in nodes {
        affected_scopes.append(&mut node.captured_scopes())
    }
    affected_scopes
}

#[cfg(test)]
mod tests {
    use whirl_ast::Change;

    use crate::Module;

    #[test]
    fn module_updating() {
        let mut module = Module::from_text(
            "
            function Main() {
              function Test () {
                square:= fn (a: Number): Number {
                    newValue := a ^ 2;
                    newValue
                };
                name: String = \"Hello\";
              }
            }
    "
            .to_string(),
        );

        module.update(&[
            // Change::new([2, 18, 2, 26], "SayMama"),
            Change::new([4, 18, 4, 24], "SquaringFunction "),
        ]);
    }
}
