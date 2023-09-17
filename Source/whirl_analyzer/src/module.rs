use crate::type_check_text;
use std::{mem::take, path::PathBuf, slice::Iter};
use whirl_ast::{Change, ScopeManager, Statement};
use whirl_errors::ProgramError;

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
                let mut checker = type_check_text(source_code);
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

    /// Reconcile a text change with the module.
    fn reconcile(&mut self, change: &Change) {
        for statement in self.statements() {
            // let affected_statements = statement.closest_nodes_to(change.span);
            // for affected_statement in affected_statements {
            match self.source {
                ModuleSource::FilePath { .. } => todo!(),
                ModuleSource::PlainText(ref plaintext) => {
                    // Creates a new revised text struct for it

                    // let revision = RevisedText::from(change);
                }
            }
            // }
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn substr() {
        println!("{:?}", "hello from the outside".get(0..8))
    }
}
