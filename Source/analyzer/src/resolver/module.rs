use ast::{ModuleAmbience, Spannable, Statement, UseDeclaration};
use errors::{ImportError, LexError, ModuleError, ParseError};
use std::path::PathBuf;

use super::program::Program;

/// A completely parsed program.
/// This struct presents higher level operations for the frontend representation of source code.
#[derive(Debug, Default)]
pub struct Module {
    pub module_path: Option<PathBuf>,
    pub module_id: usize,
    pub name: Option<String>,
    pub(crate) _line_lens: Vec<u32>,
    pub(crate) statements: Vec<Statement>,
    pub(crate) lexical_errors: Vec<LexError>,
    pub(crate) syntax_errors: Vec<ParseError>,
    pub(crate) import_errors: Vec<ImportError>,
    pub ambience: ModuleAmbience,
    pub allow_prelude: bool,
}

impl Module {
    pub fn from_program(program: Program, module_id: usize, module_path: Option<PathBuf>) -> Self {
        Module {
            module_id,
            module_path,
            name: program.ambience().get_module_name().map(|x| x.to_string()),
            _line_lens: program.line_lengths,
            statements: program.statements,
            lexical_errors: program.lexical_errors,
            syntax_errors: program.syntax_errors,
            ambience: program.ambience.take(),
            allow_prelude: program.allow_prelude,
            import_errors: vec![],
        }
    }

    /// Creates a module from Whirlwind source code text.
    pub fn from_text(value: String) -> Self {
        let program = Program::from_text(&value);
        Module::from_program(program, 0, None)
    }

    /// Read a Whirlwind file and build a module from its contents.
    pub fn from_path(path: PathBuf, module_id: usize) -> Result<Self, ImportError> {
        match path.extension() {
            Some(ext) => {
                if ext != "wrl" {
                    return Err(errors::unknown_file_type(path));
                }
                // Todo: File::open blah blah blah
                let entry_source = match std::fs::read_to_string(&path) {
                    Ok(content) => content,
                    Err(error) => return Err(errors::error_reading_entry_file(error)),
                };
                let program = Program::from_text(&entry_source);
                Ok(Module::from_program(program, module_id, Some(path)))
            }
            None => return Err(errors::unknown_file_type(path)),
        }
    }

    /// Returns the errors found in the module.
    pub fn errors(&self) -> impl Iterator<Item = ModuleError> {
        self.lexical_errors
            .iter()
            .map(|error| ModuleError::LexerError(error))
            .chain(
                self.syntax_errors
                    .iter()
                    .map(|error| ModuleError::ParserError(error)),
            )
            .chain(
                self.import_errors
                    .iter()
                    .map(|error| ModuleError::ImportError(error)),
            )
    }

    /// Returns the statements parsed from the module.
    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.statements.iter()
    }

    pub fn refresh_with_text(&mut self, new_text: String) {
        // cursed (totally safe) code.
        *self = Module::from_program(
            Program::from_text(&new_text),
            self.module_id,
            self.module_path.take(),
        );
    }

    /// Returns an iterator over the imports in the module.
    pub fn get_use_imports(&self) -> impl Iterator<Item = &UseDeclaration> {
        get_use_imports_in_statement(&self.statements).into_iter()
    }
    /// Add resolution errors to the modules.
    pub fn set_resolve_errors(&mut self, errors: Option<Vec<ImportError>>) {
        if errors.is_none() {
            return;
        }
        self.import_errors = errors.unwrap();
    }

    pub fn empty() -> Module {
        Default::default()
    }

    // /// Change parts of the module without rebuilding the entire representation.
    // pub fn update(&mut self, changes: &[Change]) {
    //     // Create the representation if it does not already exist.
    //     if !self.built {
    //         self.build();
    //     }
    //     for change in changes {
    //         self.reconcile(change)
    //     }
    // }

    // Reconcile a text change with the module.
    // TODO: The new statement generation breaks when the text is behind the original statement and overwrites a part of it.
    // fn reconcile(&mut self, change: &Change) {
    //     let nodes: Vec<&Statement> = self
    //         .statements()
    //         .map(|s| s.closest_nodes_to(change.span))
    //         .flatten()
    //         .collect();

    //     let change_range = change.span.to_range(&self.line_lens);

    //     // TODO: Incrementally change.
    //     let parsing_bounds = if nodes.len() > 0 {
    //         let first_span = nodes[0].span();
    //         let last_span = nodes.last().unwrap().span();
    //         let full_span = first_span + last_span;
    //         let mut node_range = full_span.to_range(&self.line_lens);
    //         // Enlarge span to accomodate the changes.
    //         let original_length = change_range.end - change_range.start;
    //         let expected_length = change.new_text.len();
    //         if expected_length > original_length {
    //             node_range.end += expected_length - original_length;
    //         }
    //         Some(node_range)
    //     } else {
    //         None
    //     };

    //     match source {
    //         ModuleSource::PlainText(plaintext) => {
    //             let mutation =
    //                 StringMutation::new(&change.new_text, change_range.start, change_range.end);
    //             let reviser = StringEditor::new(&plaintext, mutation, parsing_bounds);
    //             let lexer = TextLexer::from(reviser);
    //             let mut parser = Parser::from_lexer(lexer);

    //             // Remove invalidated scopes.
    //             let scope_ids = get_affected_scopes(nodes);
    //             for scope_id in scope_ids {
    //                 self.scopes.remove(scope_id);
    //             }

    //             let mut new_statements = vec![];
    //             loop {
    //                 match parser.next() {
    //                     Some(statement_partial) => new_statements.push(statement_partial),
    //                     None => break,
    //                 }
    //             }
    //             self.scopes.merge(parser.module_ambience());
    //             println!("{:#?}", self.scopes);

    //             // for statement in parser {
    //             //     parser.set_module_ambience()
    //             //     if statement.is_none() {
    //             //         continue;
    //             //     }
    //             //     let statement = statement.unwrap();

    //             // }
    //         }
    //         ModuleSource::FilePath { .. } => todo!(),
    //     };
    // }
}

fn get_use_imports_in_statement(statements: &[Statement]) -> Vec<&UseDeclaration> {
    let mut use_imports = vec![];
    statements.iter().for_each(|statement| match statement {
        Statement::UseDeclaration(usedecl) => use_imports.push(usedecl),
        Statement::TestDeclaration(test) => {
            use_imports.append(&mut get_use_imports_in_statement(&test.body.statements))
        }
        _ => {}
    });
    use_imports
}

pub fn _get_affected_scopes(nodes: Vec<&Statement>) -> Vec<usize> {
    let mut affected_scopes = vec![];
    for node in nodes {
        affected_scopes.append(&mut node.captured_scopes())
    }
    affected_scopes
}

// #[cfg(test)]
// mod tests {
//     use std::path::PathBuf;

// }
