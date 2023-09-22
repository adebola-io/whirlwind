use crate::analyze_text;
use std::path::PathBuf;
use whirl_ast::{ModuleAmbience, Spannable, Statement};
use whirl_errors::{LexError, ParseError, ProgramError, TypeError};

/// A completely parsed program.
/// This struct presents higher level operations for the frontend representation of source code.
#[derive(Debug)]
pub struct Module {
    pub name: Option<String>,
    _line_lens: Vec<u32>,
    statements: Vec<Statement>,
    lexical_errors: Vec<LexError>,
    syntax_errors: Vec<ParseError>,
    type_errors: Vec<TypeError>,
    pub ambience: ModuleAmbience,
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
    pub fn build(&mut self, source: ModuleSource) {
        match source {
            ModuleSource::PlainText(source_code) => {
                let mut inferrer = analyze_text(&source_code);
                inferrer.infer();
                // cursed (totally safe) code.
                *self = Module {
                    name: inferrer.ambience().get_module_name().map(|x| x.to_string()),
                    _line_lens: inferrer.line_lengths,
                    statements: inferrer.statements,
                    lexical_errors: inferrer.lexical_errors,
                    syntax_errors: inferrer.syntax_errors,
                    type_errors: inferrer.type_errors.take(),
                    ambience: inferrer.ambience.take(),
                };
            }
            ModuleSource::FilePath { .. } => todo!(),
        }
    }

    /// Creates a module from Whirl source code text.
    pub fn from_text(value: String) -> Self {
        let mut inferrer = analyze_text(&value);
        inferrer.infer();
        Module {
            name: inferrer.ambience().get_module_name().map(|x| x.to_string()),
            _line_lens: inferrer.line_lengths,
            statements: inferrer.statements,
            lexical_errors: inferrer.lexical_errors,
            syntax_errors: inferrer.syntax_errors,
            type_errors: inferrer.type_errors.take(),
            ambience: inferrer.ambience.take(),
        }
    }

    /// Returns the errors found in the module.
    pub fn errors(&self) -> impl Iterator<Item = ProgramError> {
        self.lexical_errors
            .iter()
            .map(|error| ProgramError::LexerError(error))
            .chain(
                self.syntax_errors
                    .iter()
                    .map(|error| ProgramError::ParserError(error)),
            )
            .chain(
                self.type_errors
                    .iter()
                    .map(|error| ProgramError::TypeError(error)),
            )
    }

    /// Returns the statements parsed from the module.
    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.statements.iter()
    }

    pub fn refresh_with_text(&mut self, new_text: String) {
        self.build(ModuleSource::PlainText(new_text));
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

pub fn _get_affected_scopes(nodes: Vec<&Statement>) -> Vec<usize> {
    let mut affected_scopes = vec![];
    for node in nodes {
        affected_scopes.append(&mut node.captured_scopes())
    }
    affected_scopes
}
