use std::{collections::HashMap, fmt::Display, path::PathBuf};

pub struct CliObject {
    pub command: Option<CliCommand>,
    pub file: Option<PathBuf>,
    pub arguments: HashMap<String, String>,
}

pub enum CliCommand {
    Run,
    Build,
    Check,
    Test,
    Eval,
    Help,
    Format,
    Version,
    Custom(String),
}

pub enum CliError {
    InvalidArgument(String),
    DuplicateOption(String),
    EntryFileNotSpecified,
}

impl Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            CliError::InvalidArgument(option) => format!("Unknown option: {}", option),
            CliError::DuplicateOption(option) => format!(
                "Duplicate option: {}. The option {} is defined more than once.",
                option, option
            ),
            CliError::EntryFileNotSpecified => format!("Entry file not specified."),
        };
        write!(f, "{message}",)
    }
}

/// Parses the command line arguments to setup Whirlwind.
pub fn parse_cli() -> Result<CliObject, CliError> {
    let mut args = std::env::args().skip(1);

    let mut arguments = HashMap::new();
    let mut file = None;
    let command = match args.next().as_ref().map(|a| a.as_str()) {
        Some("run") => CliCommand::Run,
        Some("check") => CliCommand::Check,
        Some("help" | "h") => CliCommand::Help,
        Some("version" | "v") => CliCommand::Version,
        Some("build") => CliCommand::Build,
        Some("format") => CliCommand::Format,
        Some("eval") => CliCommand::Eval,
        Some("test") => CliCommand::Test,
        Some(string) => CliCommand::Custom(string.to_string()),
        None => {
            return Ok(CliObject {
                command: None,
                file: None,
                arguments,
            })
        }
    };

    if matches!(
        command,
        CliCommand::Run
            | CliCommand::Build
            | CliCommand::Eval
            | CliCommand::Test
            | CliCommand::Check
    ) {
        let entry_file = args.next();
        if entry_file.is_none() {
            return Err(CliError::EntryFileNotSpecified);
        }
        let entry_file = entry_file.unwrap();
        let path = PathBuf::from(entry_file);
        file = Some(path);
    }

    for arg in args {
        let tuples = arg.split("=").collect::<Vec<_>>();
        if tuples.len() != 2 {
            return Err(CliError::InvalidArgument(arg));
        }
        let option = tuples[0];
        let value = tuples[1];
        if arguments.get(option).is_some() {
            return Err(CliError::DuplicateOption(arg));
        }
        arguments.insert(option.to_string(), value.to_string());
    }
    Ok(CliObject {
        command: Some(command),
        file,
        arguments,
    })
}
