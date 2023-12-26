use std::{collections::HashMap, fmt::Display};

pub struct CliObject {
    pub command: Option<CliCommand>,
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
}

impl Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CliError::InvalidArgument(option) => format!("Unknown option: {}", option),
                CliError::DuplicateOption(option) => format!(
                    "Duplicate option: {}. The option {} is defined more than once.",
                    option, option
                ),
            }
        )
    }
}

/// Parses the command line arguments to setup Whirlwind.
pub fn parse_cli() -> Result<CliObject, CliError> {
    let mut args = std::env::args().skip(1);

    let mut arguments = HashMap::new();
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
                arguments,
            })
        }
    };

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
        arguments,
    })
}
