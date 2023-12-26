use crate::options::parse_cli;
use analyzer::Standpoint;
use help::print_help;
use options::CliCommand;
use std::{path::PathBuf, process::exit};
use utils::terminal::{self, Colored};

mod globals;
mod help;
mod options;

pub const VERSION: &'static str = "v0.0.0";

fn main() {
    let mut code = 0;
    let args = parse_cli();
    match args {
        Ok(options) => match &options.command {
            Some(CliCommand::Help) | None => print_help(),
            Some(CliCommand::Version) => {
                println!("Whirlwind version {VERSION} ({})", std::env::consts::OS)
            }
            Some(CliCommand::Run | CliCommand::Build | CliCommand::Eval | CliCommand::Test) => {
                manage(options, &mut code)
            }
            Some(CliCommand::Custom(string)) => {
                terminal::error(format!("Unknown command: \"{string}\""));
                terminal::inform("Format: whirlwind [command] [file.wrl] [arguments]");
                code = 1;
            }
            _ => todo!(),
        },
        Err(error) => {
            code = 1;
            terminal::error(error);
        }
    };
    exit(code);
}

fn manage(options: options::CliObject, code: &mut i32) {
    if let Some(corelibpath) = options.arguments.get("--CORELIBPATH") {
        let corelibpath = PathBuf::from(corelibpath);
        match corelibpath.canonicalize() {
            Ok(path) => {
                if !path.exists() {
                    terminal::error("core library path does not exist.");
                    *code = 1;
                } else if !path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| name == "core.wrl")
                {
                    terminal::error("Invalid core library file.");
                    *code = 1;
                } else {
                    let time = std::time::Instant::now();
                    let mut standpoint = Standpoint::new(true, Some(corelibpath));
                    standpoint.validate();
                    let elapsed = time.elapsed();
                    let mut message = Colored::new().green().bold();
                    message.text =
                        format!("   Built {} in {:?}.", standpoint.module_map.len(), elapsed);
                    println!("{message}");
                };
            }
            Err(error) => {
                terminal::error(format!("Invalid core library path: {error}"));
                *code = 1;
            }
        }
    } else {
        terminal::error("--CORELIBPATH argument not defined.");
        terminal::inform("Format: --CORELIBPATH=[path]");
        *code = 1;
    }
}
