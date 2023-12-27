use crate::options::parse_cli;
use analyzer::{Module, Standpoint};
use diagnostics::print_diagnostics;
use help::print_help;
use options::CliCommand;
use std::{path::PathBuf, process::exit};
use utils::terminal;

mod diagnostics;
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
                println!("Whirlwind Version {VERSION} ({})", std::env::consts::OS)
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
            terminal::inform("Format: whirlwind [command] [file.wrl] [arguments]");
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
                    // ---
                    let mut standpoint = Standpoint::new(true, Some(corelibpath));

                    let entry_path = options.file.unwrap();
                    match Module::from_path(entry_path) {
                        Ok(module) => {
                            match standpoint.add_module(module) {
                                Some(pathidx) => standpoint.entry_module = pathidx,
                                None => terminal::error(
                                    "Could not add module to standpoint. Something went wrong.",
                                ),
                            };
                        }
                        Err(error) => {
                            terminal::error(error._type);
                            *code = 1;
                            return;
                        }
                    };

                    standpoint.validate();
                    // First boundary.
                    if standpoint.diagnostics.len() > 0 {
                        standpoint.validate(); // idk
                        print_diagnostics(&standpoint);
                        if standpoint
                            .diagnostics
                            .iter()
                            .any(|diagnostic| diagnostic.is_error())
                        {
                            *code = 1;
                            return;
                        }
                    }

                    let object = bytecode::generate_from(&standpoint);
                    let object = match object {
                        Ok(object) => object,
                        Err(error) => {
                            terminal::error(format!("{error}"));
                            *code = 1;
                            return;
                        }
                    };

                    let code = bytecode::serialize_object(object);
                    let mut vm = runtime::VM::new();
                    vm.instructions = code;

                    // vm
                    // --
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
