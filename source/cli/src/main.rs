use crate::options::parse_cli;
use analyzer::{Module, Standpoint};
use diagnostics::print_diagnostics;
use help::print_help;
use options::CliCommand;
use std::{path::PathBuf, process::exit};
use utils::terminal::{self, Colorable};

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
            Some(
                CliCommand::Run
                | CliCommand::Build
                | CliCommand::Eval
                | CliCommand::Test
                | CliCommand::Check,
            ) => manage(options, &mut code),
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
            Ok(path) => exec(path, code, corelibpath, options),
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

fn exec(path: PathBuf, code: &mut i32, corelibpath: PathBuf, options: options::CliObject) {
    if !path.exists() {
        terminal::error("core library path does not exist.");
        *code = 1;
        return;
    }

    if !path
        .file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name == "core.wrl")
    {
        terminal::error("Invalid core library file.");
        *code = 1;
        return;
    }

    // ---
    let entry_path = match options.file {
        Some(file) => file,
        None => {
            terminal::error("Entry file not specified.");
            *code = 1;
            return;
        }
    };
    terminal::clear();
    println!(
        "{} {}",
        "Building:".color().cyan().bold().underline(),
        entry_path.display()
    );
    let time = std::time::Instant::now();
    let mut standpoint = Standpoint::new(true, Some(corelibpath));
    match Module::from_path(entry_path) {
        Ok(module) => {
            match standpoint.add_module(module) {
                Some(pathidx) => standpoint.entry_module = pathidx,
                None => {
                    terminal::error("Could not add module to standpoint. Something went wrong.")
                }
            };
        }
        Err(error) => {
            terminal::error(error._type);
            *code = 1;
            return;
        }
    };
    standpoint.validate();
    if standpoint.diagnostics.len() > 0 {
        standpoint.validate(); // see idk
    }
    let elapsed = time.elapsed();

    print_diagnostics(&standpoint, &options.arguments);
    if standpoint
        .diagnostics
        .iter()
        .any(|diagnostic| diagnostic.is_error())
    {
        *code = 1;
        return;
    }

    let checking_finished = format!("Checking finished in {elapsed:?}.\n")
        .color()
        .green();
    println!("{checking_finished}",);

    // Break if checking.
    if matches!(options.command, Some(options::CliCommand::Check)) {
        *code = 0;
        return;
    }

    // Build the standpoint.
    let build = codegen::generate_wasm_from_whirlwind_standpoint(&standpoint);
    if build.is_err() {
        terminal::error(build.err().unwrap().to_string());
        *code = 1;
        return;
    }

    // Write to disk
    if matches!(options.command, Some(options::CliCommand::Build)) {
        // ...
        let elapsed = time.elapsed();
        let build = format!("Build finished in {elapsed:?}.\n").color().green();
        println!("{build}",);
        *code = 0;
        return;
    }

    // Run the standpoint.
}
