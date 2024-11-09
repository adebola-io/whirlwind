use utils::terminal::{Colored, TerminalTable};

pub fn print_help() {
    let handle = Colored::from("|-").cyan();
    let welcome = Colored::from("🥏 Whirlwind.").cyan().bold();
    let sideline = Colored::from("|>").cyan();
    println!("\n{handle}{welcome}");
    println!("{sideline} Whirlwind is a programming language for creating cool applications.");
    blankline();
    println!(
        "{sideline} Usage: {} [command] [file.wrl] [arguments]",
        Colored::from("whirlwind").italic()
    );

    blankline();

    let mut commands = TerminalTable::new("Commands:");
    commands
        .sideline(&sideline)
        .row("build (disabled)", vec!["Build a file to a WASM object."])
        .row("check", vec!["Checks file for syntax and semantic errors."])
        .row("eval (disabled)", vec!["Starts a new repl."])
        .row("format (disabled)", vec!["Formats a file."])
        .row("h, help", vec!["Displays this message."])
        .row("run (disabled)", vec!["Builds and runs a file."])
        .row("test (disabled)", vec!["Runs the test blocks in the file."])
        .row("v, version", vec!["Prints version of runtime installed."]);
    println!("{commands}");

    blankline();

    let mut args = TerminalTable::new("Arguments:");
    args.sideline(&sideline)
        .row("--CORELIBPATH=[path]", vec!["Path to Core Library folder."])
        .row("--OUTDIR=[path]", vec!["Output path for the WASM build."])
        .row(
            "--NO-WARNING=[true|false]",
            vec!["Disables warning messages."],
        )
        .row(
            "--NO-SOURCE-LOOKUP=[true|false]",
            vec!["Disables error and warning lookup."],
        )
        .row(
            "--SHOW-ALL-DIAGNOSTICS=[true|false]",
            vec!["Shows all diagnostics rather than a max of 30."],
        );

    println!("{args}");
}

fn blankline() {
    println!("{}", Colored::from("|").cyan());
}
