use analyzer::{DiagnosticType, ProgramDiagnostic, Standpoint};
use std::{
    fs::File,
    io::{self, Read, Seek},
    path::Path,
};
use utils::terminal::{Colorable, Colored};

/// Prints an error.
pub fn print_diagnostic_into(
    diagnostic: &ProgramDiagnostic,
    string: &mut String,
    span: ast::Span,
    path: &Path,
    line_lengths: &Vec<u32>,
) {
    let shift = " ".repeat(span.start[0].to_string().len() + 1);
    let tag = if diagnostic.is_error() {
        "error: ".color().red()
    } else {
        "warning: ".color().yellow()
    }
    .bold();
    let message = if diagnostic.is_error() {
        diagnostic.to_string().color().red()
    } else {
        diagnostic.to_string().color().yellow()
    };

    string.push_str(&format!(
        "{}{}{}\n",
        tag,
        path.display().to_string().color().cyan(),
        format!(":{}:{}", span.start[0], span.start[1])
            .color()
            .cyan(),
    ));
    if let Ok(text) = read_span_from_file(span, path, line_lengths) {
        // todo: multiline spans.
        if span.is_single_line() {
            string.push_str(
                &format!("{}|\n{} |> ", shift, span.start[0])
                    .color()
                    .bold()
                    .cyan()
                    .str(),
            );
            let whitespace_len = (text.len() - text.trim().len()) as u32;
            let text = pretty::highlight(text.trim());
            string.push_str(&text);

            string.push_str(&format!("\n{}|", shift).color().bold().cyan().str());
            let mut character_count = 0;
            while character_count < span.start[1] - whitespace_len + 2 {
                string.push(' ');
                character_count += 1
            }
            while character_count < (span.start[1] - whitespace_len + 2)
                || character_count < span.end[1] - whitespace_len + 2
            {
                let squiggle = "~".color().bold();
                string.push_str(
                    &if diagnostic.is_error() {
                        squiggle.red()
                    } else {
                        squiggle.yellow()
                    }
                    .str(),
                );
                character_count += 1
            }
            string.push_str(" ");
            string.push_str(&message.str());
            string.push_str(&format!("\n{}|", shift).color().bold().cyan().str());
        }
    }
}

fn read_span_from_file(
    span: ast::Span,
    path: &Path,
    line_lengths: &Vec<u32>,
) -> Result<String, io::Error> {
    let mut file = File::open(path.canonicalize()?)?;
    let range = span.from_line_start().to_range(line_lengths);
    let length = line_lengths[span.start[0] as usize - 1] as usize;
    let mut buffer = vec![0; length];
    file.seek(io::SeekFrom::Start(range.start as u64))?;
    file.read_exact(&mut buffer)?;
    Ok(unsafe { String::from_utf8_unchecked(buffer) })
}

/// Prints the diagnostics from the standpoint.
/// It returns a boolean indicating whether compilation can continue.
pub fn print_diagnostics(standpoint: &Standpoint) -> bool {
    let mut can_continue = true;

    let mut errors = 0;
    let mut warnings = 0;

    standpoint
        .diagnostics
        .iter()
        .for_each(|diagnostic| match &diagnostic._type {
            DiagnosticType::Error(_) => errors += 1,
            DiagnosticType::Warning(_) => warnings += 1,
        });

    if errors > 0 || warnings > 0 {
        let message = format!(
            "{}Found {} error{} and {} warning{}.",
            if errors > 0 { "Build failed. " } else { "" },
            errors,
            if errors == 1 { "" } else { "s" },
            warnings,
            if warnings == 1 { "" } else { "s" },
        );
        let mut colored = Colored::from(message).bold().cyan().underline();
        if errors > 0 {
            colored = colored.red();
        }
        println!("\n{colored}\n");
    }

    for (idx, diagnostic) in standpoint.diagnostics.iter().enumerate() {
        let mut string = String::new();
        let span = diagnostic.span();
        let typed_module = standpoint
            .module_map
            .get(diagnostic.offending_file)
            .unwrap();
        let path = &typed_module.path_buf;
        print_diagnostic_into(
            diagnostic,
            &mut string,
            span,
            &path,
            &typed_module.line_lengths,
        );
        println!("{string}");
        if diagnostic.is_error() {
            can_continue = false;
        }
        if idx == 29 {
            let note = "note: ".color().bold();
            let message = "Showing at most 30 diagnostics. To show all, run the command again with \"--SHOW-ALL-DIAGNOSTICS=true\".".color().italic();
            println!("\n{note}{message}\n");
            break;
        }
    }

    return can_continue;
}
