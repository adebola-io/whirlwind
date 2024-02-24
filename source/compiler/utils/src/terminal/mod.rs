mod table;

use std::{
    fmt::Display,
    io::{Error, Write},
};
pub use table::{TerminalTable, TermnalTableEntry};

/// A colored string.
#[derive(Clone)]
pub struct Colored {
    pub text: String,
    codes: Vec<u8>,
}

/// A trait that specifies that a struct can be transformed to a colored string.
pub trait Colorable {
    fn color(&self) -> Colored;
}

impl Colored {
    pub fn new() -> Self {
        Self {
            text: String::new(),
            codes: vec![],
        }
    }
    pub fn from<T>(value: T) -> Self
    where
        T: Display,
    {
        Colored {
            text: value.to_string(),
            codes: vec![],
        }
    }
    pub fn apply(&self, codes: Vec<u8>) -> Self {
        let mut init_codes = self.codes.clone();
        for code in codes {
            init_codes.push(code);
        }
        Colored {
            text: self.text.to_owned(),
            codes: init_codes,
        }
    }
    pub fn bold(self) -> Self {
        add_format(self, 1)
    }
    pub fn italic(self) -> Self {
        add_format(self, 3)
    }
    pub fn underline(self) -> Self {
        add_format(self, 4)
    }
    pub fn black(self) -> Self {
        add_format(self, 30)
    }
    pub fn gray(self) -> Self {
        add_format(self, 90)
    }
    pub fn red(self) -> Self {
        add_format(self, 31)
    }
    pub fn green(self) -> Self {
        add_format(self, 32)
    }
    pub fn bright_yellow(self) -> Self {
        add_format(self, 93)
    }
    pub fn yellow(self) -> Self {
        add_format(self, 33)
    }
    pub fn bright_blue(self) -> Self {
        add_format(self, 94)
    }
    pub fn blue(self) -> Self {
        add_format(self, 34)
    }
    pub fn pink(self) -> Self {
        add_format(self, 35)
    }
    pub fn cyan(self) -> Self {
        add_format(self, 36)
    }
    pub fn white(self) -> Self {
        add_format(self, 37)
    }
    pub fn bg_black(self) -> Self {
        add_format(self, 40)
    }
    pub fn bg_red(self) -> Self {
        add_format(self, 41)
    }
    pub fn bg_green(self) -> Self {
        add_format(self, 42)
    }
    pub fn bg_yellow(self) -> Self {
        add_format(self, 43)
    }
    pub fn bg_blue(self) -> Self {
        add_format(self, 44)
    }
    pub fn bg_purple(self) -> Self {
        add_format(self, 45)
    }
    pub fn bg_cyan(self) -> Self {
        add_format(self, 46)
    }
    pub fn bg_white(self) -> Self {
        add_format(self, 47)
    }

    pub fn str(&self) -> String {
        self.to_string()
    }
}

fn add_format(mut text: Colored, code: u8) -> Colored {
    text.codes.push(code);
    text
}

impl Display for Colored {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut rendered = String::from("\x1b[");
        let mut i = 0;
        let code_length = self.codes.len();
        while i < code_length {
            rendered.push_str(&self.codes[i].to_string());
            if i < code_length - 1 {
                rendered.push(';');
            }
            i += 1;
        }
        write!(f, "{}m{}\x1b[0m", rendered, self.text)
    }
}

/// Logs a success message to the terminal.
pub fn success<T>(message: T)
where
    T: Display,
{
    println!("{}", Colored::from(message).green().bold())
}

/// Clear all text from the terminal.
pub fn clear() {
    print!("{}[2J{}[1;1H", 27 as char, 27 as char);
    std::io::stdout().flush().unwrap();
}

/// Log an error to the console.
pub fn error<T>(error: T)
where
    T: Display,
{
    eprintln!(
        "{} {}",
        Colored::from(" ERROR: ").bg_red().bold(),
        Colored::from(error.to_string()).red()
    )
}

/// Logs an error to the console and close the command.
pub fn fatal_error<T>(message: T) -> !
where
    T: Display,
{
    eprintln!("{}", Colored::from(message).red());
    std::process::exit(0);
}

/// Logs a warning to the console.
pub fn warn<T>(error: T)
where
    T: Display,
{
    println!(
        "{} {}",
        Colored::from(" WARNING: ").bg_yellow().black().bold(),
        Colored::from(error).yellow()
    )
}

pub fn inform<T>(message: T)
where
    T: Display,
{
    println!("{}", Colored::from(message).cyan())
}

impl Colorable for Error {
    fn color(&self) -> Colored {
        Colored::from(self.to_string())
    }
}

impl Colorable for String {
    fn color(&self) -> Colored {
        Colored::from(&self)
    }
}
impl Colorable for &str {
    fn color(&self) -> Colored {
        Colored::from(self)
    }
}
