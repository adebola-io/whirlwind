use std::fmt::Display;

use crate::terminal::Colored;

pub type TermnalTableEntry = (String, Vec<String>);
pub struct TerminalTable {
    heading: String,
    sidebar: Option<Colored>,
    rows: Vec<TermnalTableEntry>,
}

impl TerminalTable {
    /// Creates a new Terminal table.
    pub fn new<T: Display>(heading: T) -> Self {
        TerminalTable {
            heading: heading.to_string(),
            sidebar: None,
            rows: vec![],
        }
    }
    /// Add a new row to the table. The tag is the leftmost field in the row.
    pub fn row(&mut self, tag: &str, data: Vec<&str>) -> &mut Self {
        self.rows.push((
            tag.to_string(),
            data.into_iter().map(|str| str.to_string()).collect(),
        ));
        return self;
    }
    /// Add a line demarcation to the front of each row.
    pub fn sideline(&mut self, sidebar: &Colored) -> &mut Self {
        self.sidebar = Some(sidebar.clone());
        return self;
    }

    /// Prints the sidebar into the string.
    fn print_sidebar(&self, string: &mut String) {
        if let Some(sidebar) = &self.sidebar {
            string.push_str(&sidebar.to_string());
            string.push_str(" ");
        }
    }
}

impl Display for TerminalTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();
        self.print_sidebar(&mut string);
        let heading = Colored::from(&self.heading).underline().bold();
        string.push_str(&heading.to_string());
        string.push_str("\n");

        let longest_tag_len = self
            .rows
            .iter()
            .map(|(tag, _)| tag.len())
            .reduce(|acc, e| if acc < e { e } else { acc })
            .unwrap();

        for (idx, (tag, data)) in self.rows.iter().enumerate() {
            self.print_sidebar(&mut string);
            string.push_str("   ");
            string.push_str(tag);
            let mut i = tag.len();
            while i < longest_tag_len {
                string.push(' ');
                i += 1;
            }
            if data.len() == 0 {
                string.push_str("\n");
                continue;
            }
            let data_fields = data.len() as f64;
            let space = (1 as f64 / data_fields).ceil() as i64 * 4;
            for _ in 0..space {
                string.push(' ');
            }
            for data in data {
                string.push_str(data);
                for _ in 0..space {
                    string.push(' ');
                }
            }
            if idx + 1 != self.rows.len() {
                string.push_str("\n");
            }
        }
        write!(f, "{string}")
    }
}
