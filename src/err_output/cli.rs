use colored::Colorize;
use pad::{Alignment, PadStr};
use crate::errors::{ErrorLevel, ErrorReporter, WjlError};
use crate::helpers::lineify;

pub fn output_errors_to_cli(reporter: ErrorReporter) {
    let lines = lineify(reporter.file);
    for err in reporter.errors {
        err.print(&reporter.file_name, &lines);
    }
}
fn fmt_line(line: &String, char_start: usize) -> String {
    return if line.len() < 250 {
        line.to_string()
    } else if (char_start + 1) < line.len() {
        line[0..=char_start].to_string()
    } else {
        line[0..=char_start + 1].to_string()
    };
}
impl WjlError {
    fn print(&self, file: &String, lines: &Vec<String>) {
        let level = if self.level == ErrorLevel::WARN {
            "warning".yellow()
        } else {
            "error".red()
        };
        let comp = if self.is_from_lex {
            "lexer"
        } else {
            "ast" //TODO! more levels here as we proceed
        };
        let (line_start, char_start, end) = self.location;
        let end = if self.end_char.is_some() {
            let (end_line, encd_char) = self.location.2.unwrap();
            format!(" -> {}:{}", end_line, encd_char)
        } else {"".to_string()};
        println!("(wjl/{}) {} in {}:{}:{}{}\n {}", comp.italic(), level, file.blue(), line_start, char_start, end, self.message.bold());
        println!();
        let line = &lines.get(line_start - 1).unwrap();
        let line = fmt_line(&line, char_start);
        let prev_line = lines.get(line_start - 2);
        let next_line = lines.get(line_start);
        if prev_line.is_some() {
            println!("[{}]: {}", line_start - 1, prev_line.unwrap());
        }
        let line_data = format!("[{}]: ", line_start);
        println!("{}{}", line_data, line.bold());
        if self.end_char.is_some() {
            let (end_line, end_char) = self.location.2.unwrap();
            if end_line == line_start {
                println!("{}", "^".repeat(end_char - char_start).pad_to_width_with_alignment(line.len() + line_data.len(), Alignment::Right).yellow())
            } else {
                todo!()
            }
        } else {
            println!("{}", "^ ".pad_to_width_with_alignment(line.len() + line_data.len(), Alignment::Right).yellow());
        }
        if next_line.is_some() {
            println!("[{}]: {}", line_start + 1, next_line.unwrap());
        }
        if !self.fixes.is_empty() {
            println!();
            println!("{}", "Potential fixes:".bold());
            for fix in &self.fixes {
                println!("\t- {}", fix.bold())
            }
        }
        if self.cause.is_some() {
            println!("{}", "Caused by:".bold());
            println!("{}", self.cause.as_ref().unwrap().yellow());
        }
    }
}