use crate::errors::ErrorReporter;
use crate::lexer::lex_stream;

mod lexer;
mod iter;
mod tokens;
mod errors;

fn main() {
    let baseline_dep = include_str!("../baselines/str_val.wjl").to_string();
    let mut reporter = ErrorReporter::for_file("str_val.wjl".to_string(), &baseline_dep);
    let stream = lex_stream(&baseline_dep, &mut reporter);
    dbg!(reporter);
    dbg!(stream);
}