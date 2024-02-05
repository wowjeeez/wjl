use crate::errors::ErrorReporter;
use crate::helpers::Print;
use crate::lexer::lex_stream;

mod errors;
mod iter;
mod lexer;
mod tokens;
#[macro_use]
mod helpers;
mod ast;

fn main() {
    let baseline_dep = include_str!("../baselines/str_val.wjl").to_string();
    let mut reporter = ErrorReporter::for_file("str_val.wjl".to_string(), &baseline_dep);
    let stream = lex_stream(&baseline_dep, &mut reporter);
    dbg!(reporter);
   // dbg!(&stream);
    stream.print();
}
