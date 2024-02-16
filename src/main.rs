use crate::ast::ToAst;
use crate::err_output::output_errors_to_cli;
use crate::errors::ErrorReporter;
use crate::helpers::Print;
use crate::lexer::{lex_stream};

mod errors;
mod iter;
mod lexer;
mod tokens;
#[macro_use]
mod helpers;
mod ast;
mod caching;
mod err_output;

fn main() {
    tracing_subscriber::fmt::init();
    let baseline_dep = include_str!("../baselines/isolated/template_str.wjl").to_string();
    let mut reporter = ErrorReporter::for_file("str_val.wjl".to_string(), &baseline_dep);
    let stream = lex_stream(&baseline_dep, &mut reporter);
    //stream.print();
    let ast = stream.into_ast(&mut reporter);
    if !reporter.errors.is_empty() {
        output_errors_to_cli(reporter)
    } else {
      dbg!(ast);
    }

    //dbg!(&stream);
}
