use crate::lexer::tokenize;
use crate::parser::dbg_print::dbg_vec;
use crate::parser::parse_token_stream;
mod lexer;
mod parser;
mod iter_util;
mod utils;

fn main() {
    let baseline_dep = include_str!("../baselines/deps.wjl").to_string();
    let stream = tokenize("return@test".to_string()).unwrap();
    dbg!(&stream);
    let parsed = parse_token_stream(stream);
    if parsed.is_err() {
        dbg!(parsed);
    } else {
        println!("{:?}", dbg_vec(parsed.unwrap()));
    }
}