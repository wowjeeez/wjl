use common::tokio;
use lexer::lex_string;
use lexer::token::Token;
use parser::to_parse_tree;

#[tokio::main]
async fn main() {
    let lexed_text = lex_string("function tgt::lib.test<A>(i: int): A {return i.str()} ".to_string());
    let lexed_text = lexed_text.into_iter().filter(|x| x.get() != &Token::WHITESPACE_MERGED).collect();
    let parse_tree = to_parse_tree(lexed_text).expect("err");
}