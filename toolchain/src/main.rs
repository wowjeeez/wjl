use lexer::lex_string;

fn main() {
    let lexed = lex_string("var test = myVar + 15 * (helpMe - 10)".to_string());
    dbg!(lexed);
}
