use lexer::lex_string;

fn main() {
    let lexed_numeric = lex_string("var test = myVar + 15 * (helpMe - 10)".to_string());
    dbg!(lexed_numeric);
    let lexed_text = lex_string("var test = 'Hello there $username'.join(\"Its very nice to meet you\")".to_string());
    dbg!(lexed_text);
}
