use common::tokio;
use lexer::lex_string;

#[tokio::main]
async fn main() {
    let lexed_numeric = lex_string("var test = myVar + 15.3 * (helpMe - 10)".to_string()).await;
    dbg!(lexed_numeric);
    let lexed_text = lex_string("var test = 'Hello there $username'.join(\"Its very nice to meet you\")".to_string()).await;
    dbg!(lexed_text);
}