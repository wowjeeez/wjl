use crate::lexer::{TOKEN, TokenSpan};

pub fn assert_stream(input: Vec<TokenSpan>, against: Vec<TOKEN>) {
    let tokens = input.into_iter().map(|x|x.token).collect::<Vec<TOKEN>>();
    assert_eq!(tokens, against)
}