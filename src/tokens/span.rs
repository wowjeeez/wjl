use crate::tokens::tokens::Token;

#[derive(Clone, Debug)]
pub struct Span {
    token: Token,
    pub(crate) start: usize,
    pub(crate) end: usize
}

impl Token {
    pub fn span(self, start: usize, end: usize) -> Span {
        Span {
            token: self,
            start,
            end
        }
    }
}

impl Span {
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
}