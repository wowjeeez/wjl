use std::fmt::Debug;
use crate::tokens::tokens::Token;

#[derive(Clone, Debug)]
pub struct Span<T: Clone + Debug> {
    inner: T,
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl Token {
    pub fn span(self, start: usize, end: usize) -> Span<Token> {
        Span {
            inner: self,
            start,
            end,
        }
    }

    pub fn span_zeroed(self) -> Span<Token> {
        self.span(0, 0)
    }
}

impl<T: Clone + Debug> Span<T> {
    pub fn get_inner(&self) -> T {
        self.inner.clone()
    }
}
