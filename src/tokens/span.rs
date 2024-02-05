use std::fmt::Debug;
use crate::tokens::IdentKind;
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

    pub fn is_ident(&self) -> bool {
        match self {
            Token::IDENT(_) => true,
            _ => false
        }
    }

    pub fn get_ident_inner(&self) -> (String, bool) {
        match self {
            Token::IDENT(inner) => match inner {
                IdentKind::DEFAULT(inner) => (inner.clone(), false),
                IdentKind::BACKTICK(inner) => (inner.clone(), true),
            },
            _ => unreachable!()
        }
    }
}

impl<T: Clone + Debug> Span<T> {
    pub fn get_inner(&self) -> T {
        self.inner.clone()
    }
}
