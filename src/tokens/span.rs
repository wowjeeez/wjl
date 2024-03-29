use crate::tokens::tokens::Token;
use crate::tokens::IdentKind;
use std::fmt::Debug;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
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
            _ => false,
        }
    }

    pub fn get_ident_inner(&self) -> (String, bool) {
        match self {
            Token::IDENT(inner) => match inner {
                IdentKind::DEFAULT(inner) => (inner.clone(), false),
                IdentKind::BACKTICK(inner) => (inner.clone(), true),
            },
            _ => unreachable!(),
        }
    }

    pub fn is_static(&self) -> bool {
        match self {
            Token::LITERAL_SINGLE(_) | Token::LITERAL_DOUBLE(_) => true,
            Token::HEX_NUMBER(_, _)
            | Token::BINARY_NUMBER(_, _)
            | Token::OCTAL_NUMBER(_, _)
            | Token::INT(_, _)
            | Token::FLOAT(_) => true,
            Token::KEYWORD_TRUE | Token::KEYWORD_FALSE | Token::KEYWORD_NULL => true,
            _ => false,
        }
    }
}

impl<T: Clone + Debug> Span<T> {
    pub fn get_inner(&self) -> T {
        self.inner.clone()
    }
    pub fn get_inner_ref(&self) -> &T {
        &self.inner
    }
    pub fn wrap(start: usize, end: usize, with: T) -> Span<T> {
        Span {
            inner: with,
            start,
            end,
        }
    }
    pub fn map<R: Clone + Debug, F: FnOnce(T) -> R>(self, func: F) -> Span<R> {
        let wrapped = func(self.get_inner());
        Span::wrap(self.start, self.end, wrapped)
    }
}
pub trait IntoSpan<T: Clone + Debug> {
    fn to_span(&self, start: usize, end: usize) -> Span<T>;
    fn into_span(self, start: usize, end: usize) -> Span<T>;
}

impl<T> IntoSpan<T> for T
where
    T: Clone + Debug,
{
    fn to_span(&self, start: usize, end: usize) -> Span<T> {
        Span::wrap(start, end, self.clone())
    }
    fn into_span(self, start: usize, end: usize) -> Span<T> {
        Span::wrap(start, end, self)
    }
}
