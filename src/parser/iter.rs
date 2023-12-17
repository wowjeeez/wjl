use crate::iter_util::CommonIter;
use crate::lexer::{TOKEN, TokenSpan};

#[derive(Clone, Debug)]
pub struct TokenParserIter {
    inner: Vec<TokenSpan>,
    index: Option<usize>
}


impl CommonIter for TokenParserIter {
    fn next(&mut self) -> Option<TokenSpan> {
        self.with_mline_comment_skip();
        let i = self.index.map_or(0, |x| x +1);
        self.index = Some(i);
        self.inner.get(i).map(|x|x.clone())
    }
    fn peek(&self) -> Option<TokenSpan> {
        self.peek_i_n(1)
    }
}


impl TokenParserIter {
    pub fn wrap(stream: Vec<TokenSpan>) -> TokenParserIter {
        TokenParserIter {
            inner: stream,
            index: None
        }
    }

    pub fn next_skip_whitespace(&mut self) -> Option<TokenSpan> {
        let next = self.next();
        if next.is_none() {
            return None
        }
        let next = next.unwrap();
        if next.token == TOKEN::WHITESPACE {
            return self.next_skip_whitespace()
        }
        Some(next)
    }


    fn peek_i_n(&self, n: isize) -> Option<TokenSpan> {
        let index = self.index.map_or(0 + n, |x| x as isize + n) as usize;
        self.inner.get(index).map(|x|x.clone())
    }

    fn peek_i_n_skip_whitespace(&self, n: isize) -> Option<TokenSpan> {
        let index = self.index.map_or(0 + n, |x| x as isize + n) as usize;
        let span = self.inner.get(index);
        if span.is_none() {
            return None
        }
        let span = span.unwrap();
        if span.token == TOKEN::WHITESPACE {
            return self.peek_i_n_skip_whitespace(if n.is_negative() {n - 1} else {n + 1})
        }
        return Some(span.clone())
    }

    pub fn peek_skip_whitespace(&self) -> Option<TokenSpan> {
        self.peek_i_n_skip_whitespace(1)
    }

    pub fn peek_prev_skip_whitespace(&self) -> Option<TokenSpan> {
        self.peek_i_n_skip_whitespace(-1)
    }

    pub fn peek(&self) -> Option<TokenSpan> {
        self.peek_i_n(1)
    }
    pub fn peek_prev(&self) -> Option<TokenSpan> {
        self.peek_i_n(-1)
    }
    pub fn peek_prev_n(&self, n: usize) -> Option<TokenSpan> {
        self.peek_i_n((n as isize) * -1)
    }

    pub fn peek_prev_n_skip_whitespace(&self, n: usize) -> Option<TokenSpan> {
        self.peek_i_n_skip_whitespace((n as isize) * -1)
    }

    pub fn peek_n_skip_whitespace(&self, n: usize) -> Option<TokenSpan> {
        self.peek_i_n_skip_whitespace(n as isize)
    }
    pub fn peek_n(&self, n: usize) -> Option<TokenSpan> {
        self.peek_i_n(n as isize)
    }

    pub fn lookbehind_peek_iter(&self) -> BackwardsPeekingIterator {
        BackwardsPeekingIterator::wrap(self.inner.clone(), self.index.unwrap_or(0))
    }

    pub fn pos(&self) -> usize {
        self.index.unwrap_or(0)
    }

    // will slice the wrapped vec from the current index to its end
    pub fn remaining_inclusive(&self) -> Vec<TokenSpan> {
        self.inner[self.index.unwrap_or(0)..].to_vec()
    }

    pub fn seek(&mut self, index: isize) {
        self.index = Some(self.index.map_or(index.abs() as usize, |x| ((x as isize) + index) as usize));
    }

    pub fn contains_ahead(&self, contains: TOKEN) -> bool {
        let mut cloned = self.clone();
        while let Some(span) = cloned.next() {
            if span.token == contains {
                return true
            }
        }
        return false
    }
}

pub struct BackwardsPeekingIterator {
    stream: Vec<TokenSpan>,
    index: usize
}

impl BackwardsPeekingIterator {
    pub fn wrap(vec: Vec<TokenSpan>, index: usize) -> BackwardsPeekingIterator {
        BackwardsPeekingIterator {
            stream: vec,
            index
        }
    }

    pub fn next(&mut self) -> Option<TokenSpan> {
        let next_i: isize = if self.index == 0 {
            -1
        } else {
            (self.index - 1) as isize
        };
        self.index = next_i as usize;
        return if next_i == -1 {
            None
        } else {
            self.stream.get(next_i as usize).map(|x|x.clone())
        }
    }
    pub fn next_skip_whitespace(&mut self) -> Option<TokenSpan> {
        let n = self.next();
        if n.is_none() {
            return None
        }
        let n = n.unwrap();
        if n.token == TOKEN::WHITESPACE {
            return self.next_skip_whitespace()
        }
        return Some(n.clone())
    }

    pub fn set_index(&mut self, to: usize) {
        self.index = to
    }

    pub fn get_index(&self) -> usize {self.index}
}

