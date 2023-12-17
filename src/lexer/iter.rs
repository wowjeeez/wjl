use std::fmt::Write;
use crate::iter_util::CommonIter;
use crate::lexer::error::LexError;
use crate::lexer::span::TokenSpan;
use crate::lexer::tokens::TOKEN;

pub struct CharIterator {
    inner: Vec<char>,
    index: Option<usize>
}

impl CharIterator {
    pub fn wrap_string(str: String) -> CharIterator {
        CharIterator {
            index: None,
            inner: str.chars().collect(),
        }
    }

    pub fn next(&mut self) -> Option<char> {
        self.index = Some(self.index.map_or(0, |x| x + 1));
        return self.inner.get(self.index.unwrap()).map(|x| x.clone())
    }

    pub fn peek_next(&self) -> Option<char> {
        let next_index = self.index.unwrap_or(0) + 1;
        self.inner.get(next_index).map(|x| x.clone())
    }

    pub fn peek_n(&self, n: usize) -> Option<char> {
        let next_index = self.index.unwrap_or(0) + n;
        self.inner.get(next_index).map(|x| x.clone())
    }

    pub fn peek_prev(&self) -> Option<char> {
        let prev_index = self.index;
        if self.index.is_none() {
            return None
        }
        let prev_index = prev_index.unwrap();
        if prev_index == 0 {
            return None
        }
        self.inner.get(prev_index -1).map(|x| x.clone())
    }

    pub fn eat_until_next_ignore_escaped(&mut self, until: char) -> String {
        let mut str = String::new();
        while let Some(char) = self.next() {
            let prev = self.peek_prev();
            let is_escape = prev.is_some() && prev.unwrap() == '\\';
            if char == until && !is_escape {break}
            str.write_char(char).unwrap();
        }
        str
    }

    pub fn peek_until_whitespace_or_angle(&mut self) -> (String, usize) {
        let mut str = String::new();
        let mut n = 0;
        while let Some(char) = self.peek_n(n) {
            if char.is_whitespace() || char == '>' || char == '<' {break}
            str.write_char(char).unwrap();
            n += 1;
        }
        (str, n)
    }

    pub fn eat_until_newline(&mut self) -> String {
        let mut str = String::new();
        while let Some(char) = self.next() {
            if char == '\n' {break}
            str.write_char(char).unwrap();
        }
        str
    }

    pub fn eat_until<F>(&mut self, until: F) -> String
        where F:
        Fn(char, Option<char>) -> bool
    {
        let mut str = String::new();
        while let Some(char) = self.next() {
            if until(char, self.peek_next()) {break}
            str.write_char(char).unwrap();
        }
        str
    }

    pub fn seek(&mut self, by: usize) {
        self.index = Some(self.index.map_or(by, |x| x + by))
    }

    pub fn current_index(&self) -> usize {
        self.index.unwrap_or(0)
    }

}

pub struct TokenIter {
    tokens: Vec<TokenSpan>,
    index: Option<usize>
}

impl CommonIter for TokenIter {
    fn next(&mut self) -> Option<TokenSpan> {
        self.with_mline_comment_skip();
        self.index = Some(self.index.map_or(0, |x| x + 1));
        self.tokens.get(self.index.unwrap()).map(|x| x.clone())
    }

    fn peek(&self) -> Option<TokenSpan> {
        let next = self.index? + 1;
        self.tokens.get(next).map(|x| x.clone())
    }
}

impl TokenIter {
    pub fn wrap(tokens: Vec<TokenSpan>) -> TokenIter {
        TokenIter {
            tokens,
            index: None
        }
    }


    pub fn peek_prev(&self) -> Option<TokenSpan> {
        let curr = self.index?;
        if curr == 0 {
            return None
        }
        self.tokens.get(curr - 1).map(|x| x.clone())
    }

    pub fn seek(&mut self, n: isize) {
        self.index = Some(((self.index.unwrap_or(0) as isize) + n) as usize)
    }

    pub fn collect_and_eat<F>(&mut self, collector: F) -> Result<Vec<TokenSpan>, LexError>
        where F:
        Fn(TokenSpan, Option<TokenSpan>, Option<TokenSpan>) -> Result<Option<(bool, usize)>, LexError>
    {
        let mut stream: Vec<TokenSpan> = vec![];
        while let Some(span) = self.next() {
            let next = self.peek();
            let prev = self.peek_prev();
            let result = collector(span.clone(), next, prev)?;
            if result.is_none() {
                self.index = Some(self.index.unwrap_or(1) - 1); //preserve last token
                break
            }
            let (keep, seek) = result.unwrap();
            if keep {
                stream.push(span)
            }
            self.seek(seek as isize)
        }
        return Ok(stream)
    }

    pub fn peek_n(&self, n: usize) -> Option<TokenSpan> {
        let next = self.index? + n;
        self.tokens.get(next).map(|x| x.clone())
    }
    //will just parse a codeblock until the nearest matchable closing }
    pub fn parse_brace_block(&mut self, incl: bool) -> Option<TokenSpan> {
        let start = self.index.unwrap_or(0);
        if incl {
            self.next(); //drop }
        }
        let mut tokens: Vec<TokenSpan> = vec![];
        while let Some(span) = self.next() {
            if span.token == TOKEN::RIGHT_BRACE {
                //end of block
                let last = tokens.last()?;
                let end = last.end;
                return Some(TOKEN::S2_ARBIT_BLOCK(tokens).into_span(start, end))
            }
            if span.token == TOKEN::LEFT_BRACE {
                let nested = self.parse_brace_block(false);
                if nested.is_none() {
                    continue
                }
                let nested = nested.unwrap();
                let tok = TOKEN::S2_ARBIT_BLOCK(nested.get_codeblock_content()).into_span(nested.start, nested.end);
                tokens.push(tok);
                continue
            }

            tokens.push(span)
        }
        return None
    }

    pub fn cursor_pos(&self) -> usize {
        self.index.unwrap_or(0)
    }
}