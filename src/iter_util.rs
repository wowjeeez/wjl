use crate::lexer::{TOKEN, TokenSpan};

pub trait CommonIter {
    fn next(&mut self) -> Option<TokenSpan>;
    fn peek(&self) -> Option<TokenSpan>;
    fn with_mline_comment_skip(&mut self) {
        let peeked = self.peek();
        if peeked.is_none() {
            return
        }
        let peeked = peeked.unwrap();
        let tok = peeked.token;
        if let TOKEN::COMMENT_MLINE(_) = tok {
            self.next();
            return self.with_mline_comment_skip()
        }
    }
}