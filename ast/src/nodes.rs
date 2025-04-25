use common::span::Span;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstNode<T: Into<String> = &'static str> {
    UnexpectedTokenError(T, Option<String>)
}

pub type AstSpan = Span<AstNode>;

impl AstNode {
    pub fn spanned(self, start: usize, end: usize) -> Span<AstNode> {
        AstSpan::new(start, end, self)
    }

    pub fn spanned_one(self, start: usize) -> Span<AstNode> {
        AstSpan::new(start, start, self)
    }
}