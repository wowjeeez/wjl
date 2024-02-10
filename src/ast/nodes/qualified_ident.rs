use crate::ast::nodes::expression::Expression;
use crate::tokens::span::{IntoSpan, Span};
use crate::tokens::Token;

#[derive(Clone, Debug)]
pub enum Connector {
    PERIOD,
    D_COL
}


pub type GenericArgs = Vec<Span<Expression>>;


#[derive(Clone, Debug)]
pub struct QualifiedIdentPart {
    pub(crate) segment: String,
    pub(crate) is_btick: bool,
    pub(crate) generics: Option<Span<GenericArgs>>,
    pub(crate) previous_link: Option<Connector>
}
pub type QualifiedIdent = Span<Vec<Span<QualifiedIdentPart>>>;


impl Span<Token> {
    pub fn as_qualified(&self) -> QualifiedIdent {
        let (content, is_b) = self.get_inner().get_ident_inner();
        vec![QualifiedIdentPart {
            segment: content,
            is_btick: is_b,
            previous_link: None
        }.to_span(self.start, self.end)].to_span(self.start, self.end)
    }
}