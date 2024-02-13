use std::hash::{Hash, Hasher};
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
    pub(crate) generic_args: Option<Span<GenericArgs>>,
    pub(crate) previous_link: Option<Connector>,
    pub(crate) is_opt_chained_to_next: bool,
    pub(crate) is_asserted_as_non_null: bool
}
pub type QualifiedIdent = Span<Vec<Span<QualifiedIdentPart>>>;



impl Span<Token> {
    pub fn as_qualified(&self) -> QualifiedIdent {
        let (content, is_b) = self.get_inner().get_ident_inner();
        vec![QualifiedIdentPart {
            segment: content,
            is_asserted_as_non_null: false,
            is_opt_chained_to_next: false,
            generic_args: None,
            is_btick: is_b,
            previous_link: None
        }.to_span(self.start, self.end)].to_span(self.start, self.end)
    }
}

impl QualifiedIdent {
    pub fn is_single(&self) -> bool {
        self.get_inner_ref().len() == 1 && self.get_inner_ref().first().unwrap().get_inner_ref().generic_args.is_none()
    }
    pub fn is_single_allow_gen(&self) -> bool {
        self.get_inner_ref().len() == 1
    }
    pub fn has_generic(&self) -> bool {
        self.get_inner_ref().iter().any(|x| x.get_inner_ref().generic_args.is_some())
    }
    pub fn starts_with_str<T: Into<String>>(&self, with: T) -> bool {
        let first = self.get_inner_ref().first();
        first.map_or(false, |x| x.get_inner_ref().segment == with.into())
    }
}