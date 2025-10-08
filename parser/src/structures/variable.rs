use common::span::Span;
use crate::structures::qualified_ident::Identifier;

pub enum VariableName {
    Ident(Span<Identifier>),
    ObjectDestructuring(),
    ArrayDestructuring(),
}