use common::span::Span;
use crate::structures::qualified_ident::Identifier;

#[derive(Debug, Clone)]
pub enum VariableName {
    Ident(Span<Identifier>),
    ObjectDestructuring(),
    ArrayDestructuring(),
}