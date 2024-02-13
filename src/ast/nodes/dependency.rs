use crate::ast::nodes::qualified_ident::QualifiedIdent;
use crate::ast::nodes::variable::NamedRef;
use crate::tokens::span::Span as Tspan;

#[derive(Clone, Debug)]
pub struct DependencyImport {
    pub path: QualifiedIdent,
    pub is_js: bool,
    pub receiver: Option<Tspan<NamedRef>>
}