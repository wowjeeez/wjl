use crate::parser::ast::PartialAstIr;

#[derive(Clone, Debug, PartialEq)]
pub struct MapExprAstNode {
    is_async_block: bool,
    label: Option<String>,
    input_if_not_dynamic: Option<PartialAstIr>,
    body: PartialAstIr
}