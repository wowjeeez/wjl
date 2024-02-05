use crate::errors::ErrorReporter;
use crate::tokens::span::Span;

#[derive(Debug, Clone)]
pub enum Ast {

}

trait ToAst {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Ast>;
}

impl ToAst for Vec<Span<Ast>> {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Ast> {
        vec![]
    }
}