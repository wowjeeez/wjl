use common::span::Span;
use crate::class_decl::ClassDeclaration;
use crate::function_decl::FunctionDeclaration;
use crate::import::Import;
use crate::variable::VariableDeclaration;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LexicalScope {
    imports: Vec<Span<Import>>,
    variables: Vec<Span<VariableDeclaration>>,
    functions: Vec<Span<FunctionDeclaration>>,
    classes: Vec<Span<ClassDeclaration>>,
    _ordering: Vec<Ordering>
}


#[derive(Debug, Clone, Eq, PartialEq)]
enum Ordering {
    IMPORT(usize),
    VAR(usize),
    FUNC(usize),
    CLASS(usize),
}
