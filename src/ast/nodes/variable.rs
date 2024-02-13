use crate::ast::ast::{Span};
use crate::ast::nodes::expression::Expression;
use crate::ast::nodes::qualified_ident::QualifiedIdent;
use crate::tokens::span::Span as TSpan;

#[derive(Clone, Debug)]
#[allow(non_camel_case_types, unused)]
pub enum VarDeclKind {
    CONST,
    VAL,
    VAR,
    ONCE_VAL
}
#[derive(Clone, Debug)]
#[allow(non_camel_case_types, unused)]
pub enum AssignmentKind {
    REGULAR,
    SUM,
    MUL,
    DIV,
    MOD,
    SUB,
    DECR,
    INCR
}
#[derive(Clone, Debug)]
pub struct NamedRef {
    pub(crate) is_object_destruct: bool,
    pub(crate) is_array_destruct: bool,
    pub(crate) entries: Option<Vec<TSpan<DestructuringEntry>>>,
    pub(crate) name: Option<QualifiedIdent>,
}
#[derive(Clone, Debug)]
pub struct DestructuringEntry {
    pub(crate) name: TSpan<NamedRef>,
    pub(crate) default_value: Option<TSpan<Expression>>,
    pub(crate) binding: Option<TSpan<NamedRef>>,
    pub(crate) is_rest: bool
}

#[derive(Clone, Debug)]
pub enum VisibilityScope {
    PRIVATE,
    PROTECTED,
    INTERNAL,
    PUBLIC
}



#[derive(Clone, Debug)]
pub struct VariableDeclaration {
    name: TSpan<NamedRef>,
    is_backtick_name: Option<bool>,
    kind: TSpan<VarDeclKind>,
    type_hint: Option<Box<Span>>,
    initializer: Box<Span>,
    scoping: TSpan<VisibilityScope>
}

#[derive(Clone, Debug)]
pub struct Assignment {
    to: String,
    kind: AssignmentKind,
    value: Box<Span> //len 0 if AssignmentKind incr or decr
}


#[derive(Clone, Debug)]
pub struct ModifiersAndDecorators {
    pub(crate) is_once: bool,
    pub(crate) visibility: VisibilityScope,
}