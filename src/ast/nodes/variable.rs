use std::sync::Arc;
use crate::ast::ast::Ast;

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
    pub(crate) entries: Option<Vec<DestructuringEntry>>,
    pub(crate) string_name: Option<String>,
    pub(crate) is_btick: Option<bool>
}
#[derive(Clone, Debug)]
pub struct DestructuringEntry {
    pub(crate) name: NamedRef,
    pub(crate) default_value: Option<Vec<Ast>>,
    pub(crate) alias: Option<NamedRef>,
    pub(crate) is_rest: bool
}


#[derive(Clone, Debug)]
pub struct VariableDeclaration {
    name: NamedRef,
    is_backtick_name: String,
    kind: VarDeclKind,
    initializer: Box<Ast>
}

#[derive(Clone, Debug)]
pub struct Assignment {
    to: String,
    kind: AssignmentKind,
    value: Box<Ast> //len 0 if AssignmentKind incr or decr
}