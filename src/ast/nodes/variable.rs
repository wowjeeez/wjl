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
pub struct Name {
    is_object_destruct: bool,
    is_array_destruct: bool,
    entries: Option<Vec<DestructuringEntry>>,
    string_name: Option<String>,
    is_btick: Option<String>
}
#[derive(Clone, Debug)]
pub struct DestructuringEntry {
    name: Name,
    default_value: Option<Vec<Ast>>,
    alias: Option<String>,
    is_alias_btick: Option<bool>,
    is_rest: bool
}


#[derive(Clone, Debug)]
pub struct VariableDeclaration {
    name: Name,
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