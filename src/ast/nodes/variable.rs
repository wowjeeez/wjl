use crate::ast::ast::{Span};
use crate::ast::nodes::expression::{AppliedDecoratorExpr, Expression};
use crate::ast::nodes::qualified_ident::QualifiedIdent;
use crate::tokens::span::Span as TSpan;
use crate::tokens::Token;

#[derive(Clone, Debug)]
#[allow(non_camel_case_types, unused)]
pub enum VarDeclKind {
    CONST,
    VAL,
    VAR,
    ONCE_VAL
}

impl VarDeclKind {
    pub fn parse_uc(tok: Token) -> VarDeclKind {
        match tok {
            Token::KEYWORD_CONST => VarDeclKind::CONST,
            Token::KEYWORD_VAR => VarDeclKind::VAR,
            Token::KEYWORD_VAL => VarDeclKind::VAL,
            _ => unreachable!()
        }
    }

    pub fn maybe_once(self, once: bool) -> VarDeclKind {
        match self {
            VarDeclKind::VAL if once => VarDeclKind::ONCE_VAL,
            _ => self
        }
    }
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

impl NamedRef {
    pub fn has_default(&self) -> bool {
        if self.entries.is_none() {return false};
        self.entries.as_ref().map_or(false, |x| x.iter().any(|x| x.get_inner_ref().default_value.is_some()))
    }
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
    pub name: TSpan<NamedRef>,
    pub kind: TSpan<VarDeclKind>,
    pub type_hint: Option<Box<Span>>,
    pub initializer: Option<Box<TSpan<Expression>>>,
    pub scoping: VisibilityScope,
    pub is_pure: bool,
    pub decorators: Vec<TSpan<AppliedDecoratorExpr>>
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