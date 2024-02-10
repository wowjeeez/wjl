use either::Either;
use crate::ast::ast::Span;
use crate::ast::nodes::qualified_ident::QualifiedIdent;
use crate::ast::nodes::statics::StaticExpr;
use crate::ast::nodes::variable::VisibilityScope;
use crate::tokens::span::Span as TSpan;
#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum Expression {
    STATIC(StaticExpr),
    PIPE(Vec<TSpan<Expression>>),
    FUNC_CALL(FunctionCallExpr),
    TRANSIENT_FUNC_ARGS(Vec<TSpan<Expression>>),
    IF(IfExpr),
    MATCH(MatchExpr),
    ARRAY(ArrayExpr),
    STRUCT_DECL(StructDeclExpr),
    STRUCT_INIT(StructInitExpr),
    LOGICAL(LogExpr),
    TERNARY(TernaryExpr),
    ELVIS(ElvisExpr),
    GROUPED(Box<Expression>),
    AWAIT(Box<Expression>),
    RETURN(Box<Expression>),
    YIELD(Box<Expression>),
    TYPE_CAST(Either<QualifiedIdent, StaticExpr>),
    ELVIS_EXPR(Box<Expression>),
    BLOCK(Vec<Span>),
    INCREMENT(QualifiedIdent),
    DECREMENT(QualifiedIdent),
    INVERT(Box<Expression>),
    WJL_PLACEHOLDER
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpr {}

#[derive(Clone, Debug)]
pub struct IfExpr {}

#[derive(Clone, Debug)]
pub struct MatchExpr {}

#[derive(Clone, Debug)]
pub struct LogExpr {}

#[derive(Clone, Debug)]
pub struct ArrayExpr {}

//TODO! decorators
#[derive(Clone, Debug)]
pub struct StructDeclExpr {
    pub name: QualifiedIdent,
    pub fields: TSpan<Vec<TSpan<ObjectLikeFieldDeclaration>>>
}

#[derive(Clone, Debug)]
pub struct ObjectLikeFieldDeclaration {
    pub name: String,
    pub is_optional: bool,
    pub default: Option<TSpan<Expression>>,
    pub field_type: QualifiedIdent,
    pub visibility: VisibilityScope //only public/private
}

#[derive(Clone, Debug)]
pub struct StructInitExpr {}

#[derive(Clone, Debug)]
pub struct TernaryExpr {}

#[derive(Clone, Debug)]
pub struct ElvisExpr {

}