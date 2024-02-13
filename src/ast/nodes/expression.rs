use either::Either;
use crate::ast::ast::Span;
use crate::ast::nodes::qualified_ident::QualifiedIdent;
use crate::ast::nodes::statics::StaticExpr;
use crate::ast::nodes::variable::VisibilityScope;
use crate::helpers::Triple::B;
use crate::tokens::span::Span as TSpan;
use crate::tokens::Token;

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
    IDENT(QualifiedIdent),
    BOOLEAN(BooleanExpression),
    TYPE_CONSTRAINT(Box<TypeConstraintExpr>),
    WJL_PLACEHOLDER
}



#[derive(Clone, Debug)]
pub struct BooleanExpression {
    pub(crate) left: Box<TSpan<Expression>>,
    pub(crate) right: Box<TSpan<Expression>>,
    pub(crate) op: TSpan<BinOp>
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpr {
    pub reference: Box<TSpan<Expression>>,
    pub arguments: Vec<TSpan<Expression>>
}

#[derive(Clone, Debug)]
pub struct IfExpr {}

#[derive(Clone, Debug)]
pub struct MatchExpr {}

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum LogicalOperator {
    SUM, SUBTRACT, DIV, MUL, MOD, BIT_NOT, BIT_AND, BIT_XOR, BIT_S_RIGHT_SHIFT, BIT_ZERO_FILL_RIGHT_SHIFT, BIT_ZERO_FILL_LEFT_SHIFT
}

impl Token {
    pub fn as_logical(&self) -> LogicalOperator {
        match self {
            Token::SUM => LogicalOperator::SUM,
            Token::SUBTRACT => LogicalOperator::SUBTRACT,
            Token::DIV => LogicalOperator::DIV,
            Token::MUL => LogicalOperator::MUL,
            Token::MOD => LogicalOperator::MOD,
            Token::BIT_NOT => LogicalOperator::BIT_NOT,
            Token::BIT_AND => LogicalOperator::BIT_AND,
            Token::BIT_XOR => LogicalOperator::BIT_XOR,
            Token::BIT_S_RIGHT_SHIFT => LogicalOperator::BIT_S_RIGHT_SHIFT,
            Token::BIT_ZERO_FILL_RIGHT_SHIFT => LogicalOperator::BIT_ZERO_FILL_RIGHT_SHIFT,
            Token::BIT_ZERO_FILL_LEFT_SHIFT => LogicalOperator::BIT_ZERO_FILL_LEFT_SHIFT,
            _ => unreachable!()
        }
    }

    pub fn as_binop_bool(&self) -> BinOp {
        match self {
            Token::OR => BinOp::OR,
            Token::AND => BinOp::AND,
            _ => unreachable!()
        }
    }
}

#[derive(Clone, Debug)]
pub struct LogExpr {
    pub left: Box<TSpan<Expression>>,
    pub right: Box<TSpan<Expression>>,
    pub op: TSpan<LogicalOperator>
}

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
    pub field_type: TSpan<Expression>,
    pub visibility: VisibilityScope //only public/private
}

#[derive(Clone, Debug)]
pub struct StructInitExpr {}

#[derive(Clone, Debug)]
pub struct TernaryExpr {
    pub condition: Box<TSpan<Expression>>,
    pub left: Box<TSpan<Expression>>,
    pub right: Box<TSpan<Expression>>
}

#[derive(Clone, Debug)]
pub struct ElvisExpr {
    pub left: Box<TSpan<Expression>>,
    pub right: Box<TSpan<Expression>>
}

#[derive(Clone, Debug)]
pub struct TypeConstraintExpr {
    pub receiver: TSpan<Expression>,
    pub constraints: Vec<TSpan<TypeConstraintExprPart>>
}

#[derive(Clone, Debug)]
pub enum BinOp {
    AND,
    OR
}
#[derive(Clone, Debug)]
pub struct TypeConstraintExprPart {
    pub constraint: QualifiedIdent,
    pub next_join: Option<BinOp>
}
#[derive(Clone, Debug)]
pub struct AppliedDecoratorExpr {
    pub decorator: QualifiedIdent,
    pub args: Option<Vec<Expression>> //None if the decorator is called like @Decorator
}