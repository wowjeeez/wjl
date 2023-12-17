use either::Either;
use crate::iter_util::CommonIter;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::PartialAstIr::{BOOLEXPR_FALSE, BOOLEXPR_TRUE, FLOATEXPR, IDENT_REF, INTEXPR, NULLEXPR, STREXPR};
use crate::parser::errors::ParseError;
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::variable::VariableAstNode;
use crate::parser::nodes::array::ArrayAstNode;
use crate::parser::nodes::array_destruct::ArrayDestructureAstNode;
use crate::parser::nodes::generic_arg::GenericArgAstNode;
use crate::parser::nodes::generic_decl::GenericDeclarationAstNode;
use crate::parser::nodes::identifier::IdentifierAstNode;
use crate::parser::nodes::object_destruct::ObjectDestructureAstNode;

#[derive(PartialEq, Debug, Clone)]
#[allow(non_camel_case_types)]
pub enum PartialAstIr {
    VAR_DECL(VariableAstNode),
    IDENT_REF(Vec<IdentifierAstNode>),
    ARRAY_VARIABLE_DESTRUCTURING(ArrayDestructureAstNode),
    OBJECT_VARIABLE_DESTRUCTURING(ObjectDestructureAstNode),
    TYPE_ANNOTATION(Vec<PartialAstIr>),
    PAREN_EXPR(Vec<PartialAstIr>),
    INT(i64),
    FLOAT(f64),
    ARBIT_BLOCK(Vec<PartialAstIr>),
    ARRAY_DECL(ArrayAstNode),
    BOOLEXPR_TRUE,
    BOOLEXPR_FALSE,
    FLOATEXPR(f64),
    INTEXPR(i64),
    NULLEXPR,
    STREXPR(String),
    LOGICAL_OP(TOKEN),
    GENERIC_DECL(Vec<GenericDeclarationAstNode>),
    GENERIC_ARG(Vec<GenericArgAstNode>),
    _NONCE
}

#[derive(PartialEq, Debug, Clone)]
#[allow(non_camel_case_types)]
pub enum NodeVis {
    PRIVATE,
    PUBLIC,
    PROTECTED,
    BLOCK
}

pub trait AstNodeParsable {
    fn parse(iter: &mut TokenParserIter, span: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError>;
}

impl TOKEN {
    pub fn is_logical_op(&self) -> bool {
        match self {
            TOKEN::MUL => true,
            TOKEN::DIV => true,
            TOKEN::SUM => true,
            TOKEN::SUBTRACT => true,
            TOKEN::MOD => true,
            TOKEN::B_INV => true,
            TOKEN::B_XOR => true,
            TOKEN::S2_L_GTE => true,
            TOKEN::S2_R_GTE => true,
            TOKEN::S2_OR => true,
            TOKEN::S2_AND => true,
            TOKEN::LEFT_ANGLE => true,
            TOKEN::RIGHT_ANGLE => true,
            TOKEN::S2_B_SIGNED_RIGHT => true,
            TOKEN::S2_B_RIGHT_FILL => true,
            TOKEN::S2_B_AND => true,
            TOKEN::S2_B_LEFT_FILL => true,
            _ => false
        }
    }
}

impl PartialAstIr {
    pub fn c_get_ident_ref_content(&self) -> Vec<IdentifierAstNode> {
        match self {
            IDENT_REF(inner) => inner.clone(),
            _ => unreachable!()
        }
    }

    pub fn parse_static_value(value: TOKEN) -> PartialAstIr {
        match value {
            TOKEN::TRUE => BOOLEXPR_TRUE,
            TOKEN::FALSE => BOOLEXPR_FALSE,
            TOKEN::KEYWORD_NULL => NULLEXPR,
            TOKEN::S2_FLOAT_LITERAL(f) => FLOATEXPR(f),
            TOKEN::S2_INT_LITERAL(i) => INTEXPR(i),
            TOKEN::STRING_LITERAL(str) => STREXPR(str),
            _ => unreachable!()
        }
    }
}