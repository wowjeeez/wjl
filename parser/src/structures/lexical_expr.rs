use std::fmt::Write;
use common::iter::SmartIterator;
use common::LocationDataProvider;
use common::span::Span;
use lexer::lex_string;
use lexer::token::{Token, TokenSpan};
use crate::errors::ParseError;
use crate::structures::array::parse_array_decl;
use crate::structures::function::{parse_async_func_decl, parse_func_decl};
use crate::structures::object::{parse_object_decl, ObjectField};
use crate::structures::qualified_ident::{parse_ident, parse_ident_no_expr, Identifier};
use crate::structures::type_expr::TypeExpression;
use crate::TokenIter;

#[derive(Debug, Clone)]
pub enum StringPart {
    String(String),
    Expr(Span<Expression>)
}


#[derive(Debug, Clone)]
pub enum Literal {
    DecimalNumber(String),
    OctalNumber(String),
    BinaryNumber(String),
    HexNumber(String),
    FloatNumber(String),
    SingleStringLiteral(Vec<StringPart>),
    DoubleStringLiteral(Vec<StringPart>),
}

#[derive(Debug, Clone)]
pub enum Operand {
    EQ,
    NEQ,
    GT,
    OR,
    AND,
    GTE,
    LT,
    LTE,
    SUM,
    DIV,
    SUB,
    MOD,
    MUL,
    PWR,
    B_OR,
    B_AND,
    B_XOR,
    B_ZF_LS,
    B_S_RS,
    B_ZF_RS
}

impl Operand {
    pub fn spanned(self, start: usize, end: usize) -> Span<Operand> {
        Span::new(start, end, self)
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Logical(Span<Box<Expression>>, Span<Operand>, Span<Box<Expression>>),
    Paren(Span<Box<Expression>>),
    ObjectDecl(Span<Vec<Span<ObjectField>>>),
    ArrayDecl(Span<Vec<Span<Expression>>>),
    Not(Span<Box<Expression>>),
    Reference(Span<Box<Expression>>),
    Call(Span<Box<Expression>>, Span<Vec<TypeExpression>>, Span<Vec<Expression>>),
    DecimalNumber(String),
    HexNumber(String),
    OctalNumber(String),
    FloatNumber(String),
    BinaryNumber(String),
    ConstExprTrue,
    ConstExprFalse,
    FunctionDeclExpr(()),
    IfExpr(),
    ClassExpr(),
    MatchExpr(),
    New(Span<Box<Expression>>),
    TryCatchExpr(),
    Await(Span<Box<Expression>>),
    ReflectBlock(),
    NullExpr,
    HashExpr(Span<Box<Expression>>),
}

pub fn parse_string_literal(smart_iterator: &mut TokenIter) -> Result<Span<Expression>, ParseError> {
    let (literal, _) = smart_iterator.0.next().unwrap();
    let literal = literal.clone();
    let start = literal.start();
    let end = literal.end();
    let content = match literal.get() {
        Token::D_LITERAL(literal_content) => literal_content.clone(),
        Token::S_LITERAL(literal_content) => literal_content.clone(),
        _ => unreachable!()
    };
    let double = match literal.get() {
        &Token::D_LITERAL(_) => true,
        _ => false
    };
    let mut current_static_part = String::new();
    let mut parts = Vec::new();
    let mut peekable_char_iter = content.chars().enumerate().peekable();
    while let Some((ix, character)) = peekable_char_iter.next() {
        if character == '$'
            && peekable_char_iter.peek().map_or(false, |(_, c)| *c == '{')
            && !current_static_part.ends_with("\\") {
            peekable_char_iter.next();
            let tokenized = lex_string(content[ix..].to_string());
            let mut tokens = TokenIter(SmartIterator::new(tokenized));
            let expr = parse_expr(&mut tokens)?;
            peekable_char_iter.next(); // drop trailing }
            if current_static_part.len() != 0 {
                parts.push(StringPart::String(current_static_part.clone()));
                current_static_part = String::new();
            }
            parts.push(StringPart::Expr(expr));
        }
        current_static_part.write_char(character).expect("failed to write char");
    }
    let literal = if double {
        Literal::DoubleStringLiteral(parts)
    } else {
        Literal::SingleStringLiteral(parts)
    };
    let span = Span::new(start, end, Expression::Literal(literal));
    Ok(span)
}

fn parse_expr_side(smart_iterator: &mut TokenIter) -> Result<Span<Expression>, ParseError> {
    let location = smart_iterator.get_location();
    let start = smart_iterator.0.peek().ok_or(ParseError::ExpectedExpression(location, None))?;
    let location = smart_iterator.get_location();
    let start = start.clone();
    let expr = match start.get() {
        Token::L_PAREN => {
            smart_iterator.0.next();
            let expr = parse_expr(smart_iterator)?;
            Ok(Expression::Paren(expr.boxed()))
        }
        Token::L_CURLY => {
            let object = parse_object_decl(smart_iterator)?;
            Ok(Expression::ObjectDecl(object))
        }
        Token::L_BRACKET => {
            let array = parse_array_decl(smart_iterator)?;
            Ok(Expression::ArrayDecl(array))
        }
        Token::EXCL => {
            let expr = parse_expr(smart_iterator)?;
            Ok(Expression::Not(expr.boxed()))

        }
        Token::HASH => {
            smart_iterator.next_or_expect_expr()?;
            let expr = parse_expr(smart_iterator)?;
            Ok(Expression::HashExpr(expr.boxed()))
        }
        Token::IDENT(_) => {
            let ident = parse_ident(smart_iterator)?;
            Ok(ident.get().clone())
        }
        Token::D_LITERAL(_) | Token::S_LITERAL(_) => {
            let literal = parse_string_literal(smart_iterator)?;
            Ok(literal.get().clone())
        }
        Token::D_NUMBER(v) => Ok(Expression::DecimalNumber(v.clone())),
        Token::B_NUMBER(v) => Ok(Expression::BinaryNumber(v.clone())),
        Token::H_NUMBER(v) => Ok(Expression::HexNumber(v.clone())),
        Token::O_NUMBER(v) => Ok(Expression::OctalNumber(v.clone())),
        Token::FD_NUMBER(v) => Ok(Expression::FloatNumber(v.clone())),
        Token::KEYWORD_TRUE => Ok(Expression::ConstExprTrue),
        Token::KEYWORD_FALSE => Ok(Expression::ConstExprFalse),
        Token::KEYWORD_FUNCTION => {
            let next = smart_iterator.next_or_expect_expr()?;
            let func = parse_func_decl(smart_iterator, next)?;
            Ok(Expression::FunctionDeclExpr(func))
        }
        Token::KEYWORD_IF => {
            todo!("");
            Ok(Expression::IfExpr())
        }
        Token::KEYWORD_CLASS => {
            todo!();
            Ok(Expression::ClassExpr())
        }
        Token::KEYWORD_MATCH => {
            todo!();
            Ok(Expression::MatchExpr())
        }
        Token::KEYWORD_ASYNC => {
            smart_iterator.next_or_expect_expr()?;
            let func = parse_async_func_decl(smart_iterator)?;
            Ok(Expression::FunctionDeclExpr(func))
        }
        Token::KEYWORD_NEW => {
            smart_iterator.next_or_expect_expr()?;
            let next = smart_iterator.next_or_expect_expr()?;
            let receiver = parse_expr(smart_iterator)?;
            Ok(Expression::New(receiver.boxed()))
        }
        Token::KEYWORD_TRY => {
            todo!();
            Ok(Expression::TryCatchExpr())
        }
        Token::KEYWORD_AWAIT => {
            smart_iterator.next_or_expect_expr()?;
            let receiver = parse_expr(smart_iterator)?;
            Ok(Expression::Await(receiver.boxed()))
        }
        Token::KEYWORD_REFLECTION => {
            todo!();
            Ok(Expression::ReflectBlock())
        }
        Token::KEYWORD_NULL => Ok(Expression::NullExpr),
        _ => Err(ParseError::ExpectedExpression(location, Some(start.get().clone())))
    }?;
    let expr_end = smart_iterator.0.get_next_index() -1;
    Ok(Span::new(start.start(), expr_end, expr))
}

fn parse_call_expr(smart_iterator: &mut TokenIter, receiver: Span<Expression>) -> Result<Span<Expression>, ParseError> {

}


pub fn parse_expr(smart_iterator: &mut TokenIter) -> Result<Span<Expression>, ParseError> {
    let left = parse_expr_side(smart_iterator)?;
    let next = smart_iterator.0.peek();
    if next.is_none() {
        return Ok(left);
    }
    let next = next.unwrap();
    let location = smart_iterator.get_location();
    let maybe_logical_op = smart_iterator.0.peek().ok_or(ParseError::ExpectedExpression(location, None))?;
    let maybe_logical_op = maybe_logical_op.clone();
    let maybe_logical_op_token = maybe_logical_op.get();

    let operand = match next.get() {
        Token::R_ANGLE =>
            if maybe_logical_op_token == &Token::ASSIGN {
                Some(Operand::GTE.spanned(next.start(), maybe_logical_op.end()))
            } else if maybe_logical_op_token == &Token::R_ANGLE {
                Some(Operand::B_S_RS.spanned(next.start(), maybe_logical_op.end()))
            }
                //TODO! peek +1 to check for >>>
            else {
                Some(Operand::GT.spanned(next.start(), next.end()))
            }
        Token::L_ANGLE => if maybe_logical_op_token == &Token::ASSIGN {
            Some(Operand::LTE.spanned(next.start(), maybe_logical_op.end()))
        } else if maybe_logical_op_token == &Token::L_ANGLE {
            Some(Operand::B_ZF_LS.spanned(next.start(), maybe_logical_op.end()))
        } else {
            Some(Operand::LT.spanned(next.start(), next.end()))
        }
        Token::ASSIGN if maybe_logical_op_token == &Token::ASSIGN => Some(Operand::EQ.spanned(next.start(), maybe_logical_op.end())),
        Token::EXCL if maybe_logical_op_token == &Token::ASSIGN => Some(Operand::NEQ.spanned(next.start(), maybe_logical_op.end())),
        Token::QMARK => None,
        Token::SUM => Some(Operand::SUM.spanned(next.start(), next.end())),
        Token::SUB => Some(Operand::SUB.spanned(next.start(), next.end())),
        Token::DIV => Some(Operand::DIV.spanned(next.start(), next.end())),
        Token::MUL if maybe_logical_op_token == &Token::MUL => Some(Operand::PWR.spanned(next.start(), maybe_logical_op.end())),
        Token::MUL => Some(Operand::MUL.spanned(next.start(), next.end())),
        Token::MOD => Some(Operand::MOD.spanned(next.start(), next.end())),
        Token::PIPE =>
            if maybe_logical_op_token == &Token::PIPE {
                Some(Operand::OR.spanned(next.start(), maybe_logical_op.end()))
            } else {
                Some(Operand::B_OR.spanned(next.start(), next.end()))
            }
        Token::AMP => if maybe_logical_op_token == &Token::AMP {
            Some(Operand::AND.spanned(next.start(), maybe_logical_op.end()))
        } else {
            Some(Operand::B_AND.spanned(next.start(), next.end()))
        }
        _ => None,
    };
    if operand.is_none() {
        return Ok(left);
    }
    let operand = operand.unwrap();
    if operand.end() - operand.start() != 1 {
        smart_iterator.next_or_expect_expr()?;
    }

    let right_side = parse_expr_side(smart_iterator)?;
    Ok(Span::new(left.start(), right_side.end(), Expression::Logical(left.boxed(), operand, right_side.boxed())))
}
