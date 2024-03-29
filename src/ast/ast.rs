use crate::ast::nodes::dependency::DependencyImport;
use crate::ast::nodes::expression::{
    AppliedDecoratorExpr, BinOp, BooleanExpression, ElvisExpr, Expression, FunctionCallExpr,
    LogExpr, ObjectLikeFieldDeclaration, StructDeclExpr, StructInitExpr, TernaryExpr,
    TypeConstraintExpr, TypeConstraintExprPart,
};
use crate::ast::nodes::qualified_ident::{
    Connector, GenericArgs, QualifiedIdent, QualifiedIdentPart,
};
use crate::ast::nodes::statics::StaticExpr;
use crate::ast::nodes::variable::{
    Assignment, DestructuringEntry, ModifiersAndDecorators, NamedRef, VarDeclKind,
    VariableDeclaration, VisibilityScope,
};
use crate::errors::{ErrorReporter, WjlError};
use crate::helpers::opt_to_broad_err;
use crate::helpers::vec_utils::AsOption;
use crate::iter::{wrap_iter, GenericIterator, PeekableIterator};
use crate::tokens::span::{IntoSpan, Span as TSpan};
use crate::tokens::Token;
use crate::tokens::Token::{KEYWORD_CONST, KEYWORD_VAL, KEYWORD_VAR};
use either::Either;
use std::ops::Bound::Unbounded;
use std::process::id;
use tracing::{debug, error, info};

pub type TokenSpan = TSpan<Token>;
pub type Span = TSpan<Ast>;

#[derive(Debug, Clone)]
#[allow(non_camel_case_types, unused)]
pub enum Ast {
    VAR_DECL(VariableDeclaration),
    ASSIGNMENT(Assignment),
    IMPORT(DependencyImport),
    TYPE_DEF(Expression),
}

pub trait ToAst {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Span>;
}

impl PeekableIterator<TokenSpan, Either<TSpan<AppliedDecoratorExpr>, TokenSpan>> {
    pub fn next_if_skip_ml(&mut self, eq_tok: Token) -> Option<TokenSpan> {
        self.next_skip_ml_comment()
            .map(|x| {
                if x.get_inner() == eq_tok {
                    Some(x)
                } else {
                    None
                }
            })
            .flatten()
    }

    pub fn next_skip_ml_comment(&mut self) -> Option<TokenSpan> {
        let next = self.next();
        if next
            .as_ref()
            .map_or(false, |x| match x.get_inner() {
                Token::COMMENT_ML(_) => true,
                _ => false,
            })
            .eq(&true)
        {
            return self.next_skip_ml_comment();
        }
        next
    }

    pub fn seek_back_skip_ml_comment(&mut self) {
        let prev = self.get_index().unwrap() - 1;
        self.set_index(prev);
        let curr = self.get_content().get(prev);
        if curr
            .as_ref()
            .map_or(false, |x| match x.get_inner() {
                Token::COMMENT_ML(_) => true,
                _ => false,
            })
            .eq(&true)
        {
            return self.seek_back_skip_ml_comment();
        }
    }

    pub fn peek_prev_skip_ml_comment(&self) -> (Option<TokenSpan>, isize) {
        self.peek_n_skip_ml_comment(-1)
    }
    pub fn peek_next_skip_ml_comment(&self) -> (Option<TokenSpan>, isize) {
        self.peek_n_skip_ml_comment(1)
    }

    pub fn peek_n_skip_ml_comment(&self, by: isize) -> (Option<TokenSpan>, isize) {
        let prev = self.peek_n(by);
        if prev
            .as_ref()
            .map_or(false, |x| match x.get_inner() {
                Token::COMMENT_ML(_) => true,
                _ => false,
            })
            .eq(&true)
        {
            return self.peek_n_skip_ml_comment(if by.is_negative() { by - 1 } else { by + 1 });
        }
        (prev, by)
    }

    // will consume from the iter expecting that we are on the {
    pub fn parse_object_destruct_expr(
        &mut self,
        reporter: &mut ErrorReporter,
    ) -> Option<TSpan<NamedRef>> {
        debug!("Parsing object destruct expr");
        let before = self.get_index().unwrap();
        let start = self.curr().unwrap().start;
        let first_tok = self.next_skip_ml_comment();
        if first_tok.is_none() {
            let prev = self.peek_prev_skip_ml_comment().0.unwrap();
            reporter.add(
                WjlError::ast(prev.end)
                    .message("Expected identifier, rest spread or closing }, got nothing.")
                    .ok(),
            );
            return None;
        }
        let first_tok = first_tok.unwrap();
        if first_tok.get_inner() == Token::BRACE_RIGHT {
            //happy path cuz we aint gotta do shit
            return Some(
                NamedRef {
                    entries: Some(vec![]),
                    is_array_destruct: false,
                    is_object_destruct: true,
                    name: None,
                }
                .to_span(start, first_tok.end),
            );
        }
        if first_tok.get_inner() == Token::BRACKET_LEFT {
            reporter.add(
                WjlError::ast(first_tok.start)
                    .message("Arbitrary [key] is not allowed in Wjl.")
                    .pot_fix("Use explicit properties")
                    .ok(),
            );
            return None;
        }
        if first_tok.get_inner().is_ident() || first_tok.get_inner() == Token::OP_SPREAD {
            self.set_index(before); //we dont seek because there could have been a multiline comment inbetween
            let next_entry = self.parse_identifier_in_destruct(false, reporter)?;
            let mut entries = vec![next_entry];
            let next = self.peek_next_skip_ml_comment().0;
            if next.is_none() {
                reporter.add(WjlError::ast(first_tok.end).message("Expected closing or comma after first entry in destructuring, got nothing.").ok());
                return None;
            }
            while let tok = self.next_skip_ml_comment() {
                if tok.is_none() {
                    reporter.add(WjlError::ast(first_tok.end).message("Expected closing or comma after first entry in destructuring, got nothing.").ok());
                    return None;
                }
                let tok = tok.unwrap();
                if tok.get_inner() == Token::BRACE_RIGHT {
                    break;
                }
                let entry = self.parse_identifier_in_destruct(false, reporter)?;
                entries.push(entry);
            }
            return Some(
                NamedRef {
                    is_object_destruct: true,
                    is_array_destruct: false,
                    entries: Some(entries),
                    name: None,
                }
                .to_span(start, self.curr().unwrap().end),
            );
        }

        reporter.add(
            WjlError::ast(first_tok.start)
                .message("Unexpected token.")
                .ok(),
        );
        return None;
    }

    // will consume from the iter expecting that we are on the [
    pub fn parse_array_destruct_expr(
        &mut self,
        reporter: &mut ErrorReporter,
    ) -> Option<TSpan<NamedRef>> {
        debug!("Parsing array destruct expr");
        let before = self.get_index().unwrap();
        let start = self.curr().unwrap().start;
        let first_tok = self.next_skip_ml_comment();
        if first_tok.is_none() {
            let prev = self.peek_prev_skip_ml_comment().0.unwrap();
            reporter.add(
                WjlError::ast(prev.end)
                    .message("Expected identifier, rest spread or closing ], got nothing.")
                    .ok(),
            );
            return None;
        }
        let first_tok = first_tok.unwrap();
        if first_tok.get_inner() == Token::BRACKET_RIGHT {
            //happy path cuz we aint gotta do shit
            return Some(
                NamedRef {
                    entries: Some(vec![]),
                    is_array_destruct: true,
                    is_object_destruct: false,
                    name: None,
                }
                .to_span(start, first_tok.end),
            );
        }
        if first_tok.get_inner().is_ident()
            || first_tok.get_inner() == Token::OP_SPREAD
            || first_tok.get_inner() == Token::BRACKET_LEFT
        {
            self.set_index(before); //we dont seek because there could have been a multiline comment inbetween
            let next_entry = self.parse_identifier_in_destruct(true, reporter)?;
            let mut entries = vec![next_entry];
            let next = self.peek_next_skip_ml_comment().0;
            if next.is_none() {
                reporter.add(WjlError::ast(first_tok.end).message("Expected closing or comma after first entry in destructuring, got nothing.").ok());
                return None;
            }
            while let tok = self.next_skip_ml_comment() {
                if tok.is_none() {
                    reporter.add(WjlError::ast(first_tok.end).message("Expected closing or comma after first entry in destructuring, got nothing.").ok());
                    return None;
                }
                let tok = tok.unwrap();
                if tok.get_inner() == Token::BRACKET_RIGHT {
                    break;
                }
                let entry = self.parse_identifier_in_destruct(true, reporter)?;
                entries.push(entry);
            }
            return Some(
                NamedRef {
                    is_object_destruct: false,
                    is_array_destruct: true,
                    entries: Some(entries),
                    name: None,
                }
                .to_span(start, self.curr().unwrap().end),
            );
        }
        reporter.add(
            WjlError::ast(first_tok.start)
                .message("Unexpected token.")
                .ok(),
        );
        return None;
    }

    // expecting that we are before the ident or rest arg
    pub fn parse_identifier_in_destruct(
        &mut self,
        in_arr: bool,
        reporter: &mut ErrorReporter,
    ) -> Option<TSpan<DestructuringEntry>> {
        debug!("Parsing ident in destruct expr");
        let mut target = self.next_skip_ml_comment().unwrap(); //we know its there otherwise we dont call it
        let start = target.start;
        let mut inner = target.get_inner();
        let mut is_spread = false;
        if inner == Token::OP_SPREAD {
            is_spread = true;
            let next = self.next_skip_ml_comment();
            if next.is_none() {
                reporter.add(
                    WjlError::ast(target.end)
                        .message("Expected identifier, got nothing.")
                        .ok(),
                );
                return None;
            }
            let next = next.unwrap();
            target = next;
            inner = target.get_inner();
        }
        if in_arr {
            if inner == Token::BRACKET_LEFT {
                let parsed = self.parse_array_destruct_expr(reporter)?;
                return Some(
                    DestructuringEntry {
                        name: parsed,
                        default_value: None,
                        binding: None,
                        is_rest: is_spread,
                    }
                    .to_span(start, target.end),
                );
            }
            if inner == Token::BRACE_LEFT {
                let parsed = self.parse_object_destruct_expr(reporter)?;
                return Some(
                    DestructuringEntry {
                        name: parsed,
                        default_value: None,
                        binding: None,
                        is_rest: is_spread,
                    }
                    .to_span(start, target.end),
                );
            }
        }

        if !inner.is_ident() {
            reporter.add(
                WjlError::ast(target.end)
                    .message("Expected identifier.")
                    .ok(),
            );
            return None;
        }
        let inner = target.as_qualified();
        let (next, end) = self.peek_next_skip_ml_comment();
        if next.is_none() {
            // safe to cast to usize
            reporter.add(
                WjlError::ast(target.end + end as usize)
                    .message(
                        "Expected assignment, alias, comma or closing bracket (}, ]) got nothing.",
                    )
                    .ok(),
            );
            return None;
        }
        let next = next.unwrap();
        if next.get_inner() == Token::COMMA
            || next.get_inner()
                == if in_arr {
                    Token::BRACKET_RIGHT
                } else {
                    Token::BRACE_RIGHT
                }
        {
            let parsed = self.parse_qualified_ident(reporter, false)?;
            return Some(
                DestructuringEntry {
                    is_rest: is_spread,
                    name: NamedRef {
                        is_object_destruct: false,
                        is_array_destruct: false,
                        entries: None,
                        name: Some(parsed),
                    }
                    .to_span(target.start, target.end),
                    default_value: None,
                    binding: None,
                }
                .to_span(start, next.end),
            );
        }
        self.next_skip_ml_comment(); //advance

        if next.get_inner() == Token::COLON {
            if in_arr {
                reporter.add(
                    WjlError::ast(next.start)
                        .message("Binding is not allowed in array destructuring")
                        .pot_fix("Remove the :")
                        .ok(),
                );
                return None;
            }
            let alias_tok = self.next_skip_ml_comment();
            if alias_tok.is_none() {
                reporter.add(
                    WjlError::ast(next.end)
                        .message("Expected identifier or destructuring statement, got nothing.")
                        .ok(),
                );
                return None;
            }
            let alias_tok = alias_tok.unwrap();
            if alias_tok.get_inner().is_ident() {
                let ident = self.parse_qualified_ident(reporter, false)?;
                return Some(
                    DestructuringEntry {
                        binding: Some(
                            NamedRef {
                                entries: None,
                                is_array_destruct: false,
                                is_object_destruct: false,
                                name: Some(ident),
                            }
                            .to_span(alias_tok.start, alias_tok.end),
                        ),
                        is_rest: is_spread,
                        name: NamedRef {
                            entries: None,
                            is_array_destruct: false,
                            is_object_destruct: false,
                            name: Some(inner),
                        }
                        .to_span(target.start, target.end),
                        default_value: None,
                    }
                    .to_span(start, alias_tok.end),
                );
            }
            if alias_tok.get_inner() == Token::BRACKET_LEFT {
                let binding = self.parse_array_destruct_expr(reporter)?;
                let end = binding.end;
                return Some(
                    DestructuringEntry {
                        binding: Some(binding),
                        is_rest: is_spread,
                        name: NamedRef {
                            entries: None,
                            is_array_destruct: false,
                            is_object_destruct: false,
                            name: Some(inner),
                        }
                        .to_span(target.start, target.end),
                        default_value: None,
                    }
                    .to_span(start, end),
                );
            }
            if alias_tok.get_inner() == Token::BRACE_LEFT {
                let binding = self.parse_object_destruct_expr(reporter)?;
                let end = binding.end;
                return Some(
                    DestructuringEntry {
                        binding: Some(binding),
                        is_rest: is_spread,
                        name: NamedRef {
                            entries: None,
                            is_array_destruct: false,
                            is_object_destruct: false,
                            name: Some(inner),
                        }
                        .to_span(target.start, target.end),
                        default_value: None,
                    }
                    .to_span(start, end),
                );
            }
        }

        if next.get_inner() == Token::ASSIGN {
            // default
            let default = self.parse_expr(reporter)?;
            let parsed = self.parse_qualified_ident(reporter, false)?;
            return Some(
                DestructuringEntry {
                    is_rest: is_spread,
                    name: NamedRef {
                        is_object_destruct: false,
                        is_array_destruct: false,
                        entries: None,
                        name: Some(parsed),
                    }
                    .to_span(target.start, target.end),
                    default_value: Some(default),
                    binding: None,
                }
                .to_span(start, next.end),
            );
        }

        reporter.add(WjlError::lex(next.start).message("Unexpected token").ok());
        return None;
    }

    pub fn parse_qualified_ident_2(
        &mut self,
        reporter: &mut ErrorReporter,
        treat_generics_as_decl: bool
    ) -> Option<QualifiedIdent> {
        let start = self.curr().unwrap().start;
        self.seek(-1);
        let mut elements: Vec<TSpan<QualifiedIdentPart>> = vec![];
        //ident1<A, B>?.ident2!::test<C>
        let mut global_end = 0;
        while let Some(ident) = self.next_skip_ml_comment() {
            if !ident.get_inner_ref().is_ident() {
                break
            }
            let prev = self.peek_prev_skip_ml_comment().0;
            let (prev_link, start) = if prev.map_or(false, |x| [Token::PERIOD, Token::DOUBLE_COLON].contains(x.get_inner_ref())).eq(&true) {
                let prev = prev.unwrap();
                (Some(match prev.get_inner_ref() {
                    Token::DOUBLE_COLON => Connector::D_COL,
                    Token::PERIOD => Connector::PERIOD,
                    _ => unreachable!(),
                }), prev.start)
            } else {(None, ident.start)};

            let next = self.peek_next_skip_ml_comment().0;
            if next.is_none() {
                let ident_end = ident.end;
                global_end = ident_end;
                let (ident, is_bt) = ident.get_inner_ref().get_ident_inner();
                elements.push(QualifiedIdentPart {
                    generic_args: None,
                    is_asserted_as_non_null: false,
                    is_btick: is_bt,
                    is_opt_chained_to_next: false,
                    segment: ident,
                    previous_link: prev_link
                }.into_span(start, ident_end));
                break
            }
            let next = next.unwrap();
            let next = if next.get_inner_ref() == Token::ANGLE_LEFT {

            } else {next};
            if next.get_inner_ref() == Token::QMARK || next.get_inner_ref() == Token::EXCL_MARK {

            }
        }
        return None
    }

    // expecting that we are on the opening <
    // ident<A> > 10
    // ident < A
    // ident<A, 10>
    // ident<A : B ? 1 : 2>
    // ident<[A, B]>
    // ident<class A {}>
    // broader cx: myFunction(someOtherFunction<C>(a) < 10, b > 10)
    pub fn maybe_parse_generic_arguments(&mut self, reporter: &mut ErrorReporter, force: bool) {

    }


    // expecting that we are on the first ident
    pub fn parse_qualified_ident(
        &mut self,
        reporter: &mut ErrorReporter,
        treat_generics_as_decl: bool,
    ) -> Option<QualifiedIdent> {
        debug!("Parsing identifier");
        //TODO! parse generics even if they are on the first arg
        let first_ident = self.curr().unwrap();
        let next = self.peek_next_skip_ml_comment().0;
        let (ident, is_bt) = first_ident.get_inner().get_ident_inner();
        if ident == "this".to_string() {
            todo!("this arg disambiguation")
        }
        let ident = QualifiedIdentPart {
            segment: ident,
            is_btick: is_bt,
            previous_link: None,
            generic_args: None,
            is_opt_chained_to_next: false,
            is_asserted_as_non_null: false,
        }
        .to_span(first_ident.start, first_ident.end);
        if next.is_none() {
            return Some(vec![ident].to_span(first_ident.start, first_ident.end));
        }
        let mut contents = vec![ident];
        let mut next = next.unwrap();
        let (next_after_op, seek) = self.peek_n_skip_ml_comment(2);
        if next_after_op.map_or(false, |x| ![Token::PERIOD, Token::DOUBLE_COLON].contains(x.get_inner_ref())).eq(&true) {
            self.seek(seek);
            return Some(contents.to_span(first_ident.start, first_ident.end));
        }
        self.seek(seek);

        if next.get_inner_ref() == &Token::QMARK || next.get_inner_ref() == &Token::EXCL_MARK {
            let new = contents.pop().unwrap();
            let inner = new.get_inner();
            let first = QualifiedIdentPart {
                is_opt_chained_to_next: next.get_inner_ref() == &Token::QMARK,
                is_asserted_as_non_null: next.get_inner_ref() == &Token::EXCL_MARK,
                generic_args: inner.generic_args,
                segment: inner.segment,
                is_btick: inner.is_btick,
                previous_link: inner.previous_link,
            }
            .to_span(new.start, next.end);
            contents = vec![first];
            let next_inner = self.next_skip_ml_comment();
            if next_inner.is_none() {
                return Some(contents.to_span(first_ident.start, next.end));
            }
            let next_inner = next_inner.unwrap();
            next = next_inner;
        }

        if next.get_inner() == Token::DOUBLE_COLON || next.get_inner() == Token::PERIOD {
            while let Some(n) = self.next_skip_ml_comment() {
                if n.get_inner().is_ident() {
                    reporter.add(
                        WjlError::ast(n.start)
                            .set_end_char(n.end)
                            .message("Unexpected identifier")
                            .pot_fix("Did you mean to add a ., :: or generic argument?")
                            .ok(),
                    );
                    return None;
                }
                if ![Token::DOUBLE_COLON, Token::PERIOD].contains(&n.get_inner()) {
                    self.seek_back_skip_ml_comment();
                    break;
                }
                let ident = self.next_skip_ml_comment();
                if ident.is_none() {
                    reporter.add(
                        WjlError::ast(n.end)
                            .message("Expected identifier, got nothing.")
                            .ok(),
                    );
                    return None;
                }
                let ident = ident.unwrap();
                if !ident.get_inner().is_ident() {
                    reporter.add(
                        WjlError::ast(ident.start)
                            .set_end_char(ident.end)
                            .message(format!(
                                "Expected identifier, got {}",
                                ident.to_colored_str()
                            ))
                            .ok(),
                    );
                    return None;
                }
                let (txt, is_btick) = ident.get_inner().get_ident_inner();
                let next = self.peek_next_skip_ml_comment().0;
                let mut generic_args: GenericArgs = vec![];
                let mut start_ix = 0;
                let mut end_ix = 0;
                let mut is_opt = false;
                let mut is_nn = false;
                if next.is_some() {
                    let next = next.unwrap();
                    debug!("Next token {:?}", next.get_inner_ref());
                    start_ix = next.start;
                    if next.get_inner_ref() == &Token::QMARK {
                        is_opt = true;
                    } else if next.get_inner_ref() == &Token::EXCL_MARK {
                        is_nn = true;
                    } else if next.get_inner_ref() == &Token::ANGLE_LEFT {
                        //TODO! depend on treat_generics_as_decl
                        // WE MIGHT BE in a generic argument
                        //self.next_skip_ml_comment();
                        debug!("Parsing generic argument in ident context");
                        let generic = self.parse_generic_argument(reporter, false);
                        if generic.is_err() {
                            error!("Failed to parse generic arg");
                            return None;
                        }
                        let generic = generic.unwrap();
                        if generic.is_some() {
                            let (generic_arg, was_comma) = generic.unwrap();
                            let end = generic_arg.end;
                            generic_args.push(generic_arg);
                            if was_comma {
                                'generic: while let next = self.next_skip_ml_comment() {
                                    if next.is_none() {
                                        reporter.add(WjlError::ast(end).message("Expected comma or generic closing got nothing.").ok());
                                        return None;
                                    }
                                    let arg = self.parse_generic_argument(reporter, true);
                                    if arg.is_err() {
                                        return None;
                                    }
                                    let (arg, was_comma) = arg.unwrap().unwrap();
                                    generic_args.push(arg);
                                    if was_comma {
                                        continue;
                                    }
                                    end_ix = self.curr().unwrap().end;
                                    break 'generic;
                                }
                            }
                            let seek = self.get_index().unwrap();
                            let next = self.next_skip_ml_comment();
                            if next.is_some() {
                                let next = next.unwrap();
                                if next.get_inner_ref() == &Token::QMARK
                                    || next.get_inner_ref() == &Token::EXCL_MARK
                                {
                                    is_opt = next.get_inner_ref() == &Token::QMARK;
                                    is_nn = next.get_inner_ref() == &Token::EXCL_MARK;
                                } else {
                                    self.set_index(seek);
                                }
                            }
                        }
                    }
                }

                let ident = QualifiedIdentPart {
                    segment: txt,
                    generic_args: generic_args
                        .to_option()
                        .map(|x: GenericArgs| x.into_span(start_ix, end_ix)),
                    is_btick,
                    is_asserted_as_non_null: is_nn,
                    is_opt_chained_to_next: is_opt,
                    previous_link: Some(match n.get_inner() {
                        Token::DOUBLE_COLON => Connector::D_COL,
                        Token::PERIOD => Connector::PERIOD,
                        _ => unreachable!(),
                    }),
                }
                .to_span(n.start, ident.end);
                contents.push(ident);
            }
            return Some(contents.to_span(first_ident.start, contents.last().unwrap().end));
        }

        return Some(contents.to_span(first_ident.start, first_ident.end));
    }

    //assuming that we are on the opening < or a comma
    // Returns:
    // Ok(Some()) - generic was correctly parsed
    // Ok(None) - no generic
    // Err(()) - failed to parse generic, but its supposed to be there
    pub fn parse_generic_argument(
        &mut self,
        reporter: &mut ErrorReporter,
        strict: bool,
    ) -> Result<Option<(TSpan<Expression>, bool)>, ()> {
        debug!("Parsing generic");
        let curr = self.curr().unwrap();
        //DO NOT use peeking iterators
        let next_token = self.next_skip_ml_comment();
        if next_token.is_none() {
            reporter.add(
                WjlError::ast(curr.end)
                    .message("Expected identifier, expression or value, got nothing.")
                    .ok(),
            );
            return Err(());
        }
        let next_token = next_token.unwrap();
        if !next_token.get_inner_ref().is_ident()
            && next_token.get_inner_ref().is_static()
            && next_token.get_inner_ref() != &Token::BRACKET_LEFT
            && next_token.get_inner_ref() != &Token::KEYWORD_STRUCT
            && next_token.get_inner_ref() != &Token::KEYWORD_CLASS
        {
            if strict {
                reporter.add(
                    WjlError::ast(next_token.start)
                        .message("Unexpected token in generic argument.")
                        .set_end_char(next_token.end)
                        .ok(),
                );
                return Err(());
            }
            return Ok(None);
        }
        // we might have a generic
        // and we only accept a subset of expressions here
        let arg = opt_to_broad_err(self.parse_expr(reporter))?;
        let arg = arg;
        if arg.is_none() {
            return Err(());
        }
        let arg = arg.unwrap();
        let seek = self.get_index().unwrap();
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(
                WjlError::ast(curr.end)
                    .message(
                        "Expected comma, math operation or generic closure (`>`), got nothing.",
                    )
                    .ok(),
            );
            return Err(());
        }
        let next = next.unwrap();
        debug!(
            "Next token after generic arg expr {:?}",
            next.get_inner_ref()
        );
        if next.get_inner() == Token::ANGLE_RIGHT || next.get_inner() == Token::COMMA {
            //100% generic as <IDENT> does not make sense
            if next.get_inner_ref() == &Token::COMMA {
                self.set_index(seek);
            }
            return Ok(Some((arg, next.get_inner() == Token::COMMA)));
        }
        if strict {
            reporter.add(
                WjlError::ast(next_token.start)
                    .message("Unexpected token in generic argument.")
                    .set_end_char(next_token.end)
                    .ok(),
            );
            return Err(());
        }
        debug!("Stream delimited with < was not a generic.");
        //not a generic, discard the results TODO! save on parse time using symbol hashing and a cache (since we will obviously re-parse this expression later on if it aint a gen arg
        return Ok(None);
    }

    // expecting that we are on the use keyword
    pub fn parse_dep_import(
        &mut self,
        reporter: &mut ErrorReporter,
    ) -> Option<TSpan<DependencyImport>> {
        debug!("Parsing dep import");
        let start = self.curr().unwrap();
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(
                WjlError::ast(self.peek_prev_skip_ml_comment().0.unwrap().end)
                    .message("Expected identifier, got nothing.")
                    .ok(),
            );
            return None;
        }
        let path = self.parse_qualified_ident(reporter, false)?;
        if path.has_generic() {
            reporter.add(
                WjlError::ast(path.start)
                    .set_end_char(path.end)
                    .message("Generics are not allowed in imports.")
                    .ok(),
            );
            return None;
        }
        let next = self.next_skip_ml_comment();
        if next
            .as_ref()
            .map_or(true, |x| x.get_inner_ref() == &Token::DELIMITER)
        {
            let end = path.end;
            return Some(
                DependencyImport {
                    is_js: path.starts_with_str("js"),
                    path,
                    receiver: None,
                }
                .into_span(start.start, end),
            );
        }
        let receiver = self.parse_named_ref(reporter, false, false)?;
        let end = receiver.end;
        return Some(
            DependencyImport {
                is_js: path.starts_with_str("js"),
                path,
                receiver: Some(receiver),
            }
            .into_span(start.start, end),
        );
    }
    // expecting that we are on either an ident, a brace or a bracket
    pub fn parse_named_ref(
        &mut self,
        reporter: &mut ErrorReporter,
        allow_array: bool,
        allow_default: bool,
    ) -> Option<TSpan<NamedRef>> {
        debug!("Parsing named ref");
        let next = self.curr().unwrap();
        let res = if next.get_inner_ref().is_ident() {
            let parsed = self.parse_qualified_ident(reporter, true)?;
            Some(TSpan::wrap(
                parsed.start,
                parsed.end,
                NamedRef {
                    name: Some(parsed),
                    is_array_destruct: false,
                    is_object_destruct: false,
                    entries: None,
                },
            ))
        } else if next.get_inner() == Token::BRACE_LEFT {
            let parsed = self.parse_object_destruct_expr(reporter)?;
            Some(parsed)
        } else if next.get_inner() == Token::BRACKET_LEFT {
            if !allow_array {
                reporter.add(
                    WjlError::ast(next.start)
                        .message("Array destructuring is not allowed here.")
                        .ok(),
                );
                return None;
            }
            let parsed = self.parse_array_destruct_expr(reporter)?;
            Some(parsed)
        } else {
            reporter.add(
                WjlError::ast(next.start)
                    .message(format!(
                        "Expected identifier or destructuring expression, got {}.",
                        next.to_colored_str()
                    ))
                    .ok(),
            );
            None
        }?;
        if !allow_default && res.get_inner_ref().has_default() {
            reporter.add(
                WjlError::ast(res.start)
                    .set_end_char(res.end)
                    .message("Defaults are not allowed here.")
                    .ok(),
            );
            return None;
        }
        return Some(res);
    }

    pub fn parse_until_um_brace_or_eof(
        &mut self,
        reporter: &mut ErrorReporter,
        require_brace_delim: bool,
    ) -> Option<Vec<Span>> {
        debug!("Parsing generic stream");
        let mut stream = vec![];
        while let Some(span) = self.next_skip_ml_comment() {
            let tok = span.get_inner();
            if tok == Token::AT {
                let deco = self.parse_decorator(reporter)?;
                self.put_cache(Either::Left(deco));
                continue;
            }
            if tok == Token::MOD_KEYWORD_INTERNAL
                || tok == Token::MOD_KEYWORD_PROTECTED
                || tok == Token::MOD_KEYWORD_PUBLIC
                || tok == Token::MOD_KEYWORD_PRIVATE
                || tok == Token::MOD_KEYWORD_ONCE
                || tok == Token::KEYWORD_DECORATOR
                || tok == Token::KEYWORD_PURE
            {
                self.put_cache(Either::Right(span));
                continue;
            }
            if tok == Token::KEYWORD_USE {
                let imp = self.parse_dep_import(reporter)?;
                info!("Parsed import stmt {:?}", imp.get_inner_ref());
                stream.push(imp.map(Ast::IMPORT));
                continue;
            }
            //region: variable
            if [KEYWORD_VAL, KEYWORD_VAR, KEYWORD_CONST].contains(&tok) {
                let (decorators, access_mod, mods, mod_start) = self
                    .get_and_clear_cache()
                    .into_decorators_and_modifiers(reporter, span.start)?;
                let next = self.next_skip_ml_comment();
                if next.is_none() {
                    reporter.add(
                        WjlError::ast(span.end)
                            .message(
                                "Expected variable name or destructuring expression, got nothing.",
                            )
                            .ok(),
                    );
                    break;
                }
                let next = next.unwrap();

                let var_name = self.parse_named_ref(reporter, true, true)?;
                let next = self.next_skip_ml_comment();
                let needs_init = tok == KEYWORD_CONST || (tok == KEYWORD_VAL && !mods.is_once);
                if next.is_none() {
                    if needs_init {
                        reporter.add(
                            WjlError::ast(var_name.start)
                                .set_end_char(var_name.end + 1)
                                .message("Expected initializer or type hint, got nothing.")
                                .ok(),
                        );
                        break;
                    }
                    let end = var_name.end;
                    let decl = VariableDeclaration {
                        name: var_name,
                        scoping: access_mod,
                        initializer: None,
                        type_hint: None,
                        decorators,
                        kind: VarDeclKind::parse_uc(tok)
                            .maybe_once(mods.is_once)
                            .into_span(span.start, span.end),
                        is_pure: mods.is_pure,
                    };
                    stream.push(decl.into_span(mod_start, end).map(|x| Ast::VAR_DECL(x)));
                    continue;
                }
                let next = next.unwrap();
                let mut static_type: Option<TSpan<Expression>> = None;
                let mut initializer: Option<TSpan<Expression>> = None;
                let mut next_2 = Some(next.clone());
                if next.get_inner_ref() == &Token::COLON {
                    let type_hint = self.parse_type_annotation(reporter)?;
                    //type hint
                    static_type = Some(type_hint);
                    next_2 = self.next_skip_ml_comment();
                    debug!("Next after thint: {:?}", next_2);
                }

                if next_2.is_none() && needs_init {
                    let stt = static_type.unwrap(); //we know its here
                    reporter.add(
                        WjlError::ast(stt.end)
                            .message("Expected variable initializer, got nothing.")
                            .ok(),
                    );
                    break;
                }
                let next = next_2.unwrap();
                if next.get_inner_ref() == &Token::ASSIGN {
                    initializer = Some(self.parse_expr(reporter)?);
                } else if needs_init {
                    reporter.add(
                        WjlError::ast(var_name.start)
                            .set_end_char(var_name.end)
                            .message("Variable needs initializer")
                            .ok(),
                    );
                    break;
                }
                let end = initializer.as_ref().map_or_else(
                    || static_type.as_ref().map_or(var_name.end, |x| x.end),
                    |x| x.end,
                );
                let decl = VariableDeclaration {
                    name: var_name,
                    initializer: initializer.map(|x| Box::new(x)),
                    type_hint: static_type.map(|x| Box::new(x.map(|hint| Ast::TYPE_DEF(hint)))),
                    scoping: access_mod,
                    kind: VarDeclKind::parse_uc(tok)
                        .maybe_once(mods.is_once)
                        .into_span(span.start, span.end),
                    is_pure: mods.is_pure,
                    decorators,
                };

                stream.push(decl.into_span(mod_start, end).map(|x| Ast::VAR_DECL(x)))
            }
            //endregion
        }
        Some(stream)
    }
    // assuming we are on the [
    pub fn parse_arr_expr(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<StaticExpr>> {
        debug!("Parsing array expr");
        let start = self.curr().unwrap();
        let next = self.peek_next_skip_ml_comment().0;
        if next.is_none() {
            reporter.add(
                WjlError::lex(start.end)
                    .message("Unmatched array literal")
                    .pot_fix("Did you forget the ]?")
                    .ok(),
            );
            return None;
        }
        let next = next.unwrap();
        if next.get_inner() == Token::BRACKET_RIGHT {
            return Some(StaticExpr::ARR(vec![]).to_span(start.start, next.end));
        }
        let expr = self.parse_expr(reporter)?;
        let curr = self.curr().unwrap();
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(
                WjlError::lex(curr.end)
                    .message("Unmatched array literal")
                    .pot_fix("Did you forget the ]?")
                    .ok(),
            );
            return None;
        }

        let next = next.unwrap();
        if next.get_inner() == Token::BRACKET_RIGHT {
            return Some(StaticExpr::ARR(vec![expr]).to_span(start.start, next.end));
        }
        if next.get_inner() == Token::COMMA {
            let mut exprs = vec![expr];
            while let Some(tok) = self.next_skip_ml_comment() {
                let expr = self.parse_expr(reporter)?;
                exprs.push(expr);
                let next = self.next_skip_ml_comment();
                if next.is_none() {
                    reporter.add(WjlError::lex(tok.end).message("Unexpected token. Expected comma or array literal closure (]), got nothing.").pot_fix("Did you forget the ]?").ok());
                    return None;
                }
                let next = next.unwrap();
                if next.get_inner() == Token::COMMA {
                    continue;
                }
                if next.get_inner() == Token::BRACKET_RIGHT {
                    break;
                }
                reporter.add(
                    WjlError::lex(next.start)
                        .message("Unexpected token. Expected comma or array literal closure (])")
                        .pot_fix("Did you forget the ]?")
                        .ok(),
                );
                return None;
            }
            return Some(StaticExpr::ARR(exprs).to_span(start.start, self.curr().unwrap().end));
        }
        reporter.add(
            WjlError::lex(next.end)
                .message("Unexpected token. Expected comma or array literal closure (])")
                .pot_fix("Did you forget the ]?")
                .ok(),
        );
        return None;
    }

    pub fn ecx_parse_w_preceeding_expr(
        &mut self,
        reporter: &mut ErrorReporter,
        expr: TSpan<Expression>,
    ) -> Option<TSpan<Expression>> {
        debug!(
            "Parsing expression with preceeding expression: {:?}",
            expr.get_inner_ref()
        );
        let seek_b = self.get_index().unwrap();
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            let (start, end) = (expr.start, expr.end);
            return Some(expr);
        }
        let next = next.unwrap();
        if next.get_inner_ref() == &Token::PAREN_LEFT {
            return Some(
                self.parse_paren_func_call(reporter, expr)?
                    .map(Expression::FUNC_CALL),
            );
        }
        if next.get_inner_ref() == &Token::BRACE_LEFT {
            return Some(
                self.parse_struct_init_or_code_block_fcall(reporter, expr)?
                    .map(Expression::STRUCT_INIT),
            );
        }
        if next.get_inner_ref() == &Token::PIPE_OP {}
        if next.get_inner_ref().is_math() {
            let start = expr.start;
            let right = self.parse_expr(reporter)?;
            let end = right.end;
            let op = next.get_inner_ref().as_logical();
            return Some(
                Expression::LOGICAL(LogExpr {
                    left: Box::new(expr),
                    right: Box::new(right),
                    op: op.into_span(next.start, next.end),
                })
                .into_span(start, end),
            );
        }
        if next.get_inner_ref().is_binop() {
            let start = expr.start;
            let right = self.parse_expr(reporter)?;
            let end = right.end;
            let op = next.get_inner_ref().as_binop_bool();
            return Some(
                Expression::BOOLEAN(BooleanExpression {
                    left: Box::new(expr),
                    right: Box::new(right),
                    op: op.into_span(next.start, next.end),
                })
                .into_span(start, end),
            );
        }
        if next.get_inner_ref() == &Token::OP_ELVIS {
            let start = expr.start;
            let right = self.parse_expr(reporter)?;
            let end = right.end;
            return Some(
                Expression::ELVIS(ElvisExpr {
                    left: Box::new(expr),
                    right: Box::new(right),
                })
                .into_span(start, end),
            );
        }
        self.set_index(seek_b);
        return Some(expr);
    }

    // expecting that we are on the comma/paren
    pub fn parse_func_arg(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<Expression>> {
        let next = self.peek_next_skip_ml_comment().0;
        if next.is_none() {
            reporter.add(WjlError::ast(self.curr().unwrap().end).message("Expected expression or non positional argument, got nothing.").ok());
            return None
        }
        let next = next.unwrap();
        if next.get_inner_ref().is_ident() {
            self.next_skip_ml_comment();
            let ident = self.parse_qualified_ident(reporter, false)?;
            let next = self.peek_next_skip_ml_comment().0;
            if next.is_none() {
                reporter.add(WjlError::ast(ident.end).message("Expected comma, assignment or closing parenthesis, got nothing.").ok());
                return None
            }
            let next = next.unwrap();
            if [Token::COMMA, Token::PAREN_RIGHT].contains(&next.get_inner_ref()) {
                let (s, e) = (ident.start, ident.end);
                return Some(Expression::IDENT(ident).into_span(s, e))
            }
            if next.get_inner_ref() == &Token::ASSIGN {
                self.next_skip_ml_comment();
                let assigned_val = self.parse_expr(reporter)?;
                let start = ident.start;
                let expr = Expression::NONPOS_ARG(ident, Box::new(assigned_val.get_inner())).into_span(start, assigned_val.end);
                return Some(expr)
            }
            reporter.add(WjlError::ast(next.start).set_end_char(next.end).message("Expected comma, assignment or closing parenthesis.").ok());
            return None
        }
        self.parse_expr(reporter)

    }

    pub fn parse_expr_func_call(
        &mut self,
        reporter: &mut ErrorReporter,
        reference: TSpan<Expression>,
    ) {
        debug!("Parsing func call expr");
    }
    // expecting that we are on the paren
    pub fn parse_paren_func_call(
        &mut self,
        reporter: &mut ErrorReporter,
        reference: TSpan<Expression>,
    ) -> Option<TSpan<FunctionCallExpr>> {
        debug!("Parsing paren func call");
        let start = reference.start;
        let paren = self.peek_next_skip_ml_comment().0;
        if paren.is_none() {
            reporter.add(
                WjlError::ast(reference.end)
                    .message("Expected comma or ), got nothing.")
                    .ok(),
            );
            return None;
        }
        let p = paren.unwrap();
        if p.get_inner_ref() == &Token::PAREN_RIGHT {
            return Some(
                FunctionCallExpr {
                    reference: Box::new(reference),
                    arguments: vec![],
                }
                .into_span(start, p.end),
            );
        }

        let argument1 = self.parse_func_arg(reporter)?;
        let ix = self.get_index().unwrap();
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(
                WjlError::ast(argument1.end)
                    .message("Expected comma or ), got nothing.")
                    .ok(),
            );
            return None;
        }
        let next = next.unwrap();
        if next.get_inner_ref() == &Token::PAREN_RIGHT {
            return Some(
                FunctionCallExpr {
                    reference: Box::new(reference),
                    arguments: vec![argument1],
                }
                .into_span(start, next.end),
            );
        }
        if next.get_inner_ref() != &Token::COMMA {
            reporter.add(
                WjlError::ast(next.start)
                    .set_end_char(next.end)
                    .message("Expected comma or ).")
                    .ok(),
            );
            return None;
        }
        self.set_index(ix);
        let mut args = vec![argument1];
        let mut end = 0;
        while let next = self.next_skip_ml_comment() {
            if next.is_none() {
                reporter.add(
                    WjlError::ast(args.pop().unwrap().end)
                        .message("Expected comma or ), got nothing.")
                        .ok(),
                );
                return None;
            }
            let next = next.unwrap();
            if next.get_inner_ref() == &Token::COMMA {
                let arg = self.parse_func_arg(reporter)?;
                args.push(arg);
                continue;
            }
            if next.get_inner_ref() == &Token::PAREN_RIGHT {
                end = next.end;
                break;
            }
            reporter.add(
                WjlError::ast(next.start)
                    .set_end_char(next.end)
                    .message("Expected comma or ).")
                    .ok(),
            );
            return None;
        }
        Some(
            FunctionCallExpr {
                arguments: args,
                reference: Box::new(reference),
            }
            .into_span(start, end),
        )
    }

    fn qualified_ident_to_expr(ident: QualifiedIdent) -> TSpan<Expression> {
        let (start, end) = (ident.start, ident.end);
        return Expression::IDENT(ident).into_span(start, end);
    }

    // expecting that we are before the expr
    // expression kinds that we be parsing here:
    pub fn parse_expr(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<Expression>> {
        let start = self.curr().unwrap();
        info!("Parsing expr from {:?}", start.get_inner_ref());
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(
                WjlError::ast(start.end)
                    .message("Expected expression, got nothing.")
                    .ok(),
            );
            return None;
        }
        let next = next.unwrap();
        let first_part = if next.get_inner_ref() == &Token::PAREN_LEFT {
            let wrapped = self.parse_expr(reporter)?;
            wrapped.map(|x| Expression::GROUPED(Box::new(x)))
        } else if next.get_inner_ref().is_ident() {
            let ident = self.parse_qualified_ident(reporter, false)?;
            let (start, end) = (ident.start, ident.end);
            let expr = self.ecx_parse_w_preceeding_expr(
                reporter,
                Expression::IDENT(ident).into_span(start, end),
            )?;
            expr
        } else if next.get_inner_ref().is_static() {
            Expression::STATIC(next.get_inner().as_static_unchecked(reporter)?)
                .into_span(next.start, next.end)
        } else if next.get_inner_ref() == &Token::KEYWORD_AWAIT {
            let wrapped = self.parse_expr(reporter)?;
            wrapped.map(|x| Expression::AWAIT(Box::new(x)))
        } else if next.get_inner_ref() == &Token::KEYWORD_CLASS {
            self.parse_class_decl(reporter, true)?
        } else if next.get_inner_ref() == &Token::KEYWORD_IF {
            self.parse_if_expr_or_stmt(reporter, true)?
        } else if next.get_inner_ref() == &Token::WJL_COMPILER_PLACEHOLDER {
            Expression::WJL_PLACEHOLDER.into_span(next.start, next.end)
        } else if next.get_inner_ref() == &Token::OP_SPREAD {
            self.parse_spread_expr(reporter)?
        } else if next.get_inner_ref() == &Token::KEYWORD_TRY {
            self.parse_try_catch(reporter)?
        } else if next.get_inner_ref() == &Token::INCR {
            self.parse_incr_or_decr(reporter, true)?
        } else if next.get_inner_ref() == &Token::KEYWORD_MATCH {
            self.parse_match_expr(reporter)?
        } else if next.get_inner_ref() == &Token::DECR {
            self.parse_incr_or_decr(reporter, false)?
        } else if next.get_inner_ref() == &Token::KEYWORD_FUNC {
            self.parse_func_decl(reporter)?
        } else if next.get_inner_ref() == &Token::BRACKET_LEFT {
            self.parse_arr_expr(reporter)?.map(Expression::STATIC)
        } else {
            //TODO! bool invert expr somehow, make it fast
            reporter.add(
                WjlError::ast(start.start)
                    .set_end_char(start.end)
                    .message("Expected expression.")
                    .ok(),
            );
            return None;
        };
        return Some(first_part);
        return None;
    }
    // expecting we are before the annotation (on a colon in most cases)
    // correct examples of typing:
    // : 10
    // : "wstri"
    // : IntRange<a, b>
    // : T : ToString ? 10 : 20
    // : T : ToString + A ? 10 : 20
    // : <Ident>
    // : <Static>
    pub fn parse_type_annotation(
        &mut self,
        reporter: &mut ErrorReporter,
    ) -> Option<TSpan<Expression>> {
        debug!("Parsing type annotation");

        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(
                WjlError::ast(self.peek_prev_skip_ml_comment().0.unwrap().end)
                    .message("Expected type annotation, got nothing.")
                    .ok(),
            );
            return None;
        }
        let next = next.unwrap();
        let start_ix = next.start;
        let start = if next.get_inner_ref().is_static() {
            Expression::STATIC(next.get_inner().as_static_unchecked(reporter)?)
                .into_span(start_ix, next.end)
        } else if next.get_inner_ref().is_ident() {
            let ident = self.parse_qualified_ident(reporter, false)?;
            let end = ident.end;
            Expression::IDENT(ident).into_span(start_ix, end)
        } else {
            reporter.add(
                WjlError::ast(next.start)
                    .set_end_char(next.end)
                    .message("Expected identifier or static expression.")
                    .ok(),
            );
            return None;
        };
        let seek = self.get_index().unwrap();
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            return Some(start);
        }
        let next = next.unwrap();
        if next.get_inner_ref() != &Token::COLON {
            self.set_index(seek);
            return Some(start);
        }
        let constraint = self.parse_constraint(reporter, start)?;
        let next = self.peek_next_skip_ml_comment().0;
        let (start, end) = (constraint.start, constraint.end);
        let constraint =
            Expression::TYPE_CONSTRAINT(Box::new(constraint.get_inner())).into_span(start, end);
       // dbg!(&next);
        if next
            .map_or(true, |x| x.get_inner_ref() != &Token::QMARK)
            .eq(&true)
        {
            return Some(constraint);
        }
        self.next_skip_ml_comment(); //go to qmark
        let ternary = self.parse_ternary_expr(reporter, constraint)?;
        let (start, end) = (ternary.start, ternary.end);
        Some(Expression::TERNARY(ternary.get_inner()).into_span(start, end))
    }

    // expecting that we are on the colon from where the constraint is expected
    pub fn parse_constraint(
        &mut self,
        reporter: &mut ErrorReporter,
        left: TSpan<Expression>,
    ) -> Option<TSpan<TypeConstraintExpr>> {
        debug!("Parsing type constraint");
        let colon = self.curr().unwrap();
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(
                WjlError::ast(colon.end)
                    .message("Expected identifier, got nothing.")
                    .ok(),
            );
            return None;
        }
        let next = next.unwrap();
        if !next.get_inner_ref().is_ident() {
            reporter.add(
                WjlError::ast(next.start)
                    .set_end_char(next.end)
                    .message("Expected identifier.")
                    .ok(),
            );
            return None;
        }
        let first = self.parse_qualified_ident(reporter, false)?;
        let (start, end) = (first.start, first.end);
        let next = self.curr();
        if next.is_none() {
            return Some(
                TypeConstraintExpr {
                    receiver: left,
                    constraints: vec![TypeConstraintExprPart {
                        constraint: first,
                        next_join: None,
                    }
                    .into_span(start, end)],
                }
                .into_span(colon.start, end),
            );
        }
        let next = next.unwrap();
        if next.get_inner_ref() != &Token::PIPE && next.get_inner_ref() != &Token::SUM {
            return Some(
                TypeConstraintExpr {
                    receiver: left,
                    constraints: vec![TypeConstraintExprPart {
                        constraint: first,
                        next_join: None,
                    }
                    .into_span(start, end)],
                }
                .into_span(colon.start, end),
            );
        }
        let to_constraint_op = |x: &Token| match x {
            Token::SUM => BinOp::AND,
            Token::PIPE => BinOp::OR,
            _ => unreachable!(),
        };

        let mut constraints = vec![TypeConstraintExprPart {
            constraint: first,
            next_join: Some(to_constraint_op(next.get_inner_ref())),
        }
        .into_span(start, end)];

        self.next_skip_ml_comment();
        let mut o_end = 0;
        while let next = self.next_skip_ml_comment() {
            if next.is_none() {
                reporter.add(
                    WjlError::ast(constraints.pop().unwrap().end)
                        .message("Expected identifier, got nothing.")
                        .ok(),
                );
                return None;
            }
            let next = next.unwrap();
            if !next.get_inner_ref().is_ident() {
                reporter.add(
                    WjlError::ast(next.start)
                        .set_end_char(next.end)
                        .message("Expected identifier.")
                        .ok(),
                );
                return None;
            }
            let constraint = self.parse_qualified_ident(reporter, false)?;
            let next = self.peek_next_skip_ml_comment().0;
            if next
                .as_ref()
                .map_or(true, |x| {
                    x.get_inner_ref() != &Token::SUM && x.get_inner_ref() != &Token::PIPE
                })
                .eq(&true)
            {
                let start = constraint.start;
                let end = constraint.end;
                o_end = end;
                constraints.push(
                    TypeConstraintExprPart {
                        constraint,
                        next_join: None,
                    }
                    .into_span(start, end),
                );
                break;
            }
            let next = next.unwrap();
            let end = next.end;
            o_end = end;
            constraints.push(
                TypeConstraintExprPart {
                    constraint,
                    next_join: Some(to_constraint_op(next.get_inner_ref())),
                }
                .into_span(start, end),
            );
        }
        let start = left.start;
        return Some(
            TypeConstraintExpr {
                receiver: left,
                constraints,
            }
            .into_span(start, end),
        );
    }

    // expecting that we are on the question mark
    pub fn parse_ternary_expr(
        &mut self,
        reporter: &mut ErrorReporter,
        condition: TSpan<Expression>,
    ) -> Option<TSpan<TernaryExpr>> {
        debug!("Parsing ternary expr");
        let left_expr = self.parse_expr(reporter)?;
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(
                WjlError::ast(left_expr.end)
                    .message("Expected colon, got nothing.")
                    .ok(),
            );
            return None;
        }
        let next = next.unwrap();
        if next.get_inner_ref() != &Token::COLON {
            reporter.add(
                WjlError::ast(next.start)
                    .set_end_char(next.end)
                    .message("Expected colon.")
                    .ok(),
            );
            return None;
        }
        let right_expr = self.parse_expr(reporter)?;
        let start = condition.start;
        let end = right_expr.end;
        Some(
            TernaryExpr {
                condition: Box::new(condition),
                left: Box::new(left_expr),
                right: Box::new(right_expr),
            }
            .into_span(start, end),
        )
    }

    pub fn parse_func_decl(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<Expression>> {
        todo!()
    }

    pub fn parse_incr_or_decr(
        &mut self,
        reporter: &mut ErrorReporter,
        is_incr: bool,
    ) -> Option<TSpan<Expression>> {
        todo!()
    }

    pub fn parse_try_catch(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<Expression>> {
        todo!()
    }

    pub fn parse_spread_expr(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<Expression>> {
        todo!()
    }

    // expecting that we are on the as kw
    pub fn parse_typecast(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<Expression>> {
        todo!()
    }

    // expecting that we are on the left brace
    pub fn parse_struct_init_or_code_block_fcall(
        &mut self,
        reporter: &mut ErrorReporter,
        expr: TSpan<Expression>,
    ) -> Option<TSpan<StructInitExpr>> {
        todo!()
    }

    // expecting that we are on the struct kw
    pub fn parse_struct_decl(
        &mut self,
        reporter: &mut ErrorReporter,
    ) -> Option<TSpan<StructDeclExpr>> {
        let kw = self.curr().unwrap();
        let struct_start = kw.start;
        let next = self.next_skip_ml_comment();
        let modifiers = self.get_and_clear_cache();
        if next.is_none() {
            reporter.add(
                WjlError::ast(kw.end)
                    .message("Expected identifier, got nothing.")
                    .ok(),
            );
            return None;
        }

        let next = next.unwrap();
        if !next.get_inner().is_ident() {
            reporter.add(WjlError::ast(kw.end).message("Expected identifier.").ok());
            return None;
        }
        let struct_name = self.parse_qualified_ident(reporter, true)?;
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(
                WjlError::ast(struct_name.end)
                    .message("Expected {, got nothing.")
                    .ok(),
            );
            return None;
        }
        let next = next.unwrap();
        if next.get_inner() != Token::BRACE_LEFT {
            reporter.add(
                WjlError::ast(next.start)
                    .message("Expected {.")
                    .set_end_char(next.end)
                    .ok(),
            );
            return None;
        }
        let mut fields = vec![];
        let fields_start = next.end;
        let mut end = 0_usize;
        while let next = self.next_skip_ml_comment() {
            let mut local_start = 0_usize;
            if next.is_none() {
                let prev = self.peek_prev().unwrap().end;
                reporter.add(
                    WjlError::ast(prev)
                        .message("Expected identifier or }, got nothing.")
                        .ok(),
                );
                return None;
            }
            let next = next.unwrap();
            if next.get_inner() == Token::BRACE_RIGHT {
                break;
            }
            local_start = next.start;
            let mut visibility = Token::MOD_KEYWORD_PRIVATE;
            if !next.get_inner().is_ident() {
                if next.get_inner_ref() == &Token::MOD_KEYWORD_PUBLIC
                    || next.get_inner_ref() == &Token::MOD_KEYWORD_PRIVATE
                {
                    visibility = next.get_inner();
                    let ident = self.next_skip_ml_comment();
                    if ident.is_none() {
                        reporter.add(
                            WjlError::ast(next.end)
                                .message("Expected identifier, got nothing.")
                                .ok(),
                        );
                        return None;
                    }
                    let ident = ident.unwrap();
                    if !ident.get_inner_ref().is_ident() {
                        reporter.add(
                            WjlError::ast(ident.start)
                                .set_end_char(ident.end)
                                .message("Expected identifier.")
                                .ok(),
                        );
                        return None;
                    }
                    local_start = ident.start;
                } else {
                    reporter.add(
                        WjlError::ast(next.start)
                            .set_end_char(next.end)
                            .message("Expected identifier or public/private modifier.")
                            .ok(),
                    );
                    return None;
                }
            }
            let key_ident = self.parse_qualified_ident(reporter, false)?;
            if !key_ident.is_single() {
                reporter.add(
                    WjlError::ast(key_ident.start)
                        .set_end_char(key_ident.end)
                        .message("Paths and generics are not allowed as struct properties.")
                        .ok(),
                );
                return None;
            }
            let visibility = match visibility {
                Token::MOD_KEYWORD_PUBLIC => VisibilityScope::PUBLIC,
                Token::MOD_KEYWORD_PRIVATE => VisibilityScope::PRIVATE,
                _ => unreachable!(),
            };
            let key = key_ident.get_inner().first().unwrap().get_inner().segment;
            let next = self.next_skip_ml_comment();
            if next.is_none() {
                reporter.add(
                    WjlError::ast(key_ident.end)
                        .message("Expected colon or question mark, got nothing.")
                        .ok(),
                );
                return None;
            }
            let mut qmark_or_colon = next.unwrap();
            let is_optional = if qmark_or_colon.get_inner() == Token::QMARK {
                let next = self.next_skip_ml_comment();
                if next.is_none() {
                    reporter.add(
                        WjlError::ast(qmark_or_colon.end)
                            .message("Expected colon, got nothing.")
                            .ok(),
                    );
                    return None;
                }
                qmark_or_colon = next.unwrap();
                true
            } else {
                false
            };
            if qmark_or_colon.get_inner_ref() != &Token::COLON {
                reporter.add(
                    WjlError::ast(qmark_or_colon.start)
                        .set_end_char(qmark_or_colon.end)
                        .message("Expected colon.")
                        .ok(),
                );
                return None;
            }
            let field_type = self.parse_type_annotation(reporter)?;
            end = field_type.end;

            let default = if self
                .next_skip_ml_comment()
                .map_or(false, |x| x.get_inner_ref() == &Token::ASSIGN)
                .eq(&true)
            {
                let seek = self.get_index().unwrap();
                let next = self.next_skip_ml_comment();
                if next.is_none() {
                    reporter.add(
                        WjlError::ast(field_type.end)
                            .message("Expected expression, got nothing.")
                            .ok(),
                    );
                    return None;
                }
                end = next.unwrap().end;
                self.set_index(seek);
                Some(self.parse_expr(reporter))?
            } else {
                None
            };

            fields.push(
                ObjectLikeFieldDeclaration {
                    name: key,
                    is_optional,
                    default,
                    field_type,
                    visibility,
                }
                .to_span(local_start, end),
            );

            let next = self.next_skip_ml_comment();
            if next.is_none() {
                reporter.add(
                    WjlError::ast(end)
                        .message("Expected comma or closing brace, got nothing.")
                        .ok(),
                );
                return None;
            }
            let next = next.unwrap();
            if next.get_inner_ref() == &Token::BRACE_RIGHT {
                end = next.end;
                break;
            }
            if next.get_inner_ref() == &Token::COMMA {
                continue;
            }
            reporter.add(
                WjlError::ast(next.start)
                    .set_end_char(next.end)
                    .message("Expected comma or closing brace.")
                    .ok(),
            );
        }
        return Some(
            StructDeclExpr {
                name: struct_name,
                fields: fields.to_span(fields_start, end),
            }
            .to_span(struct_start, end),
        );
    }

    pub fn parse_match_expr(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<Expression>> {
        todo!()
    }

    // expecting that we are on the enum kw
    pub fn parse_enum_decl(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<Expression>> {
        todo!()
    }

    // expecting that we are on the class kw
    pub fn parse_class_decl(
        &mut self,
        reporter: &mut ErrorReporter,
        can_be_anon: bool,
    ) -> Option<TSpan<Expression>> {
        todo!()
    }

    pub fn parse_if_expr_or_stmt(
        &mut self,
        reporter: &mut ErrorReporter,
        needs_catchall: bool,
    ) -> Option<TSpan<Expression>> {
        todo!()
    }

    pub fn parse_yield_or_return(
        &mut self,
        reporter: &mut ErrorReporter,
    ) -> Option<TSpan<Expression>> {
        todo!()
    }

    // expecting that we are on the @
    pub fn parse_decorator(
        &mut self,
        reporter: &mut ErrorReporter,
    ) -> Option<TSpan<AppliedDecoratorExpr>> {
        let ident = self.next_skip_ml_comment();
        if ident.is_none() {
            reporter.add(WjlError::ast(self.curr().unwrap().end).message("Expected identifier for decorator, got nothing.").ok());
            return None
        }
        let ident = ident.unwrap();
        if !ident.get_inner_ref().is_ident() {
            reporter.add(WjlError::ast(self.curr().unwrap().end).message("Expected identifier for decorator.").ok());
            return None
        }
        let ident = self.parse_qualified_ident(reporter, false)?;
        let next = self.peek_next_skip_ml_comment().0;
        if next.map_or(false, |x| x.get_inner_ref() == &Token::PAREN_LEFT).eq(&true) {
            self.next_skip_ml_comment();
            let ident_c = ident.clone();
            let start = ident_c.start;
            let dec = self.parse_paren_func_call(reporter, Expression::IDENT(ident).into_span(ident_c.start, ident_c.end))?;
            let end = dec.end;
            return Some(AppliedDecoratorExpr {
                decorator: ident_c,
                args: Some(dec.get_inner().arguments)
            }.into_span(start, end))
        }

        let (s, e) = (ident.start, ident.end);
        return Some(AppliedDecoratorExpr {
            decorator: ident,
            args: None
        }.into_span(s, e));
    }
}

impl ToAst for Vec<TokenSpan> {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Span> {
        let mut iter = wrap_iter(self);
        iter.parse_until_um_brace_or_eof(reporter, false)
            .unwrap_or(vec![])
    }
}
pub struct Modifiers {
    is_once: bool,
    is_pure: bool,
}
trait IntoDecoratorsAndModifiers {
    fn into_decorators_and_modifiers(
        self,
        reporter: &mut ErrorReporter,
        fallback_start: usize,
    ) -> Option<(
        Vec<TSpan<AppliedDecoratorExpr>>,
        VisibilityScope,
        Modifiers,
        usize,
    )>;
}

impl IntoDecoratorsAndModifiers for Vec<Either<TSpan<AppliedDecoratorExpr>, TokenSpan>> {
    fn into_decorators_and_modifiers(
        self,
        reporter: &mut ErrorReporter,
        fallback_start: usize,
    ) -> Option<(
        Vec<TSpan<AppliedDecoratorExpr>>,
        VisibilityScope,
        Modifiers,
        usize,
    )> {
        let mut decos = vec![];
        let mut visibility = VisibilityScope::PRIVATE;
        let mut was_vis_set = false;
        let mut mods = Modifiers {
            is_pure: false,
            is_once: false,
        };
        let mut start = {
            let f = self.first();
            let f = f.as_ref();
            f.map_or(fallback_start, |x| {
                let mapped = x.as_ref().map_left(|x| x.start).map_right(|x| x.start);
                mapped.left().unwrap_or_else(|| mapped.right().unwrap())
            })
        };

        for entry in self.into_iter() {
            if entry.is_left() {
                decos.push(entry.left().unwrap());
                continue;
            }
            let tok = entry.right().unwrap();
            if [
                Token::MOD_KEYWORD_INTERNAL,
                Token::MOD_KEYWORD_PRIVATE,
                Token::MOD_KEYWORD_PUBLIC,
                Token::MOD_KEYWORD_PROTECTED,
            ]
            .contains(tok.get_inner_ref())
            {
                if was_vis_set {
                    reporter.add(
                        WjlError::ast(tok.start)
                            .set_end_char(tok.end)
                            .message("Cannot have 2 access modifiers on 1 declaration.")
                            .pot_fix("Remove the modifier.")
                            .ok(),
                    );
                    return None;
                }
                visibility = match tok.get_inner_ref() {
                    Token::MOD_KEYWORD_PUBLIC => VisibilityScope::PUBLIC,
                    Token::MOD_KEYWORD_PRIVATE => VisibilityScope::PRIVATE,
                    Token::MOD_KEYWORD_PROTECTED => VisibilityScope::PROTECTED,
                    Token::MOD_KEYWORD_INTERNAL => VisibilityScope::INTERNAL,
                    _ => unreachable!(),
                };
                was_vis_set = true;
                continue;
            }
            if tok.get_inner_ref() == &Token::KEYWORD_PURE {
                mods.is_pure = true;
                continue;
            }
            if tok.get_inner_ref() == &Token::MOD_KEYWORD_ONCE {
                mods.is_once = true;
                continue;
            }
            reporter.add(WjlError::ast(tok.start).set_end_char(tok.end)
                .message("Unexpected token. This is a mistake left by the authors, you should never encounter this error in normal circumstances. Please open a GitHub issue.")
                .pot_fix(format!("Remove {:?}", tok.get_inner_ref()))
                .ok());
            return None;
        }

        return Some((decos, visibility, mods, start));
    }
}
