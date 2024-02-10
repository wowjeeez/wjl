use either::Either;
use crate::ast::nodes::expression::Expression;
use crate::ast::nodes::qualified_ident::{Connector, QualifiedIdent, QualifiedIdentPart};
use crate::ast::nodes::statics::StaticExpr;
use crate::ast::nodes::variable::{Assignment, DestructuringEntry, ModifiersAndDecorators, NamedRef, VariableDeclaration, VisibilityScope};
use crate::errors::{ErrorReporter, WjlError};
use crate::helpers::Triple;
use crate::iter::{GenericIterator, PeekableIterator, wrap_iter};
use crate::tokens::span::{IntoSpan, Span as TSpan};
use crate::tokens::Token;
use crate::tokens::Token::{KEYWORD_CONST, KEYWORD_VAL, KEYWORD_VAR};

pub type TokenSpan = TSpan<Token>;
pub type Span = TSpan<Ast>;

#[derive(Debug, Clone)]
#[allow(non_camel_case_types, unused)]
pub enum Ast {
    VAR_DECL(VariableDeclaration),
    ASSIGNMENT(Assignment),
}

pub trait ToAst {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Span>;
}

impl PeekableIterator<TokenSpan> {
    pub fn next_if_skip_ml(&mut self, eq_tok: Token) -> Option<TokenSpan> {
        self.next_skip_ml_comment().map(|x| if x.get_inner() == eq_tok {
            Some(x)
        } else { None }).flatten()
    }

    pub fn next_skip_ml_comment(&mut self) -> Option<TokenSpan> {
        let next = self.next();
        if next.as_ref().map_or(false, |x| match x.get_inner() {
            Token::COMMENT_ML(_) => true,
            _ => false
        }).eq(&true) {
            return self.next_skip_ml_comment();
        }
        next
    }

    pub fn seek_back_skip_ml_comment(&mut self) {
        let prev = self.get_index().unwrap() - 1;
        self.set_index(prev);
        let curr = self.get_content().get(prev);
        if curr.as_ref().map_or(false, |x| match x.get_inner() {
            Token::COMMENT_ML(_) => true,
            _ => false
        }).eq(&true) {
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
        if prev.as_ref().map_or(false, |x| match x.get_inner() {
            Token::COMMENT_ML(_) => true,
            _ => false
        }).eq(&true) {
            return self.peek_n_skip_ml_comment(if by.is_negative() { by - 1 } else { by + 1 });
        }
        (prev, by)
    }

    // will consume from the iter expecting that we are on the {
    pub fn parse_object_destruct_expr(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<NamedRef>> {
        let before = self.get_index().unwrap();
        let start = self.curr().unwrap().start;
        let first_tok = self.next_skip_ml_comment();
        if first_tok.is_none() {
            let prev = self.peek_prev_skip_ml_comment().0.unwrap();;
            reporter.add(WjlError::ast(prev.end).message("Expected identifier, rest spread or closing }, got nothing.").ok());
            return None
        }
        let first_tok = first_tok.unwrap();
        if first_tok.get_inner() == Token::BRACE_RIGHT { //happy path cuz we aint gotta do shit
            return Some(NamedRef {
                entries: Some(vec![]),
                is_array_destruct: false,
                is_object_destruct: true,
                name: None
            }.to_span(start, first_tok.end))
        }
        if first_tok.get_inner() == Token::BRACKET_LEFT {
            reporter.add(WjlError::ast(first_tok.start)
                .message("Arbitrary [key] is not allowed in Wjl.")
                .pot_fix("Use explicit properties").ok()
            );
            return None
        }
        if first_tok.get_inner().is_ident() || first_tok.get_inner() == Token::OP_SPREAD {
            self.set_index(before); //we dont seek because there could have been a multiline comment inbetween
            let next_entry = self.parse_identifier_in_destruct(false, reporter)?;
            let mut entries = vec![next_entry];
            let next = self.peek_next_skip_ml_comment().0;
            if next.is_none() {
                reporter.add(WjlError::ast(first_tok.end).message("Expected closing or comma after first entry in destructuring, got nothing.").ok());
                return None
            }
            while let tok = self.next_skip_ml_comment() {
                if tok.is_none() {
                    reporter.add(WjlError::ast(first_tok.end).message("Expected closing or comma after first entry in destructuring, got nothing.").ok());
                    return None
                }
                let tok = tok.unwrap();
                if tok.get_inner() == Token::BRACE_RIGHT {
                    break
                }
                let entry = self.parse_identifier_in_destruct(false, reporter)?;
                entries.push(entry);
            }
            return Some(NamedRef {
                is_object_destruct: true,
                is_array_destruct: false,
                entries: Some(entries),
                name: None,
            }.to_span(start, self.curr().unwrap().end))
        }

        reporter.add(WjlError::ast(first_tok.start).message("Unexpected token.").ok());
        return None
    }

    // will consume from the iter expecting that we are on the [
    pub fn parse_array_destruct_expr(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<NamedRef>> {
        let before = self.get_index().unwrap();
        let start = self.curr().unwrap().start;
        let first_tok = self.next_skip_ml_comment();
        if first_tok.is_none() {
            let prev = self.peek_prev_skip_ml_comment().0.unwrap();;
            reporter.add(WjlError::ast(prev.end).message("Expected identifier, rest spread or closing ], got nothing.").ok());
            return None
        }
        let first_tok = first_tok.unwrap();
        if first_tok.get_inner() == Token::BRACKET_RIGHT { //happy path cuz we aint gotta do shit
            return Some(NamedRef {
                entries: Some(vec![]),
                is_array_destruct: true,
                is_object_destruct: false,
                name: None
            }.to_span(start, first_tok.end))
        }
        if first_tok.get_inner().is_ident() || first_tok.get_inner() == Token::OP_SPREAD || first_tok.get_inner() == Token::BRACKET_LEFT {
            self.set_index(before); //we dont seek because there could have been a multiline comment inbetween
            let next_entry = self.parse_identifier_in_destruct(true, reporter)?;
            let mut entries = vec![next_entry];
            let next = self.peek_next_skip_ml_comment().0;
            if next.is_none() {
                reporter.add(WjlError::ast(first_tok.end).message("Expected closing or comma after first entry in destructuring, got nothing.").ok());
                return None
            }
            while let tok = self.next_skip_ml_comment() {
                if tok.is_none() {
                    reporter.add(WjlError::ast(first_tok.end).message("Expected closing or comma after first entry in destructuring, got nothing.").ok());
                    return None
                }
                let tok = tok.unwrap();
                if tok.get_inner() == Token::BRACKET_RIGHT {
                    break
                }
                let entry = self.parse_identifier_in_destruct(true, reporter)?;
                entries.push(entry);
            }
            return Some(NamedRef {
                is_object_destruct: false,
                is_array_destruct: true,
                entries: Some(entries),
                name: None,
            }.to_span(start, self.curr().unwrap().end))
        }
        reporter.add(WjlError::ast(first_tok.start).message("Unexpected token.").ok());
        return None
    }

    // expecting that we are before the ident or rest arg
    pub fn parse_identifier_in_destruct(&mut self, in_arr: bool, reporter: &mut ErrorReporter) -> Option<TSpan<DestructuringEntry>> {
        let mut target = self.next_skip_ml_comment().unwrap(); //we know its there otherwise we dont call it
        let start = target.start;
        let mut inner = target.get_inner();
        let mut is_spread = false;
        if inner == Token::OP_SPREAD {
            is_spread = true;
            let next = self.next_skip_ml_comment();
            if next.is_none() {
                reporter.add(WjlError::ast(target.end).message("Expected identifier, got nothing.").ok());
                return None;
            }
            let next = next.unwrap();
            target = next;
            inner = target.get_inner();
        }
        if in_arr {
            if inner == Token::BRACKET_LEFT {
                let parsed = self.parse_array_destruct_expr(reporter)?;
                return Some(DestructuringEntry {
                    name: parsed,
                    default_value: None,
                    binding: None,
                    is_rest: is_spread,
                }.to_span(start, target.end));
            }
            if inner == Token::BRACE_LEFT {
                let parsed = self.parse_object_destruct_expr(reporter)?;
                return Some(DestructuringEntry {
                    name: parsed,
                    default_value: None,
                    binding: None,
                    is_rest: is_spread,
                }.to_span(start, target.end));
            }
        }

        if !inner.is_ident() {
            reporter.add(WjlError::ast(target.end).message("Expected identifier.").ok());
            return None;
        }
        let inner = target.as_qualified();
        let (next, end) = self.peek_next_skip_ml_comment();
        if next.is_none() {
            // safe to cast to usize
            reporter.add(WjlError::ast(target.end + end as usize)
                .message("Expected assignment, alias, comma or closing bracket (}, ]) got nothing.").ok());
            return None;
        }
        let next = next.unwrap();
        if next.get_inner() == Token::COMMA || next.get_inner() == if in_arr { Token::BRACKET_RIGHT } else { Token::BRACE_RIGHT } {
            let parsed = self.parse_qualified_ident(reporter)?;
            return Some(DestructuringEntry {
                is_rest: is_spread,
                name: NamedRef {
                    is_object_destruct: false,
                    is_array_destruct: false,
                    entries: None,
                    name: Some(parsed),
                }.to_span(target.start, target.end),
                default_value: None,
                binding: None,
            }.to_span(start, next.end));
        }
        self.next_skip_ml_comment(); //advance

        if next.get_inner() == Token::COLON {
            if in_arr {
                reporter.add(WjlError::ast(next.start)
                    .message("Binding is not allowed in array destructuring")
                    .pot_fix("Remove the :").ok());
                return None
            }
            let alias_tok = self.next_skip_ml_comment();
            if alias_tok.is_none() {
                reporter.add(WjlError::ast(next.end).message("Expected identifier or destructuring statement, got nothing.").ok());
                return None
            }
            let alias_tok = alias_tok.unwrap();
            if alias_tok.get_inner().is_ident() {
                let ident = self.parse_qualified_ident(reporter)?;
                return Some(DestructuringEntry {
                    binding: Some(NamedRef {
                        entries: None,
                        is_array_destruct: false,
                        is_object_destruct: false,
                        name: Some(ident)
                    }.to_span(alias_tok.start, alias_tok.end)),
                    is_rest: is_spread,
                    name: NamedRef {
                        entries: None,
                        is_array_destruct: false,
                        is_object_destruct: false,
                        name: Some(inner)
                    }.to_span(target.start, target.end),
                    default_value: None
                }.to_span(start, alias_tok.end))
            }
            if alias_tok.get_inner() == Token::BRACKET_LEFT {
                let binding = self.parse_array_destruct_expr(reporter)?;
                let end = binding.end;
                return Some(DestructuringEntry {
                    binding: Some(binding),
                    is_rest: is_spread,
                    name: NamedRef {
                        entries: None,
                        is_array_destruct: false,
                        is_object_destruct: false,
                        name: Some(inner)
                    }.to_span(target.start, target.end),
                    default_value: None
                }.to_span(start, end))
            }
            if alias_tok.get_inner() == Token::BRACE_LEFT {
                let binding = self.parse_object_destruct_expr(reporter)?;
                let end = binding.end;
                return Some(DestructuringEntry {
                    binding: Some(binding),
                    is_rest: is_spread,
                    name: NamedRef {
                        entries: None,
                        is_array_destruct: false,
                        is_object_destruct: false,
                        name: Some(inner)
                    }.to_span(target.start, target.end),
                    default_value: None
                }.to_span(start, end))
            }
        }

        if next.get_inner() == Token::ASSIGN {
            // default
            todo!("Parse expr here")
        }

        reporter.add(WjlError::lex(next.start).message("Unexpected token").ok());
        return None
    }

    pub fn reverse_collect_mod(&self, once_preceeding_decl: bool, reporter: &mut ErrorReporter) -> ModifiersAndDecorators {
        let curr = self.get_index().unwrap();
        let mut cloned = self.get_content().clone();
        let mut sliced = &mut cloned[0..curr];
        sliced.reverse();
        let once_accessmod_or_ident = sliced.first();
        if once_accessmod_or_ident.is_none() {
            return ModifiersAndDecorators {
                is_once: false,
                visibility: VisibilityScope::PRIVATE
            }
        }
        let mut once_accessmod_or_ident = once_accessmod_or_ident.unwrap();

        let inner = once_accessmod_or_ident.get_inner();
        let mut index = 1;
        let is_once = if once_preceeding_decl && inner == Token::MOD_KEYWORD_ONCE {
            let inner = sliced.get(index);
            index += 1;
            if inner.is_none() {
                return ModifiersAndDecorators {
                    visibility: VisibilityScope::PRIVATE,
                    is_once: true
                }
            }
            once_accessmod_or_ident = inner.unwrap();
            true
        } else if once_preceeding_decl {
            false
        } else if inner == Token::MOD_KEYWORD_ONCE {
            reporter.add(WjlError::ast(once_accessmod_or_ident.start).set_end_char(once_accessmod_or_ident.end)
                .message("Once can not appear here.").ok());
            false
        } else {false};

        let visibility = if [Token::MOD_KEYWORD_INTERNAL, Token::MOD_KEYWORD_PROTECTED, Token::MOD_KEYWORD_PUBLIC, Token::MOD_KEYWORD_PRIVATE].contains(&inner) {
            let scope = match inner {
                Token::MOD_KEYWORD_INTERNAL => VisibilityScope::INTERNAL,
                Token::MOD_KEYWORD_PROTECTED => VisibilityScope::PROTECTED,
                Token::MOD_KEYWORD_PUBLIC => VisibilityScope::PUBLIC,
                Token::MOD_KEYWORD_PRIVATE => VisibilityScope::PRIVATE,
                _ => unreachable!()
            };
            let inner = sliced.get(index);
            index += 1;
            if inner.is_none() {
                return ModifiersAndDecorators {
                    visibility: scope,
                    is_once
                }
            }
            once_accessmod_or_ident = inner.unwrap();
            scope
        } else {VisibilityScope::PRIVATE};

        return ModifiersAndDecorators {
            is_once,
            visibility
        }

    }

    // expecting that we are on the first ident
    pub fn parse_qualified_ident(&mut self, reporter: &mut ErrorReporter) -> Option<QualifiedIdent> {
        let first_ident = self.curr().unwrap();
        let next = self.peek_next_skip_ml_comment().0;
        let (ident, is_bt) = first_ident.get_inner().get_ident_inner();
        let ident = QualifiedIdentPart {
            segment: ident,
            is_btick: is_bt,
            previous_link: None
        }.to_span(first_ident.start, first_ident.end);
        if next.is_none() {
            return Some(vec![ident].to_span(first_ident.start, first_ident.end))
        }
        let mut contents = vec![ident];
        let next = next.unwrap();
        if next.get_inner() == Token::DOUBLE_COLON || next.get_inner() == Token::PERIOD {
            while let Some(n) = self.next_skip_ml_comment() {
                if n.get_inner().is_ident() {
                    reporter.add(WjlError::ast(n.start).set_end_char(n.end)
                        .message("Unexpected identifier")
                        .pot_fix("Did you mean to add a ., :: or generic argument?")
                        .ok());
                    return None
                }
                if ![Token::DOUBLE_COLON, Token::PERIOD].contains(&n.get_inner()) {
                    self.seek_back_skip_ml_comment();
                    break
                }
                let ident = self.next_skip_ml_comment();
                if ident.is_none() {
                    reporter.add(WjlError::ast(n.end).message("Expected identifier, got nothing.").ok());
                    return None
                }
                let ident = ident.unwrap();
                if !ident.get_inner().is_ident() {
                    reporter.add(WjlError::ast(ident.start).set_end_char(ident.end).message(format!("Expected identifier, got {}", ident.to_colored_str())).ok());
                    return None
                }
                let (txt, is_btick) = ident.get_inner().get_ident_inner();
                let next = self.peek_next_skip_ml_comment().0;
                if next.is_some() {
                    let next = next.unwrap();
                    if next.get_inner() == Token::ANGLE_LEFT {
                        // WE MIGHT BE in a generic argument
                        self.next_skip_ml_comment();
                        let generic = self.parse_generic_argument(reporter);
                    }
                }

                let ident = QualifiedIdentPart {
                    segment: txt,
                    is_btick,
                    previous_link: Some(match n.get_inner() {
                        Token::DOUBLE_COLON => Connector::D_COL,
                        Token::PERIOD => Connector::PERIOD,
                        _ => unreachable!()
                    })
                }.to_span(n.start, ident.end);
                contents.push(ident);
            }
            return Some(contents.to_span(first_ident.start, contents.last().unwrap().end))
        }


        return Some(contents.to_span(first_ident.start, first_ident.end))
    }

    //assuming that we are on the opening <
    // Returns:
    // Ok(Some()) - generic was correctly parsed
    // Ok(None) - no generic
    // Err(()) - failed to parse generic, but its supposed to be there
    pub fn parse_generic_argument(&mut self, reporter: &mut ErrorReporter) -> Result<Option<()>, ()> {
        let start_index = self.get_index().unwrap(); //we will rewind here if
        let curr = self.curr().unwrap();
        //DO NOT use peeking iterators
        let next_token = self.next_skip_ml_comment();
        if next_token.is_none() {
            reporter.add(WjlError::ast(curr.end).message("Expected identifier, expression or value, got nothing.").ok());
            return Err(())
        }
        let next_token = next_token.unwrap();
        if !next_token.get_inner().is_ident() && !next_token.get_inner().is_static() && !next_token.get_inner() != Token::BRACKET_LEFT && !next_token.get_inner() != Token::KEYWORD_STRUCT {
            return Ok(None)
        }
        // we might have a generic
        // and we only accept a subset of generics here
        let first_token = if next_token.get_inner().is_ident() {
            let ident = self.parse_qualified_ident(reporter);
            if ident.is_none() {
                return Err(())
            }
            let ident = ident.unwrap();
            Either::Left(ident)
        } else if next_token.get_inner().is_static() {
            let expr = next_token.get_inner().as_static_unchecked();
            Either::Right(expr)
        } else if next_token.get_inner() == Token::BRACKET_LEFT {

        } else {
            // struct keyword
        };
        return Ok(None)
    }

    // assuming we are on the [
    pub fn parse_arr_expr(&mut self, reporter: &mut ErrorReporter) -> Option<TSpan<StaticExpr>> {
        let start = self.curr().unwrap();
        let next = self.peek_next_skip_ml_comment().0;
        if next.is_none() {
            reporter.add(WjlError::lex(start.end).message("Unmatched array literal").pot_fix("Did you forget the ]?").ok());
            return None
        }
        let next = next.unwrap();
        if next.get_inner() == Token::BRACKET_RIGHT {
            return Some(StaticExpr::ARR(vec![]).to_span(start.start, next.end))
        }
        let expr = self.parse_expr(reporter)?;
        let curr = self.curr().unwrap();
        let next = self.next_skip_ml_comment();
        if next.is_none() {
            reporter.add(WjlError::lex(curr.end).message("Unmatched array literal").pot_fix("Did you forget the ]?").ok());
            return None
        }

        let next = next.unwrap();
        if next.get_inner() == Token::BRACKET_RIGHT {
            return Some(StaticExpr::ARR(vec![expr]).to_span(start.start, next.end))
        }
        if next.get_inner() == Token::COMMA {
            let mut exprs = vec![expr];
            while let Some(tok) = self.next_skip_ml_comment() {
                let expr = self.parse_expr(reporter)?;
                exprs.push(expr);
                let next = self.next_skip_ml_comment();
                if next.is_none() {
                    reporter.add(WjlError::lex(tok.end).message("Unexpected token. Expected comma or array literal closure (]), got nothing.").pot_fix("Did you forget the ]?").ok());
                    return None
                }
                let next = next.unwrap();
                if next.get_inner() == Token::COMMA {continue}
                if next.get_inner() == Token::BRACKET_RIGHT {break}
                reporter.add(WjlError::lex(next.start).message("Unexpected token. Expected comma or array literal closure (])").pot_fix("Did you forget the ]?").ok());
                return None
            }
            return Some(StaticExpr::ARR(exprs).to_span(start.start, self.curr().unwrap().end))
        }
        reporter.add(WjlError::lex(next.end).message("Unexpected token. Expected comma or array literal closure (])").pot_fix("Did you forget the ]?").ok());
        return None
    }

    // expecting that we are before the expr
    // expression kinds that we be parsing here:
    pub fn parse_expr(&mut self, reporter: &mut ErrorReporter) -> Option<Expression> {
        let start = self.curr().unwrap();
        let next = self.next();
        if next.is_none() {
            reporter.add(WjlError::ast(start.end).message("Expected expression, got nothing.").ok());
            return None
        }
        let next = next.unwrap();

        let first_part = if next.get_inner() == Token::PAREN_LEFT {
            let wrapped = self.parse_expr(reporter)?;
            Triple::A(Expression::GROUPED(Box::new(wrapped)))
        } else if next.get_inner().is_ident() {
            Triple::B(self.parse_qualified_ident(reporter)?)
        }
        else if next.get_inner() == Token::KEYWORD_AWAIT {
            Triple::A(self.parse_await_expr(reporter)?)
        } else if next.get_inner() == Token::KEYWORD_CLASS {
            Triple::A(self.parse_class_decl(reporter, true)?)
        } else if next.get_inner() == Token::KEYWORD_IF {
            Triple::A(self.parse_if_expr_or_stmt(reporter, true)?)
        } else if next.get_inner() == Token::WJL_COMPILER_PLACEHOLDER {
            Triple::A(Expression::WJL_PLACEHOLDER)
        } else if next.get_inner() == Token::OP_SPREAD {
            Triple::A(self.parse_spread_expr(reporter)?)
        } else if next.get_inner() == Token::KEYWORD_TRY {
            Triple::A(self.parse_try_catch(reporter)?)
        } else if next.get_inner() == Token::INCR {
            Triple::A(self.parse_incr_or_decr(reporter, true)?)
        } else if next.get_inner() == Token::DECR {
            Triple::A(self.parse_incr_or_decr(reporter, false)?)
        } else if next.get_inner() == Token::KEYWORD_FUNC {
            Triple::A(self.parse_func_call(reporter)?)
        } else {
            Triple::C(next)
        };






        return None
    }

    pub fn parse_func_call(&mut self, reporter: &mut ErrorReporter) {

    }

    pub fn parse_incr_or_decr(&mut self, reporter: &mut ErrorReporter, is_incr: bool) {

    }

    pub fn parse_try_catch(&mut self, reporter: &mut ErrorReporter) {

    }

    pub fn parse_spread_expr(&mut self, reporter: &mut ErrorReporter) {

    }

    // expecting that we are on the await
    pub fn parse_await_expr(&mut self, reporter: &mut ErrorReporter) {

    }
    // expecting that we are on the as kw
    pub fn parse_typecast(&mut self, reporter: &mut ErrorReporter) {

    }

    // expecting that we are on the type name
    pub fn parse_struct_init(&mut self, reporter: &mut ErrorReporter) {

    }

    // expecting that we are on the struct kw
    pub fn parse_struct_decl(&mut self, reporter: &mut ErrorReporter) {

    }

    // expecting that we are on the enum kw
    pub fn parse_enum_decl(&mut self, reporter: &mut ErrorReporter) {

    }

    // expecting that we are on the class kw
    pub fn parse_class_decl(&mut self, reporter: &mut ErrorReporter, can_be_anon: bool) {

    }

    pub fn parse_if_expr_or_stmt(&mut self, reporter: &mut ErrorReporter, needs_catchall: bool) {

    }

    pub fn parse_yield_or_return(&mut self, reporter: &mut ErrorReporter) {

    }

    // expecting that we are on the colon
    pub fn parse_type_annotation(&mut self, reporter: &mut ErrorReporter) -> Option<()> {
        let start = self.curr().unwrap().start;
        let ident_or_paren = self.next_skip_ml_comment();
        if ident_or_paren.is_none() {
            reporter.add(WjlError::ast(start).message("Expected type annotation, got nothing.").ok());
            return None
        }
        let ident_or_paren = ident_or_paren.unwrap();
        if ident_or_paren.get_inner().is_ident() {
            let qualified_type = self.parse_qualified_ident(reporter)?;

        }

        return None
    }
}

impl ToAst for Vec<TokenSpan> {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Span> {
        let mut iter = wrap_iter(self);
        let mut stream = vec![];
        while let Some(span) = iter.next_skip_ml_comment() {
            let tok = span.get_inner();
            //region: variable
            if [KEYWORD_VAL, KEYWORD_VAR, KEYWORD_CONST].contains(&tok) {
                let preceeding_data = iter.reverse_collect_mod(KEYWORD_VAL == tok, reporter);
                let decl_keyword_idx = iter.get_index().unwrap();
                let next = iter.next_skip_ml_comment();
                if next.is_none() {
                    reporter.add(WjlError::ast(span.end).message("Expected variable name or destructuring expression, got nothing.").ok());
                    break;
                }
                let next = next.unwrap();

                let var_name = if let Token::IDENT(ident) = next.get_inner() {
                    let parsed = iter.parse_qualified_ident(reporter);
                    if parsed.is_none() {
                        break
                    }
                    TSpan::wrap(0, 0, NamedRef {
                        name: Some(parsed.unwrap()),
                        is_array_destruct: false,
                        is_object_destruct: false,
                        entries: None,
                    })
                } else if next.get_inner() == Token::BRACE_LEFT {
                    let parsed = iter.parse_object_destruct_expr(reporter);
                    if parsed.is_none() {
                        break;
                    }
                    parsed.unwrap()
                } else if next.get_inner() == Token::BRACKET_LEFT {
                    let parsed = iter.parse_array_destruct_expr(reporter);
                    if parsed.is_none() {
                        break;
                    }
                    parsed.unwrap()
                } else {
                    reporter.add(WjlError::ast(next.start).message(format!("Expected variable name or destructuring expression, got {}.", next.to_colored_str())).ok());
                    break;
                };
                let next = iter.next_skip_ml_comment();
                if next.is_none() {
                    reporter.add(WjlError::ast(var_name.start).set_end_char(var_name.end + 1).message("Expected initializer or type hint.").ok());
                    break
                }
                let next = next.unwrap();
                let needs_init = tok == KEYWORD_CONST || (tok == KEYWORD_VAL && !preceeding_data.is_once);
                if next.get_inner() == Token::COLON {

                    //type hint
                }
                if next.get_inner() == Token::ASSIGN {
                    // init
                }
                if needs_init {
                    reporter.add(WjlError::ast(var_name.start).set_end_char(var_name.end).message("Variable needs initializer").ok());
                    break
                }

            }
            //endregion
        }
        stream
    }
}