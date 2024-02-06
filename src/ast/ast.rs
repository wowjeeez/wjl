use crate::ast::nodes::variable::{Assignment, DestructuringEntry, NamedRef, VariableDeclaration};
use crate::errors::{ErrorReporter, WjlError};
use crate::iter::{GenericIterator, PeekableIterator, wrap_iter};
use crate::tokens::span::Span as GenericSpan;
use crate::tokens::Token;
use crate::tokens::Token::{KEYWORD_CONST, KEYWORD_VAL, KEYWORD_VAR};

pub type TokenSpan = GenericSpan<Token>;

#[derive(Debug, Clone)]
#[allow(non_camel_case_types, unused)]
pub enum Ast {
    VAR_DECL(VariableDeclaration),
    ASSIGNMENT(Assignment),
}

trait ToAst {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Ast>;
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
    pub fn parse_object_destruct_expr(&mut self, reporter: &mut ErrorReporter) -> Option<NamedRef> {
        let before = self.get_index().unwrap();
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
                is_btick: None,
                string_name: None
            })
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
                string_name: None,
                is_btick: None
            })
        }

        reporter.add(WjlError::ast(first_tok.start).message("Unexpected token.").ok());
        return None
    }

    // will consume from the iter expecting that we are on the [
    pub fn parse_array_destruct_expr(&mut self, reporter: &mut ErrorReporter) -> Option<NamedRef> {
        let before = self.get_index().unwrap();
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
                is_btick: None,
                string_name: None
            })
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
                string_name: None,
                is_btick: None
            })
        }
        reporter.add(WjlError::ast(first_tok.start).message("Unexpected token.").ok());
        return None
    }

    // expecting that we are before the ident or rest arg
    pub fn parse_identifier_in_destruct(&mut self, in_arr: bool, reporter: &mut ErrorReporter) -> Option<DestructuringEntry> {
        let mut target = self.next_skip_ml_comment().unwrap(); //we know its there otherwise we dont call it
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
                });
            }
            if inner == Token::BRACE_LEFT {
                let parsed = self.parse_object_destruct_expr(reporter)?;
                return Some(DestructuringEntry {
                    name: parsed,
                    default_value: None,
                    binding: None,
                    is_rest: is_spread,
                });
            }
        }

        if !inner.is_ident() {
            reporter.add(WjlError::ast(target.end).message("Expected identifier.").ok());
            return None;
        }
        let (next, end) = self.peek_next_skip_ml_comment();
        if next.is_none() {
            // safe to cast to usize
            reporter.add(WjlError::ast(target.end + end as usize)
                .message("Expected assignment, alias, comma or closing bracket (}, ]) got nothing.").ok());
            return None;
        }
        let next = next.unwrap();
        if next.get_inner() == Token::COMMA || next.get_inner() == if in_arr { Token::BRACKET_RIGHT } else { Token::BRACE_RIGHT } {
            let (tx, is_b) = inner.get_ident_inner();
            return Some(DestructuringEntry {
                is_rest: is_spread,
                name: NamedRef {
                    is_object_destruct: false,
                    is_array_destruct: false,
                    entries: None,
                    string_name: Some(tx),
                    is_btick: Some(is_b),
                },
                default_value: None,
                binding: None,
            });
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
            let (tx, is_b) = inner.get_ident_inner();
            if alias_tok.get_inner().is_ident() {
                let (ident, is_btick) = alias_tok.get_inner().get_ident_inner();
                return Some(DestructuringEntry {
                    binding: Some(NamedRef {
                        entries: None,
                        is_array_destruct: false,
                        is_object_destruct: false,
                        is_btick: Some(is_btick),
                        string_name: Some(ident)
                    }),
                    is_rest: is_spread,
                    name: NamedRef {
                        entries: None,
                        is_array_destruct: false,
                        is_object_destruct: false,
                        is_btick: Some(is_b),
                        string_name: Some(tx)
                    },
                    default_value: None
                })
            }
            if alias_tok.get_inner() == Token::BRACKET_LEFT {
                let binding = self.parse_array_destruct_expr(reporter)?;
                return Some(DestructuringEntry {
                    binding: Some(binding),
                    is_rest: is_spread,
                    name: NamedRef {
                        entries: None,
                        is_array_destruct: false,
                        is_object_destruct: false,
                        is_btick: Some(is_b),
                        string_name: Some(tx)
                    },
                    default_value: None
                })
            }
            if alias_tok.get_inner() == Token::BRACE_LEFT {
                let binding = self.parse_object_destruct_expr(reporter)?;
                return Some(DestructuringEntry {
                    binding: Some(binding),
                    is_rest: is_spread,
                    name: NamedRef {
                        entries: None,
                        is_array_destruct: false,
                        is_object_destruct: false,
                        is_btick: Some(is_b),
                        string_name: Some(tx)
                    },
                    default_value: None
                })
            }
        }

        if next.get_inner() == Token::ASSIGN {
            // default
            todo!("Parse expr here")
        }

        reporter.add(WjlError::lex(next.start).message("Unexpected token").ok());
        return None
    }
}

impl ToAst for Vec<TokenSpan> {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Ast> {
        let mut iter = wrap_iter(self);
        let mut stream = vec![];
        while let Some(span) = iter.next_skip_ml_comment() {
            let tok = span.get_inner();
            if [KEYWORD_VAL, KEYWORD_VAR, KEYWORD_CONST].contains(&tok) {
                let prev = iter.peek_prev_skip_ml_comment();
                let decl_keyword_idx = iter.get_index().unwrap();
                let next = iter.next_skip_ml_comment();
                if next.is_none() {
                    reporter.add(WjlError::ast(span.end).message("Expected variable name or destructuring expression, got nothing.").ok());
                    break;
                }
                let next = next.unwrap();

                let var_name = if let Token::IDENT(ident) = next.get_inner() {
                    NamedRef {
                        is_btick: Some(ident.is_btick()),
                        string_name: Some(ident.get_text()),
                        is_array_destruct: false,
                        is_object_destruct: false,
                        entries: None,
                    }
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
            }
        }
        stream
    }
}