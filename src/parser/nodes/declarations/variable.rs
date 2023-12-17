use std::sync::RwLock;
use either::Either;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::{PartialAstIr, AstNodeParsable, NodeVis};
use crate::parser::ast::PartialAstIr::{_NONCE, TYPE_ANNOTATION, VAR_DECL};
use crate::parser::errors::ParseError;
use crate::parser::errors::ParseError::{EXPECTED_MODIFIER, EXPECTED_NAME_OR_DESTRUCTURING, EXPECTED_NAME_OR_DESTRUCTURING_OR_TYPE, EXPECTED_ONEOF, MODIFIER_CANT_APPEAR, NEEDS_INIT, ONLY_ONE_VISIBILITY_MODIFIER, VISIBILITY_MODIFIER_SHOULD_BE_FIRST};
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::array_destruct::ArrayDestructureAstNode;
use crate::parser::nodes::identifier::IdentifierAstNode;
use crate::parser::nodes::object_destruct::ObjectDestructureAstNode;
use crate::parser::nodes::variable::VARIABLE_DECL_TYPE::{CONST, IMMUT, MUT, MUT_ONCE};
use crate::parser::parse::ParsableTokenStream;

#[derive(PartialEq, Debug, Clone)]
#[allow(non_camel_case_types)]
pub enum VARIABLE_DECL_TYPE {
    IMMUT,
    MUT,
    MUT_ONCE,
    CONST
}

#[derive(PartialEq, Debug, Clone)]
pub struct VariableAstNode {
    pub name: Box<PartialAstIr>, //either a plain name or some kind of a destructuring statement
    pub decltype: VARIABLE_DECL_TYPE,
    pub initializer: Option<Vec<PartialAstIr>>,
    pub visibility: NodeVis,
    pub modifiers: Vec<TOKEN>,
    pub explicit_type: Option<Box<PartialAstIr>>, //typings in code
    pub inferred_type: Option<Box<PartialAstIr>>, //the inferred type, or in a later stage the final type of the node
}

fn validate_and_separate_modifiers(modifiers: Vec<TOKEN>, token_span: TokenSpan) -> Result<(NodeVis, Vec<TOKEN>), ParseError> {
    let mut vis = None;
    let mut mods = vec![];
    for (modidx, modifier) in modifiers.iter().enumerate() {
        let is_vis_mod = match modifier {
            TOKEN::ACCESSMOD_EXTERNAL | TOKEN::ACCESSMOD_INTERNAL | TOKEN::ACCESSMOD_PRIVATE => true,
            _ => false
        };
        if is_vis_mod && modidx != 0 && vis.is_none() {
            return Err(VISIBILITY_MODIFIER_SHOULD_BE_FIRST(token_span))
        }
        if is_vis_mod && vis.is_some() {
            return Err(ONLY_ONE_VISIBILITY_MODIFIER(token_span))
        }
        if is_vis_mod {
            vis = Some(modifier.clone());
        } else {
            mods.push(modifier.clone())
        }
    }
    Ok((vis.map_or(NodeVis::BLOCK, |vis| match vis {
        TOKEN::ACCESSMOD_EXTERNAL => NodeVis::PUBLIC,
        TOKEN::ACCESSMOD_INTERNAL => NodeVis::PROTECTED,
        TOKEN::ACCESSMOD_PRIVATE => NodeVis::PRIVATE,
        _ => unreachable!()
    }), mods))
}

impl AstNodeParsable for VariableAstNode {
    // expecting tokens ONLY (Either::Left)
    fn parse(iter: &mut TokenParserIter, span: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {
        let span = span.left().unwrap();
        let mut peek_lookbehind_base = RwLock::new(1);
        let decltype = match span.token {
            TOKEN::DECL_CONST => CONST,
            TOKEN::DECL_VAL => {
                let prev = iter.peek_prev_skip_whitespace();
                if prev.is_some() && prev.unwrap().token == TOKEN::DECL_ONCE {
                    *peek_lookbehind_base.write().unwrap() = 1;
                    MUT_ONCE
                } else {
                    IMMUT
                }
            },
            TOKEN::DECL_VAR => MUT,
            _ => unreachable!()
        };
        // Lookbehind parsing
        let prev = iter.peek_prev_skip_whitespace();
        if prev.is_some() {
            let prev = prev.unwrap();
            if prev.token == TOKEN::DECL_ONCE && span.token != TOKEN::DECL_VAL {
                return Err(MODIFIER_CANT_APPEAR(prev, TOKEN::DECL_ONCE, span.token, vec![]))
            }
        }
        let maybe_modifier = iter.peek_prev_n_skip_whitespace(*peek_lookbehind_base.read().unwrap() + 1);
        let mut modifiers = if maybe_modifier.is_some() {
            let maybe_modifier = maybe_modifier.unwrap();
            let mut mods = vec![];
            if maybe_modifier.token != TOKEN::SEMI_COLON {
                //there might be some modifiers so we start peekin and geekin

                let mut peek_iter = iter.lookbehind_peek_iter();
                // we only go back to maybe skip the once as that is part of the declaration and is not a modifier, its not a major perf tradeoff to let the parser look at `maybe_modifier` again
                peek_iter.set_index(((iter.pos() as isize) - (*peek_lookbehind_base.read().unwrap() as isize)) as usize);
                while let Some(maybe_modifier) = peek_iter.next_skip_whitespace() {
                    if maybe_modifier.token == TOKEN::SEMI_COLON {break} // no more modifiers
                    if maybe_modifier.is_variable_modifier() {
                        mods.push(maybe_modifier.token);
                    } else {
                        let tok = maybe_modifier.token.clone(); //yes we are saving that few bytes (by only cloning the enum ordinal and not the whole TokenSpan), its 3 am I am all for optimization
                        return Err(EXPECTED_MODIFIER(maybe_modifier, tok, vec![TOKEN::KEYWORD_ASYNC, TOKEN::KEYWORD_OPERATOR, TOKEN::ACCESSMOD_EXTERNAL, TOKEN::ACCESSMOD_INTERNAL, TOKEN::ACCESSMOD_PRIVATE]))
                    }
                }
            }
            mods
        } else {vec![]};
        modifiers.reverse(); //we go backwards, so we have to reverse it in order for the ordering to be correct

        let name = iter.next_skip_whitespace();// this goes to the capitalized part val NAME


        if name.is_none() {
            return Err(EXPECTED_NAME_OR_DESTRUCTURING_OR_TYPE(span, None))
        }
        let name = name.unwrap();
        let mut type_ident = None;
        let var_name = match &name.token {
            TOKEN::IDENTIFIER(_) | TOKEN::BACKTICK_LITERAL(_) => {
                let ident = IdentifierAstNode::parse(iter, Either::Left(name.clone()))?;
                // this could also indicate a type hint
                let name_or_type_path = iter.next_skip_whitespace();
                let next_name = if name_or_type_path.is_some() {
                    let name_or_type_path = name_or_type_path.unwrap();
                    let name = match name_or_type_path.token {
                        TOKEN::S2_ASSIGN => None,
                        TOKEN::IDENTIFIER(_) | TOKEN::BACKTICK_LITERAL(_) => Some(IdentifierAstNode::parse(iter, Either::Left(name_or_type_path.clone()))?),
                        TOKEN::LEFT_BRACE => Some(ObjectDestructureAstNode::parse(iter, Either::Left(name_or_type_path.clone()))?),
                        TOKEN::LEFT_BRACKET => Some(ArrayDestructureAstNode::parse(iter, Either::Left(name_or_type_path.clone()))?),
                        _ => None
                    };
                    if name.is_some() {
                        type_ident = Some(ident.clone());
                        Some(name.unwrap())
                    } else {
                        None
                    }
                } else {None};
                if next_name.is_some() {
                    next_name.unwrap()
                } else {
                    iter.seek(-1);
                    ident
                }
            },
            TOKEN::LEFT_BRACE => ObjectDestructureAstNode::parse(iter, Either::Left(span.clone()))?,
            TOKEN::LEFT_BRACKET => ArrayDestructureAstNode::parse(iter, Either::Left(span.clone()))?,
            _ => _NONCE
        };
        if var_name == _NONCE {
            return Err(EXPECTED_NAME_OR_DESTRUCTURING_OR_TYPE(span, Some(name.token)))
        }

        drop(name);

        // Lookahead parsing
        let (vis, modifiers) = validate_and_separate_modifiers(modifiers, span.clone())?;
        let peeked = iter.peek_skip_whitespace();
        let has_init = peeked.as_ref().map_or(false, |x|x.token == TOKEN::S2_ASSIGN);
        if !has_init {
            if decltype == CONST || decltype == IMMUT {
                return Err(NEEDS_INIT(span, decltype, peeked.map(|x|x.token)))
            }
            return Ok(VAR_DECL(VariableAstNode {
                name: Box::new(var_name),
                decltype,
                initializer: None,
                visibility: vis,
                modifiers,
                explicit_type: type_ident.map(|x| Box::new(TYPE_ANNOTATION(vec![x]))),
                inferred_type: None,
            }))
        }
        //TODO! generic argument parsing and better type parsing (separate logic for parsing paths and generics to not dupe code)
        iter.next_skip_whitespace(); //drop =
        iter.next_skip_whitespace(); //we dont actually drop it, since we use the inclusive array call below
        let (initializer, seek) = iter.remaining_inclusive().parse_into_ast_vec(vec![TOKEN::SEMI_COLON])?;
        iter.seek((seek + 1) as isize);

        Ok(VAR_DECL(VariableAstNode {
            name: Box::new(var_name),
            decltype,
            initializer: Some(initializer),
            visibility: vis,
            modifiers,
            explicit_type: type_ident.map(|x| Box::new(TYPE_ANNOTATION(vec![x]))),
            inferred_type: None,
        }))
    }
}