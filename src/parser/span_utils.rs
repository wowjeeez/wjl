use crate::lexer::{TOKEN, TokenSpan};

impl TokenSpan {
    pub fn is_variable_declaration(&self) -> bool {
        match self.token {
            TOKEN::DECL_CONST => true,
            TOKEN::DECL_VAL => true,
            TOKEN::DECL_VAR => true,
            _ => false
        }
    }

    pub fn is_variable_modifier(&self) -> bool {
        match self.token {
            TOKEN::KEYWORD_ASYNC => true,
            TOKEN::KEYWORD_OPERATOR => true,
            TOKEN::ACCESSMOD_EXTERNAL => true,
            TOKEN::ACCESSMOD_INTERNAL => true,
            TOKEN::ACCESSMOD_PRIVATE => true,
            TOKEN::KEYWORD_STATIC => true,
            TOKEN::KEYWORD_PURE => true,
            TOKEN::DECL_ONCE => true,
            TOKEN::KEYWORD_FINAL => true,
            TOKEN::KEYWORD_ABSTRACT => true,
            _ => false
        }
      //  todo!("Also, check if this is a visibility modifier because then this goes to somewhere else in the parser. Rule: Visibility mod should appear first");
       // todo!("Check if its async, operator etc etc and also (prolly at some later stage) apply further checks based off of the lexical context and the type signature of the function, because e.g operator cant appear on a fucking string, and external cannot appear on something not scoped in global")
    }
}