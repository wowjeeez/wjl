#[cfg(test)]
mod tests {
    use crate::lexer::span::TokenSpan;
    use crate::lexer::tokenize;
    use crate::lexer::tokens::TOKEN;

    fn assert_import_tokens(token: Option<&TokenSpan>, paths: &[&str]) {
        if token.is_none() {
            panic!("no token received");
        }
        let token = token.unwrap().token.clone();
        if let TOKEN::S2_IMPORT_PATH(path) = token {
            let result = path.iter().enumerate().all(|(ix, x)| paths.get(ix).unwrap().to_string() == match x.token.clone() {
                TOKEN::IDENTIFIER(inner) => inner,
                _ => "".to_string()
            });
            assert!(result)
        } else {
            panic!("recv token was not S2_IMPORT_PATH")
        }
    }

    fn assert_ext_import_tokens(token: Option<&TokenSpan>, expected_module: String, expected_untyped: bool, expected_receiver_token_stream: TOKEN) {
        if token.is_none() {
            panic!("no token received");
        }
        let token = token.unwrap().token.clone();
        if let TOKEN::S2_EXTERNAL_IMPORT {
            module,
            untyped,
            receiver
        } = token {
            assert_eq!(module, expected_module);
            assert_eq!(untyped, expected_untyped);
            assert_eq!(receiver.token, expected_receiver_token_stream)
        } else {
            panic!("recv token was not S2_EXTERNAL_IMPORT")
        }
    }



    #[test]
    fn should_parse_mod_import() {
        let buffer = String::from("dep mydep");
        let tokens = tokenize(buffer).unwrap();
        let tok = tokens.first();
        assert_import_tokens(tok, &["mydep"])
    }
    #[test]
    fn should_parse_deep_mod_import() {
        let buffer = String::from("dep mydep::inner");
        let tokens = tokenize(buffer).unwrap();
        let tok = tokens.first();
        assert_import_tokens(tok, &["mydep", "inner"])
    }

    #[test]
    fn should_parse_file_mod_import() {
        let buffer = String::from("dep mydep::inner::file");
        let tokens = tokenize(buffer).unwrap();
        let tok = tokens.first();
        assert_import_tokens(tok, &["mydep", "inner", "file"])
    }

}