#[derive(Debug, Clone)]
pub struct WjlError {
    pub char: usize,
    pub end_char: Option<usize>,
    pub message: String,
    pub fixes: Vec<String>,
    pub level: ErrorLevel,
    pub cause: Option<String>,
    pub location: (usize, usize, Option<(usize, usize)>),
    pub is_from_lex: bool,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErrorLevel {
    WARN,
    ERROR,
}

impl WjlError {
    pub fn lex(index: usize) -> WjlError {
        WjlError {
            char: index,
            message: String::from("No error message provided."),
            fixes: vec![],
            level: ErrorLevel::ERROR,
            location: (0, 0, None),
            end_char: None,
            cause: None,
            is_from_lex: true,
        }
    }

    pub fn ast(index: usize) -> WjlError {
        WjlError {
            char: index,
            message: String::from("No error message provided."),
            fixes: vec![],
            level: ErrorLevel::ERROR,
            location: (0, 0, None),
            end_char: None,
            cause: None,
            is_from_lex: false,
        }
    }

    pub fn cause<T: Into<String>>(&mut self, msg: T) -> &mut Self {
        self.cause = Some(msg.into());
        return self;
    }

    pub fn message<T: Into<String>>(&mut self, msg: T) -> &mut Self {
        self.message = msg.into();
        self
    }
    pub fn pot_fix<T: Into<String>>(&mut self, fix: T) -> &mut Self {
        self.fixes.push(fix.into());
        self
    }

    pub fn level(&mut self, lvl: ErrorLevel) -> &mut Self {
        self.level = lvl;
        self
    }

    pub fn _set_loc(&mut self, line: usize, pos: usize) -> &mut Self {
        self.location = (line, pos, None);
        self
    }

    pub fn _set_loc_ranged(
        &mut self,
        from_line: usize,
        from_pos: usize,
        to_line: usize,
        to_pos: usize,
    ) -> &mut Self {
        self.location = (from_line, from_pos, Some((to_line, to_pos)));
        self
    }

    pub fn set_end_char(&mut self, end: usize) -> &mut Self {
        self.end_char = Some(end);
        self
    }
    pub fn ok(&self) -> Self {
        self.clone()
    }
}
