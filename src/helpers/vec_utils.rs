pub trait AsOption<T> {
    fn to_option(self) -> Option<T>;
}

impl <T> AsOption<T> for Vec<T> {
    fn to_option(self) -> Option<T> {
        if self.len() == 0 {return None}
        return Some(self)
    }
}