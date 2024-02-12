pub trait AsOption<T> {
    fn to_option(self) -> Option<Vec<T>>;
}

impl <T> AsOption<T> for Vec<T> {
    fn to_option(self) -> Option<Vec<T>> {
        if self.len() == 0 {return None}
        return Some(self)
    }
}