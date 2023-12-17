pub fn clone_option<T: Clone>(opt: &Option<T>) -> Option<T> {
    if opt.is_some() {
        return Some(opt.as_ref().unwrap().clone())
    }
    None
}