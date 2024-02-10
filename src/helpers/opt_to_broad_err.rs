pub fn opt_to_broad_err<T>(opt: Option<T>) -> Result<Option<T>, ()> {
    if opt.is_none() {
        return Err(())
    }
    return Ok(opt)
}