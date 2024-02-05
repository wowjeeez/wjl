use std::num::IntErrorKind;

pub fn treat_as_bigint(input: String, radix: u32) -> bool {
    let i64_res = i64::from_str_radix(&*input, radix);
    i64_res
        .map_err(|x| match x.kind() {
            IntErrorKind::Empty => false,
            IntErrorKind::InvalidDigit => false,
            IntErrorKind::PosOverflow => true,
            IntErrorKind::NegOverflow => false,
            IntErrorKind::Zero => false,
            _ => false,
        })
        .map_or_else(|e| e, |x| false)
}
