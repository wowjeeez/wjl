use std::fmt::Debug;

pub enum Triple<A: Debug, B : Debug, C: Debug> {
    A(A),
    B(B),
    C(C)
}

impl <A: Debug, B : Debug, C: Debug> Triple<A, B, C> {}

