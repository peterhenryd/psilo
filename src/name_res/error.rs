use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

pub type NameResult<'a, T> = Result<T, NameError<'a>>;

pub enum NameError<'a> {
    UnknownCtx(&'a str)
}

impl Debug for NameError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NameError::UnknownCtx(name) => {
                f.write_str("unknown local variable: ")?;
                f.write_str(*name)
            }
        }
    }
}

impl Display for NameError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NameError::UnknownCtx(s) => f.write_str(format!("unknown variable: {}", s).as_str())
        }
    }
}

impl Error for NameError<'_> {}