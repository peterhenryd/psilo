use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::lexer::{Token};
use crate::lexer::id::TokenId;

pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;

pub enum ParseError<'a> {
    Expected(Vec<TokenId>, Option<Token<'a>>),
    ExpectedType(Option<Token<'a>>),
    ExpectedExpr(Option<Token<'a>>),
}

impl Debug for ParseError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for ParseError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Expected(expected, given) => f.write_str(
                format!("expected {} but got {}",
                        expected.iter()
                            .map(|id| format!("`{}`", id))
                            .collect::<Vec<String>>()
                            .join(", "),
                        given.map_or_else(
                            || "nothing".to_string(),
                            |token| format!("`{}`", token.id())
                        )
                ).as_str()
            ),
            ParseError::ExpectedType(given) => f.write_str(
                format!("expected type but got {}",
                        given.map_or_else(
                            || "nothing".to_string(),
                            |token| format!("`{}`", token.id())
                        )
                ).as_str()
            ),
            ParseError::ExpectedExpr(given) => f.write_str(
                format!("expected expr but got {}",
                        given.map_or_else(
                            || "nothing".to_string(),
                            |token| format!("`{}`", token.id())
                        )
                ).as_str()
            ),
        }
    }
}

impl Error for ParseError<'_> {}