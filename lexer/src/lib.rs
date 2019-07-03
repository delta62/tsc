#[macro_use]
extern crate error_chain;
extern crate unicode;

mod charclass;
mod errors;
mod lexstream;
mod location;
mod token;
mod tokens;

pub use self::tokens::Tokens;
pub use self::token::Token;

pub struct Lexer {
    input: String,
}

impl Lexer {
    pub fn from_string(input: String) -> Lexer {
        Lexer { input }
    }
}

impl <'a> IntoIterator for Lexer {
    type Item = Token;
    type IntoIter = Tokens<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Tokens::new(self.input)
    }
}
