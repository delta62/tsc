#[macro_use]
extern crate error_chain;
extern crate unicode;

// mod charclass;
mod errors;
mod reservedword;
mod token;
mod tokens;
mod tokentype;

pub use self::token::Token;
pub use self::tokens::Tokens;
pub use self::tokentype::TokenType;

pub struct Lexer<'a> {
    input: &'a str,
}

impl <'a> Lexer<'a> {
    pub fn with_str(input: &str) -> Lexer {
        Lexer { input }
    }
}

impl <'a> IntoIterator for Lexer<'a> {
    type Item = Token;
    type IntoIter = Tokens<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Tokens::new(self.input)
    }
}
