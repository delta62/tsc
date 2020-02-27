#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate lazy_static;
extern crate unicode;

mod charclass;
pub mod errors;
mod languageword;
mod token;
mod tokens;
mod tokentype;

pub use self::languageword::LanguageWord;
pub use self::token::Token;
pub use self::tokens::Tokens;
pub use self::tokentype::{Identifier,TokenType};

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
