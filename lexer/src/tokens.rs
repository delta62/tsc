use std::iter::{Enumerate,Peekable};
use std::str::Chars;

use super::errors::*;
use super::token::{Token};

pub struct Tokens<'a> {
    input: Peekable<Enumerate<Chars<'a>>>,
}

impl <'a> Tokens<'a> {
    pub fn new(input: &'a str) -> Tokens<'a> {
        let stream = input.chars().enumerate().peekable();
        Tokens { input: stream }
    }

    fn unexpected_char(idx: usize, c: char) -> Result<Token> {
        Err(ErrorKind::UnexpectedChar(c, idx).into())
    }

    fn unexpected_eof(idx: usize) -> Result<Token> {
        Err(ErrorKind::UnexpectedEof(idx).into())
    }

    fn slash(&mut self, idx: usize) -> Result<Token> {
        Err(ErrorKind::UnexpectedEof(idx).into())
    }
}

impl <'a> Iterator for Tokens<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let t = match self.input.next() {
                Some((i, '/')) => token(i, self.slash()),
                Some((i, c))   => Tokens::unexpected_char(i, c),
                None => None,
            };

            match t {
                Some(Ok(x))  => return Some(x),
                Some(Err(_)) => (),
                None         => return None,
            }
        }
    }
}

fn token(location: usize, maybe: Result<TokenType>) -> Option<Result<Token>> {
    maybe
        .map(|x| Token::new(location, x))
}
