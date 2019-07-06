use std::iter::{Enumerate,Peekable};
use std::str::Chars;

use super::errors::*;
use super::token::Token;
use super::tokentype::TokenType;

pub struct Tokens<'a> {
    input: Peekable<Enumerate<Chars<'a>>>,
    diagnostics: Vec<Error>,
}

impl <'a> Tokens<'a> {
    pub fn new(input: &'a str) -> Tokens<'a> {
        let stream = input.chars().enumerate().peekable();
        Tokens {
            input: stream,
            diagnostics: Vec::new(),
        }
    }

    pub fn diagnostics(&self) -> &Vec<Error> {
        &self.diagnostics
    }

    fn unexpected_char(idx: usize, c: char) -> Result<Token> {
        Err(ErrorKind::UnexpectedChar(c, idx).into())
    }

    fn unexpected_eof() -> Result<Token> {
        Err(ErrorKind::UnexpectedEof.into())
    }

    fn slash(&mut self) -> Result<TokenType> {
        Err(ErrorKind::UnexpectedEof.into())
    }

    fn minus(&mut self) -> TokenType {
        match self.input.peek() {
            Some((_, '-')) => {
                self.input.next();
                TokenType::Decrement
            },
            Some((_, '=')) => {
                self.input.next();
                TokenType::MinusEquals
            },
            _ => TokenType::Minus,
        }
    }

    fn plus(&mut self) -> TokenType {
        match self.input.peek() {
            Some((_, '+')) => {
                self.input.next();
                TokenType::Increment
            },
            Some((_, '=')) => {
                self.input.next();
                TokenType::PlusEquals
            },
            _ => TokenType::Plus,
        }
    }
}

impl <'a> Iterator for Tokens<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let t = self.input.next().map(|x| match x {
                (i, '/') => token(i, self.slash()),
                (i, '-') => token(i, Ok(self.minus())),
                (i, '+') => token(i, Ok(self.plus())),
                (i, c)   => Tokens::unexpected_char(i, c),
            });

            match t {
                Some(Ok(x))  => return Some(x),
                Some(Err(e)) => self.diagnostics.push(e),
                None         => return None,
            }
        }
    }
}

fn token(location: usize, maybe: Result<TokenType>) -> Result<Token> {
    maybe.map(|x| Token::new(location, x))
}
