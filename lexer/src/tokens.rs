use std::iter::Peekable;
use std::str::Chars;
use super::token::Token;

pub struct Tokens<'a> {
    input: Peekable<Chars<'a>>,
}

impl <'a> Tokens<'a> {
    pub fn new(input: &str) -> Tokens<'a> {
        let stream = input.chars().peekable();
        Tokens { input: stream }
    }
}

impl <'a> Iterator for Tokens<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
