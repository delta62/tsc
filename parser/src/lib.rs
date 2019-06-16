extern crate lexer;

use lexer::Lexer;
use lexer::Token;

pub enum ParseError {
    UnexpectedToken(Token),
}

pub enum Node {
    Declaration,
    Script(Vec<Node>),
    Statement,
}

pub struct Parser<I>
where I: Iterator<Item = char> {
    lexer: Lexer<I>,
}

impl<I> Parser<I>
where I: Iterator<Item = char> {
    pub fn new(lexer: Lexer<I>) -> Parser<I> {
        Parser { lexer: lexer }
    }

    pub fn parse(&mut self) -> Result<Node, ParseError> {
        self.script_body()
    }

    fn script_body(&mut self) -> Result<Node, ParseError> {
        let body = Vec::new();
        loop {
            match self.lexer.next() {
                Some(Ok(t)) => {
                    match t {
                        _ => return Err(ParseError::UnexpectedToken(t))
                    }
                },
                Some(Err(_)) |
                None => break
            }
        }

        Ok(Node::Script(body))
    }
}
