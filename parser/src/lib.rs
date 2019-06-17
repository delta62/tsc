extern crate lexer;

use lexer::{Lexer,Token,TokenType};

pub enum ParseError {
    UnexpectedToken,
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
        self.script()
    }

    fn script(&mut self) -> Result<Node, ParseError> {
        let mut body = Vec::new();
        loop {
            match self.lexer.next() {
                Some(Ok(Token { column, line, typ })) => {
                    match typ {
                        // LexicalDeclaration
                        TokenType::Identifier(ref text, None) if text == "let" || text == "const" => {
                            let decl = self.declaration();
                            match decl {
                                Ok(n) => body.push(n),
                                Err(e) => return Err(e),
                            }
                        },
                        _ => return Err(ParseError::UnexpectedToken)
                    }
                },
                Some(Err(_)) | None => break
            }
        }

        Ok(Node::Script(body))
    }

    fn declaration(&mut self) -> Result<Node, ParseError> {
        Ok(Node::Declaration)
    }
}
