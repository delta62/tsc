extern crate lexer;

use lexer::{Lexer,ReservedWord,Token,TokenType,get_reserved_word};

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

    pub fn script(&mut self) -> Result<Node, ParseError> {
        let body = self.statement_list();
        body.map(|list| Node::Script(list))
    }

    fn statement_list(&mut self) -> Result<Vec<Node>, ParseError> {
        let mut stmts: Vec<Node> = Vec::new();
        loop {
            let next = self.lexer.next();
            match &next {
                Some(Ok(token)) => {
                    let s = self.match_statement();
                    match s {
                        Some(Ok(s)) => stmts.push(s),
                        Some(Err(_)) => return Err(ParseError::UnexpectedToken),
                        None => {
                            let d = self.match_declaration();
                            match d {
                                Some(Ok(d)) => stmts.push(d),
                                Some(Err(_)) => return Err(ParseError::UnexpectedToken),
                                None => break
                            }
                        }
                    }
                },
                Some(Err(_)) => return Err(ParseError::UnexpectedToken),
                None => break,
            }

            if next.is_none() {
                break
            }
        }
        Ok(stmts)
    }

    fn match_statement(&mut self) -> Option<Result<Node, ParseError>> {
        match self.lexer.next() {
            Some(Ok(Token { typ: TokenType::LeftBrace, .. })) => {
                Some(self.block())
            },
            Some(Err(_)) => return Some(Err(ParseError::UnexpectedToken)),
            Some(_) => return Some(Err(ParseError::UnexpectedToken)),
            None => None,
        }
    }

    fn match_declaration(&mut self) -> Option<Result<Node, ParseError>> {
        self.lexer.next().map(|token| {
            token.map_err(|_| ParseError::UnexpectedToken)
                .and_then(|token| {
                    match get_reserved_word(&token.typ) {
                        Some(ReservedWord::Let) |
                        Some(ReservedWord::Const)    => self.variable_declaration(),
                        Some(ReservedWord::Async) |
                        Some(ReservedWord::Function) => self.function_declaration(),
                        Some(ReservedWord::Class)    => self.class_declaration(),
                        _                            => Err(ParseError::UnexpectedToken),
                    }
                })
        })
    }

    fn variable_declaration(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::UnexpectedToken)
    }

    fn function_declaration(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::UnexpectedToken)
    }

    fn class_declaration(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::UnexpectedToken)
    }

    fn block(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::UnexpectedToken)
    }
}
