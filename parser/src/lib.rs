extern crate lexer;

use lexer::{Lexer,ReservedWord,Token,TokenType};

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
        let stmts = Vec::new();
        loop {
            let next = self.lexer.next();
            match &next {
                Some(Ok(token)) => {
                    if is_statement_start(&token) {

                    } else if is_declaration_start(&token) {

                    } else {
                        break
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
}

fn is_statement_start(token: &Token) -> bool {
    match token.typ {
        TokenType::LeftBrace
        | TokenType::Identifier(_, Some(ReservedWord::Var))
        | TokenType::Semicolon => true,
        _ => false,
    }
}

fn is_declaration_start(_token: &Token) -> bool {
    false
}
