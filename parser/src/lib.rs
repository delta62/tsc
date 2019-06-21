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
        let mut stmts: Vec<Node> = Vec::new();
        loop {
            let next = self.lexer.next();
            match &next {
                Some(Ok(token)) => {
                    let result = self.declaration_or_statement();
                    match result {
                        Ok(x) => stmts.push(x),
                        Err(e) => return Err(e),
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

    fn declaration_or_statement(&mut self) -> Result<Node, ParseError> {
        let token = self.lexer.next().unwrap();
        token
            .map_err(|_| ParseError::UnexpectedToken)
            .and_then(|token| {
                match &token {
                    // Declarations
                    x if is_variable_declaration(x) => self.variable_declaration(),
                    x if is_function_declaration(x) => self.function_declaration(),
                    x if is_class_declaration(x)    => self.class_declaration(),
                    // Statements
                    // Other
                    _                            => Err(ParseError::UnexpectedToken),
                }
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

fn is_variable_declaration(token: &Token) -> bool {
    match token.typ {
        TokenType::Identifier(_, Some(ReservedWord::Let))
        | TokenType::Identifier(_, Some(ReservedWord::Const))
        | TokenType::Identifier(_, Some(ReservedWord::Var)) => true,
        _ => false,
    }
}

fn is_function_declaration(token: &Token) -> bool {
    match token.typ {
        TokenType::Identifier(_, Some(ReservedWord::Async))
        | TokenType::Identifier(_, Some(ReservedWord::Function)) => true,
        _ => false,
    }
}

fn is_class_declaration(token: &Token) -> bool {
    match token.typ {
        TokenType::Identifier(_, Some(ReservedWord::Class)) => true,
        _ => false,
    }
}
