extern crate lexer;

use lexer::{Lexer,ReservedWord,Token,TokenType};

pub enum ParseError {
    NotImplemented,
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
        self.statement_list().map(|list| Node::Script(list))
    }

    fn statement_list(&mut self) -> Result<Vec<Node>, ParseError> {
        let mut stmts: Vec<Node> = Vec::new();
        loop {
            let next = self.lexer.next();
            match &next {
                Some(Ok(_)) => {
                    let result = self.declaration_or_statement();
                    match result {
                        Ok(x) => stmts.push(x),
                        Err(e) => return Err(e),
                    }
                },
                Some(Err(_)) => return Err(ParseError::NotImplemented),
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
            .map_err(|_| ParseError::NotImplemented)
            .and_then(|token| {
                match &token {
                    // Declarations
                    x if is_variable_declaration(x)                   => self.variable_declaration(),
                    x if is_function_declaration(x)                   => self.function_declaration(),
                    x if is_class_declaration(x)                      => self.class_declaration(),
                    // Statements
                    x if is_variable_statement(x)                     => self.variable_declaration(),
                    x if x.typ == TokenType::LeftBrace                => self.block(),
                    x if x.typ == TokenType::Semicolon                => self.empty_statement(),
                    // ExpressionStatement
                    x if is_reserved_word(x, &ReservedWord::If)       => self.if_statement(),
                    x if is_reserved_word(x, &ReservedWord::Switch)   => self.switch_statement(),
                    x if is_reserved_word(x, &ReservedWord::Do)       => self.do_statement(),
                    x if is_reserved_word(x, &ReservedWord::While)    => self.while_statement(),
                    x if is_reserved_word(x, &ReservedWord::For)      => self.for_statement(),
                    x if is_reserved_word(x, &ReservedWord::Continue) => self.continue_statement(),
                    x if is_reserved_word(x, &ReservedWord::Break)    => self.break_statement(),
                    x if is_reserved_word(x, &ReservedWord::Return)   => self.return_statement(),
                    x if is_reserved_word(x, &ReservedWord::With)     => self.with_statement(),
                    // LabelledStatement
                    x if is_reserved_word(x, &ReservedWord::Throw)    => self.throw_statement(),
                    x if is_reserved_word(x, &ReservedWord::Try)      => self.try_statement(),
                    x if is_reserved_word(x, &ReservedWord::Debugger) => self.debugger_statement(),
                    // Other
                    _                                                 => Err(ParseError::UnexpectedToken),
                }
            })
    }

    fn variable_declaration(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn function_declaration(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn class_declaration(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn empty_statement(&mut self) -> Result<Node, ParseError> {
        Ok(Node::Statement)
    }

    fn block(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn if_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn continue_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn break_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn return_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn with_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn throw_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn try_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn debugger_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn switch_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn do_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn while_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }

    fn for_statement(&mut self) -> Result<Node, ParseError> {
        Err(ParseError::NotImplemented)
    }
}

fn is_variable_declaration(token: &Token) -> bool {
    match token.typ {
        TokenType::Identifier(_, Some(ReservedWord::Let))
        | TokenType::Identifier(_, Some(ReservedWord::Const)) => true,
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

fn is_variable_statement(token: &Token) -> bool {
    match token.typ {
        TokenType::Identifier(_, Some(ReservedWord::Var)) => true,
        _ => false,
    }
}

fn is_reserved_word(token: &Token, expected: &ReservedWord) -> bool {
    match &token.typ {
        TokenType::Identifier(_, Some(actual)) if actual == expected => true,
        _ => false,
    }
}
