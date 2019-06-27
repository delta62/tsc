#[macro_use]
extern crate error_chain;
extern crate lexer;

mod errors;

use errors::*;
use lexer::{Lexer,ReservedWord,Token,TokenType};

pub enum ParseError {
    NotImplemented,
    UnexpectedToken,
    UnexpectedEof,
}

pub enum LetOrConst {
    Let,
    Const,
}

pub enum Declaration {
    LexicalDeclaration(LetOrConst, Vec<Node>),
}

pub struct Assignment { }

pub struct Binding {
    identifier: String,
    assignment: Option<Assignment>,
}

pub enum Node {
    Binding(Binding),
    BindingList(Vec<Node>),
    Declaration(Declaration),
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

    pub fn script(&mut self) -> Result<Node> {
        self.statement_list().map(|list| Node::Script(list))
    }

    fn statement_list(&mut self) -> Result<Vec<Node>> {
        let mut stmts: Vec<Node> = Vec::new();
        loop {
            let next = self.lexer.next();
            match &next {
                Some(_) => {
                    let result = self.declaration_or_statement();
                    match result {
                        Ok(x) => stmts.push(x),
                        Err(e) => return Err(e),
                    }
                },
                _ => break
            }

            if next.is_none() {
                break
            }
        }
        Ok(stmts)
    }

    fn declaration_or_statement(&mut self) -> Result<Node> {
        let token = self.lexer.next().unwrap();
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
            x                                                 => Err(ErrorKind::UnexpectedToken.into()),
        }
    }

    fn variable_declaration(&mut self) -> Result<Node> {
        self.let_or_const()
            .and_then(|kw| self.binding_list().map(|list| Declaration::LexicalDeclaration(kw, vec! [ list ])))
            .map(|decl| Node::Declaration(decl))
    }

    fn let_or_const(&mut self) -> Result<LetOrConst> {
        match self.lexer.next() {
            Some(Token { column: _, line: _, typ: TokenType::Identifier(_, Some(ReservedWord::Let)) }) => {
                Ok(LetOrConst::Let)
            },
            Some(Token { column: _, line: _, typ: TokenType::Identifier(_, Some(ReservedWord::Const)) }) => {
                Ok(LetOrConst::Const)
            },
            x => Err(ErrorKind::UnexpectedToken.into()),
        }
    }

    fn binding_list(&mut self) -> Result<Node> {
        let list = Vec::new();
        let fst = self.lexical_binding();
        Ok(Node::BindingList(list))
    }

    fn lexical_binding(&mut self) -> Result<Node> {
        let identifier = match self.lexer.next() {
            // Some(Ok(Token { column: _, line: _, typ: TokenType::Identifier(_, Some(ReservedWord::Yield)) })) => None,
            // Some(Ok(Token { column: _, line: _, typ: TokenType::Identifier(_, Some(ReservedWord::Await)) })) => None,
            // Some(Ok(t)) if is_reserved_word(t) => Err(ParseError::UnexpectedToken),
            // Some(Ok(t)) if is_identifier(t)    => Ok(t),
            Some(t) => Err(ErrorKind::UnexpectedToken.into()),
            _ => Err(ErrorKind::NotImplemented.into())
            // Some(Err(e)) => Err(e),
            // None => Err(ErrorKind::UnexpectedEof.into()),
        };

        identifier.and_then(|id| {
            let assignment = match self.lexer.next() {
                Some(Token { column: _, line: _, typ: TokenType::Equals }) => self.assignment_expression(),
                _ => Err(ErrorKind::UnexpectedEof.into())
            };

            assignment.map(|assignment| {
                Node::Binding(
                    Binding {
                        identifier: id,
                        assignment: Some(assignment),
                    }
                )
            })
        })
    }

    fn assignment_expression(&mut self) -> Result<Assignment> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn function_declaration(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn class_declaration(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn empty_statement(&mut self) -> Result<Node> {
        Ok(Node::Statement)
    }

    fn block(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn if_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn continue_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn break_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn return_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn with_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn throw_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn try_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn debugger_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn switch_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn do_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn while_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
    }

    fn for_statement(&mut self) -> Result<Node> {
        Err(ErrorKind::NotImplemented.into())
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
