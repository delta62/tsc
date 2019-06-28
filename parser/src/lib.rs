#[macro_use]
extern crate error_chain;
extern crate lexer;

mod errors;

use errors::*;
use lexer::{Lexer,TokenType};

pub enum Assignment {
    ConditionalExpression,
    YieldExpression,
    ArrowFunction,
    AsyncArrowFunction,
    LeftHandSideExpression,
}

pub struct Script {
    pub body: Vec<StatementListItem>,
}

pub enum StatementListItem {
    Statement(Statement),
    Declaration(Declaration),
}

pub enum Statement {
    BlockStatement,
    VariableStatement,
    EmptyStatement,
    ExpressionStatement,
    IfStatement,
    BreakableStatement,
    ContinueStatement,
    BreakStatement,
    ReturnStatement,
    WithStatement,
    LabelledStatement,
    ThrowStatement,
    TryStatement,
    DebuggerStatement,
}

pub enum Declaration {
    HoistableDeclaration(HoistableDeclaration),
    ClassDeclaration,
    LexicalDeclaration,
}

pub enum HoistableDeclaration {
    FunctionDeclaration,
    GeneratorDefinition,
    AsyncFunctionDefinition,
    AsyncGeneratorDefinition,
}

pub struct Parser<I>
where I: Iterator<Item = char> {
    lexer:       Lexer<I>,
    diagnostics: Vec<Error>,
}

impl<I> Parser<I>
where I: Iterator<Item = char> {
    pub fn new(lexer: Lexer<I>) -> Parser<I> {
        Parser {
            lexer:       lexer,
            diagnostics: Vec::new(),
        }
    }

    pub fn diagnostics(&self) -> &Vec<Error> {
        &self.diagnostics
    }

    pub fn script(&mut self) -> Script {
        let mut statements = Vec::new();

        loop {
            match self.lexer.next() {
                Some(t) => {
                    match t.typ {
                        TokenType::Semicolon => statements.push(StatementListItem::Statement(Statement::EmptyStatement)),
                        _                    => self.diagnostics.push(ErrorKind::UnexpectedToken(t).into()),
                    }
                },
                None => break,
            }
        }

        Script { body: statements }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_empty_script() {
        let res = parse("");
        assert_eq!(res.body.len(), 0);
    }

    #[test]
    fn parses_empty_statement() {
        let res = parse(";");
        assert_eq!(res.body.len(), 1);
    }

    fn parse(input: &str) -> Script {
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);
        parser.script()
    }
}
