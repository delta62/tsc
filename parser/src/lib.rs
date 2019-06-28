#[macro_use]
extern crate error_chain;
extern crate lexer;

mod errors;

use errors::*;
use lexer::{Lexer,ReservedWord,TokenType};

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
    BlockStatement(Vec<StatementListItem>),
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
        let statements = self.statement_list();
        Script { body: statements }
    }

    fn statement_list(&mut self) -> Vec<StatementListItem> {
        let mut stmts = Vec::new();
        loop {
            match self.lexer.next() {
                Some(t) => {
                    match t.typ {
                        TokenType::Semicolon => stmts.push(StatementListItem::Statement(Statement::EmptyStatement)),
                        TokenType::LeftBrace => stmts.push(StatementListItem::Statement(self.block())),
                        TokenType::Identifier(_, Some(ReservedWord::Debugger)) => stmts.push(StatementListItem::Statement(self.debugger())),
                        _                    => self.diagnostics.push(ErrorKind::UnexpectedToken(t).into()),
                    }
                },
                None => break,
            }
        }
        stmts
    }

    fn block(&mut self) -> Statement {
        let statements = self.statement_list();
        self.expect_next(TokenType::RightBrace);
        Statement::BlockStatement(statements)
    }

    fn debugger(&mut self) -> Statement {
        self.expect_next(TokenType::Semicolon);
        Statement::DebuggerStatement
    }

    fn expect_next(&mut self, expected: TokenType) {
        if let Some(t) = self.lexer.next() {
            if t.typ != expected {
                self.diagnostics.push(ErrorKind::UnexpectedToken(t).into());
            }
        }
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

    #[test]
    fn parses_several_empty_statements() {
        let res = parse(";;;");
        assert_eq!(res.body.len(), 3);
    }

    #[test]
    fn parses_block_statement() {
        let res = parse("{}");
        assert_eq!(res.body.len(), 1);
    }

    #[test]
    fn parses_debug_statement() {
        let res = parse("debugger;");
        assert_eq!(res.body.len(), 1);
    }

    fn parse(input: &str) -> Script {
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);
        parser.script()
    }
}
