#[macro_use]
extern crate error_chain;
extern crate lexer;

mod errors;

use errors::*;
use lexer::{Lexer,ReservedWord,Token,TokenType};

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

pub struct VariableDeclaration {
    pub identifier: String,
    pub initializer: Option<AssignmentExpression>,
}

pub enum AssignmentExpression {
    ConditionalExpression,
    YieldExpression,
    ArrowFunction,
    AsyncArrowFunction,
    // TODO LeftHandSideExpression,
}

pub enum Statement {
    BlockStatement(Vec<StatementListItem>),
    VariableStatement(Vec<VariableDeclaration>),
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
                        TokenType::Identifier(_, Some(ReservedWord::Debugger)) =>
                            stmts.push(StatementListItem::Statement(self.debugger())),
                        TokenType::Identifier(_, Some(ReservedWord::Var)) =>
                            stmts.push(StatementListItem::Statement(self.var_statement())),
                        _                    => self.diagnostics.push(ErrorKind::UnexpectedToken(t.line, t.column).into()),
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

    fn var_statement(&mut self) -> Statement {
        let mut decls = Vec::new();
        let id = match self.lexer.next() {
            Some(t) => {
                match t.typ {
                    TokenType::Identifier(x, Some(ReservedWord::Yield))
                    | TokenType::Identifier(x, Some(ReservedWord::Await))
                    | TokenType::Identifier(x, None) => Some(x),
                    _ => None
                }
            },
            None => None,
        };

        id.map(|id| {
            decls.push(VariableDeclaration {
                identifier: id,
                initializer: None,
            });
        });

        self.expect_next(TokenType::Semicolon);

        Statement::VariableStatement(decls)
    }

    fn expect_next(&mut self, expected: TokenType) {
        match self.lexer.next() {
            Some(ref t) if t.typ != expected && is_line_terminator(t) => {
                self.diagnostics.push(ErrorKind::UnexpectedToken(t.line, t.column).into());
            },
            None if expected != TokenType::Semicolon => {
                self.diagnostics.push(ErrorKind::UnexpectedEof.into());
            },
            _ => (),
        }
    }
}

fn is_line_terminator(token: &Token) -> bool {
    match token.typ {
        TokenType::LineTerminator(_) => true,
        _ => false,
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

    #[test]
    fn inserts_semicolon_at_newline() {
        let res = parse("debugger\n");
        assert_eq!(res.body.len(), 1);
    }

    #[test]
    fn inserts_semicolon_at_eof() {
        verify_single("debugger");
    }

    #[test]
    fn parses_var_stmt() {
        verify_single("var test;");
    }

    #[test]
    fn allows_var_named_yield() {
        verify_single("var yield;");
    }

    #[test]
    fn allows_var_named_await() {
        verify_single("var await;");
    }

    fn parse(input: &str) -> Script {
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);
        parser.script()
    }

    fn verify_single(input: &str) {
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);
        let res = parser.script();
        assert_eq!(parser.diagnostics.len(), 0, "parse error");
        assert_eq!(res.body.len(), 1, "Expected exactly one node to be parsed");
    }
}
