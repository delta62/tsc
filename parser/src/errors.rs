use lexer::Token;

error_chain! {

    links {
        Lex(lexer::errors::Error, lexer::errors::ErrorKind);
    }

    errors {
        UnexpectedEof

        NotImplemented

        UnexpectedToken(token: Token) {
            description("unexpected token")
            display("Unexpected token {:?}", token)
        }
    }
}
