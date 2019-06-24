error_chain! {
    errors {
        UnexpectedEof(line: u32, column: u32) {
            description("unexpected end of file")
            display("Unexpected end of file at {}:{}", line, column)
        }

        UnexpectedChar(c: char, line: u32, column: u32) {
            description("unexpected character")
            display("Unexpected character '{}' at {}:{}", c, line, column)
        }

        InvalidCodePoint(val: String, line: u32, column: u32) {
            description("invalid code point")
            display("Invalid code point {} at {}:{}", val, line, column)
        }
    }
}
