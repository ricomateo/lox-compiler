use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, Token, TokenType},
};

pub fn compile(source: String) -> Result<(), &'static str> {
    let mut parser = Parser::new(source);
    parser.advance();
    // expression(); TODO
    parser.consume(TokenType::Eof, "Expected end of expression");
    parser.end_compiler();
    if parser.had_error {
        // TODO: consider returning an actual error here
        return Err("");
    }
    Ok(())
}

struct Parser {
    current: Option<Token>,
    previous: Option<Token>,
    had_error: bool,
    panic_mode: bool,
    // TODO: consider removing scanner from here
    scanner: Scanner,
    chunk: Chunk,
}

impl Parser {
    fn new(source: String) -> Self {
        Self {
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
            scanner: Scanner::new(source),
            chunk: Chunk::new(),
        }
    }

    fn advance(&mut self) {
        loop {
            let token = self.scanner.scan_token();
            if token.kind != TokenType::Error {
                break;
            }
            self.current = Some(token);
            self.error_at_current("");
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if let Some(token) = &self.current {
            if token.kind == token_type {
                self.advance();
                return;
            }
        }
        self.error_at_current(message);
    }

    fn emit_byte(&mut self, byte: OpCode) {
        let line = self.previous.clone().unwrap().line;
        self.chunk.write(byte, line);
    }

    fn end_compiler(&mut self) {
        self.emit_byte(OpCode::Return);
    }

    fn error_at_current(&mut self, message: &str) {
        let token = &self.current.clone().unwrap();
        self.error_at(token, message);
    }

    fn error_at(&mut self, token: &Token, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        match token.kind {
            TokenType::Eof => {
                eprint!(" at end");
            }
            // Do nothing
            TokenType::Error => (),
            _ => {
                // TODO: check if it is correct to print the lexeme here
                // https://craftinginterpreters.com/compiling-expressions.html#handling-syntax-errors
                eprint!(" at '{}'", token.lexeme);
            }
        }
        eprintln!(": {message}");
        self.had_error = true;
    }
}
