use crate::{
    chunk::{Chunk, OpCode, Value},
    scanner::{Scanner, Token, TokenType},
};

pub fn compile(source: String) -> Result<(), &'static str> {
    let mut parser = Parser::new(source);
    parser.advance();
    parser.expression();
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

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    // Returns the next precedence level
    // In the book, this is achieved by simply incrementing the precedence by 1
    fn next(&self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => unreachable!(),
        }
    }
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

    fn binary(&mut self) {
        // At this point we already know we are parsing a binary operator
        // i.e. we already consumed the first operand and the operator,
        // now we need to parse the rest of the expression
        let operator_type = self.previous.clone().unwrap().kind;
        let rule = get_rule(operator_type);
        self.parse_precedence(rule.precedence.next());

        match operator_type {
            TokenType::Plus => {
                self.emit_byte(OpCode::Add);
            }
            TokenType::Minus => {
                self.emit_byte(OpCode::Subtract);
            }
            TokenType::Star => {
                self.emit_byte(OpCode::Multiply);
            }
            TokenType::Slash => {
                self.emit_byte(OpCode::Divide);
            }
            _ => unreachable!(),
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn grouping(&mut self) {
        // Compile the expression wrapped in parentheses
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn number(&mut self) {
        // The token has already been consumed and is stored in self.previous
        let value_lexeme = self.previous.clone().unwrap().lexeme;
        let value = Value::Number(value_lexeme.parse::<f64>().unwrap());
        self.emit_constant(value);
    }

    fn unary(&mut self) {
        let operator_type = self.previous.clone().unwrap().kind;
        // Compile the operand
        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => {
                self.emit_byte(OpCode::Negate);
            }
            _ => unreachable!(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let previous_token_type = self.previous.clone().unwrap().kind;
        let Some(prefix_rule) = get_rule(previous_token_type).prefix else {
            self.error("Expect expression.");
            return;
        };
        prefix_rule(self);

        while precedence <= get_rule(self.current.clone().unwrap().kind).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous.clone().unwrap().kind).infix.unwrap();
            infix_rule(self);
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let constant_index = self.make_constant(value);
        self.emit_byte(OpCode::Constant(constant_index));
    }

    /// Adds a constant the `value` constant to the chunk and returns its index
    fn make_constant(&mut self, value: Value) -> usize {
        let constant_index = self.chunk.add_constant(value);
        constant_index
    }

    fn error_at_current(&mut self, message: &str) {
        let token = &self.current.clone().unwrap();
        self.error_at(token, message);
    }

    fn error(&mut self, message: &str) {
        let token = self.previous.clone().unwrap();
        self.error_at(&token, message);
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

fn get_rule(token_type: TokenType) -> ParseRule {
    match token_type {
        TokenType::LeftParen => ParseRule::new(Some(Parser::grouping), None, Precedence::None),
        TokenType::Minus => {
            ParseRule::new(Some(Parser::unary), Some(Parser::binary), Precedence::Term)
        }
        TokenType::Plus => ParseRule::new(None, Some(Parser::binary), Precedence::Term),
        TokenType::Slash => ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
        TokenType::Star => ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
        TokenType::Number => ParseRule::new(Some(Parser::number), None, Precedence::None),
        _ => ParseRule::default(),
    }
}

struct ParseRule {
    prefix: Option<fn(&mut Parser)>,
    infix: Option<fn(&mut Parser)>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Parser)>,
        infix: Option<fn(&mut Parser)>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }

    fn default() -> Self {
        Self {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        }
    }
}
