use crate::{
    expr::{Expr, Literal},
    scanner::{Token, TokenType},
};

#[derive(Debug, PartialEq, PartialOrd)]
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

pub struct Parser {
    current: Option<Token>,
    previous: Option<Token>,
    tokens: Vec<Token>,
    current_index: usize,
    had_error: bool,
    panic_mode: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut current = None;
        let mut idx = 0;
        if !tokens.is_empty() {
            current = Some(tokens[0].clone());
            idx = 1;
        }
        Self {
            current,
            previous: None,
            tokens,
            current_index: idx,
            had_error: false,
            panic_mode: false,
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Expr {
        self.advance();

        if self.previous.is_none() {
            self.previous = self.current.clone();
        }

        let previous_token_type = self.previous.clone().unwrap().kind;
        let prefix_rule = get_rule(previous_token_type).prefix;
        if prefix_rule.is_none() {
            self.error("Expect expression.");
            return Expr::Literal(Literal::Number(0.0));
        }

        let mut expr = prefix_rule.unwrap()(self);

        while precedence <= get_rule(self.current.clone().unwrap().kind).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous.clone().unwrap().kind).infix;
            if let Some(rule) = infix_rule {
                expr = rule(self, expr);
            }
        }

        expr
    }

    // ---------- Parsing rules ----------

    pub fn expression(&mut self) -> Expr {
        self.parse_precedence(Precedence::Assignment)
    }

    fn number(&mut self) -> Expr {
        let value = self
            .previous
            .clone()
            .unwrap()
            .lexeme
            .parse::<f64>()
            .unwrap();
        Expr::Literal(Literal::Number(value))
    }

    fn unary(&mut self) -> Expr {
        let operator = self.previous.clone().unwrap();
        let right = self.parse_precedence(Precedence::Unary);
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    fn binary(&mut self, left: Expr) -> Expr {
        let operator = self.previous.clone().unwrap();
        let rule = get_rule(operator.kind);
        let right = self.parse_precedence(rule.precedence.next());
        Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn grouping(&mut self) -> Expr {
        let expr = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
        Expr::Grouping {
            expression: Box::new(expr),
        }
    }

    // ---------- Helpers ----------

    fn advance(&mut self) {
        if let Some(current_token) = &self.current {
            self.previous = Some(current_token.clone());
        }

        loop {
            if self.current_index >= self.tokens.len() {
                self.current = Some(Token {
                    kind: TokenType::Eof,
                    start: 0,
                    length: 0,
                    line: 0,
                    lexeme: String::new(),
                });
                break;
            }

            let token = self.tokens[self.current_index].clone();
            self.current = Some(token.clone());
            self.current_index += 1;

            if token.kind != TokenType::Error {
                break;
            }

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
        self.error(message);
    }

    // ---------- Error handling ----------

    fn error(&mut self, message: &str) {
        if let Some(token) = &self.previous {
            let token = token.clone();
            self.error_at(&token, message);
        }
    }

    fn error_at(&mut self, token: &Token, _message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        match token.kind {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => (),
            _ => eprint!(" at '{}'", token.lexeme),
        }

        self.had_error = true;
    }

    fn error_at_current(&mut self, message: &str) {
        if let Some(token) = &self.current {
            let token = token.clone();
            self.error_at(&token, message);
        } else {
            self.error(message);
        }
    }
}

struct ParseRule {
    prefix: Option<fn(&mut Parser) -> Expr>,
    infix: Option<fn(&mut Parser, Expr) -> Expr>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Parser) -> Expr>,
        infix: Option<fn(&mut Parser, Expr) -> Expr>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
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
        _ => ParseRule::new(None, None, Precedence::None),
    }
}
