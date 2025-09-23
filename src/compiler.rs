use crate::{
    chunk::{Chunk, OpCode, Value},
    expr::Expr,
};

use crate::expr::Literal;
use crate::scanner::Token;
use crate::scanner::TokenType;

pub struct Compiler {
    chunk: Chunk,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
        }
    }

    pub fn compile(&mut self, expr: &Expr) -> Chunk {
        self.compile_expr(expr);
        self.end_compiler();
        self.chunk.clone()
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                self.compile_binary(operator, left, right);
            }
            Expr::Unary { operator, right } => {
                self.compile_unary(operator, right);
            }
            Expr::Literal(literal) => {
                self.compile_literal(literal);
            }
            Expr::Grouping { expression } => {
                self.compile_expr(expression);
            }
        }
    }

    // ---------- Helpers ----------

    fn emit_byte(&mut self, byte: OpCode, line: usize) {
        self.chunk.write(byte, line);
    }

    fn make_constant(&mut self, value: Value) -> usize {
        self.chunk.add_constant(value)
    }

    fn emit_constant(&mut self, value: Value, line: usize) {
        let constant_index = self.make_constant(value);
        self.emit_byte(OpCode::Constant(constant_index), line);
    }

    fn end_compiler(&mut self) {
        self.emit_byte(OpCode::Return, 0);
    }

    fn compile_binary(&mut self, operator: &Token, left: &Expr, right: &Expr) {
        self.compile_expr(left);
        self.compile_expr(right);

        match operator.kind {
            TokenType::Plus => self.emit_byte(OpCode::Add, operator.line),
            TokenType::Minus => self.emit_byte(OpCode::Subtract, operator.line),
            TokenType::Star => self.emit_byte(OpCode::Multiply, operator.line),
            TokenType::Slash => self.emit_byte(OpCode::Divide, operator.line),
            TokenType::EqualEqual => {
                self.emit_byte(OpCode::Equal, operator.line);
            }
            TokenType::BangEqual => {
                // Syntactic sugar: a != b  is the same as !(a == b)
                self.emit_byte(OpCode::Equal, operator.line);
                self.emit_byte(OpCode::Not, operator.line);
            }
            TokenType::Greater => {
                self.emit_byte(OpCode::Greater, operator.line);
            }
            TokenType::GreaterEqual => {
                // Syntactic sugar: a >= b  is the same as !(a < b)
                self.emit_byte(OpCode::Less, operator.line);
                self.emit_byte(OpCode::Not, operator.line);
            }
            TokenType::Less => {
                self.emit_byte(OpCode::Less, operator.line);
            }
            TokenType::LessEqual => {
                // Syntactic sugar: a <= b  is the same as !(a > b)
                self.emit_byte(OpCode::Greater, operator.line);
                self.emit_byte(OpCode::Not, operator.line);
            }
            _ => unreachable!(),
        }
    }

    fn compile_unary(&mut self, operator: &Token, right: &Expr) {
        self.compile_expr(right);

        match operator.kind {
            TokenType::Minus => self.emit_byte(OpCode::Negate, operator.line),
            TokenType::Bang => self.emit_byte(OpCode::Not, operator.line),
            _ => unreachable!(),
        }
    }

    fn compile_literal(&mut self, literal: &Literal) {
        match literal {
            Literal::Number(value) => {
                self.emit_constant(Value::Number(*value), 0);
            }
            Literal::Bool(value) => match value {
                true => self.emit_byte(OpCode::True, 0),
                false => self.emit_byte(OpCode::False, 0),
            },
            Literal::Nil => {
                self.emit_byte(OpCode::Nil, 0);
            }
        }
    }
}
