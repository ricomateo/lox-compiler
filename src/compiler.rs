use crate::{
    chunk::{Chunk, Object, OpCode, Value},
    declaration::{Declaration, DeclarationKind, Statement},
    expr::Expr,
};

use crate::expr::Literal;
use crate::scanner::Token;
use crate::scanner::TokenType;

pub struct Compiler {
    chunk: Chunk,
    current_line: usize,
    locals: Vec<Local>,
    scope_depth: usize,
}

pub struct Local {
    name: String,
    pub depth: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            current_line: 0,
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self, declarations: &Vec<Declaration>) -> Chunk {
        for declaration in declarations {
            self.compile_declaration(declaration);
        }
        self.end_compiler();
        self.chunk.clone()
    }

    fn compile_declaration(&mut self, declaration: &Declaration) {
        self.current_line = declaration.line;
        match &declaration.inner {
            DeclarationKind::Statement(Statement::PrintStatement(expr)) => {
                self.compile_expr(&expr);
                self.emit_byte(OpCode::Print, self.current_line);
            }
            DeclarationKind::Statement(Statement::ExprStatement(expr)) => {
                self.compile_expr(&expr);
                self.emit_byte(OpCode::Pop, self.current_line);
            }
            DeclarationKind::VariableDeclaration { name, initializer } => {
                if let Some(expr) = initializer {
                    self.compile_expr(&expr);
                } else {
                    self.emit_byte(OpCode::Nil, self.current_line);
                }
                // TODO: handle error
                self.declare_variable(name.clone()).unwrap();
                if self.scope_depth > 0 {
                    return;
                }
                let constant_index = self.identifier_constant(name.clone());
                self.emit_byte(OpCode::DefineGlobal(constant_index), self.current_line);
            }
            DeclarationKind::Block(declarations) => {
                self.begin_scope();
                for declaration in declarations {
                    self.compile_declaration(declaration);
                }
                self.end_scope();
            }
        }
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
            Expr::Variable { name } => {
                let constant_index = self.identifier_constant(name.clone());
                self.emit_byte(OpCode::GetGlobal(constant_index), self.current_line);
            }
            Expr::VariableAssignment { name, value } => {
                self.compile_expr(value);
                let constant_index = self.identifier_constant(name.clone());
                self.define_variable(constant_index);
            }
        }
    }

    fn define_variable(&mut self, constant_index: usize) {
        // Do not emit bytecode for local variables
        if self.scope_depth > 0 {
            return;
        }
        self.emit_byte(OpCode::SetGlobal(constant_index), self.current_line);
    }

    fn declare_variable(&mut self, name: String) -> Result<(), CompilationError> {
        // If we are in the global scope, return
        if self.scope_depth == 0 {
            return Ok(());
        }

        // Loop through the local variables in reverse order, looking
        // for duplicate variables in the current scope
        // (Local variables are appended to the array when they are declared,
        //  which means the current scope is always at the end of the array)
        for local in self.locals.iter().rev() {
            // Encountering a local variable with a scope depth
            // smaller than the current means we reached the outer scope
            if local.depth < self.scope_depth {
                break;
            }
            if name == local.name {
                return Err(CompilationError::DuplicateLocalVariable);
            }
        }

        self.add_local(name);
        Ok(())
    }

    fn add_local(&mut self, name: String) {
        let local = Local {
            name,
            depth: self.scope_depth,
        };
        self.locals.push(local);
    }

    // ---------- Helpers ----------

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        // Remove local variables from the last scope
        while let Some(local) = self.locals.last()
            && local.depth > self.scope_depth
        {
            self.locals.pop();
            self.emit_byte(OpCode::Pop, 0);
        }
    }

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
        self.emit_byte(OpCode::Return, self.current_line);
    }

    fn identifier_constant(&mut self, name: String) -> usize {
        self.make_constant(Value::Object(Object::String(name)))
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
        let line = self.current_line;
        match literal {
            Literal::Number(value) => {
                self.emit_constant(Value::Number(*value), line);
            }
            Literal::Bool(value) => match value {
                true => self.emit_byte(OpCode::True, line),
                false => self.emit_byte(OpCode::False, line),
            },
            Literal::Nil => {
                self.emit_byte(OpCode::Nil, line);
            }
            Literal::String(string) => {
                // TODO: set the right line here
                self.emit_constant(Value::Object(Object::String(string.clone())), line);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk::{Chunk, OpCode, Value};
    use crate::scanner::{Token, TokenType};

    // ---------- Helpers ----------

    /// Create a token
    fn token(kind: TokenType) -> Token {
        Token {
            kind,
            start: 0,
            length: 0,
            line: 1,
            lexeme: "".to_string(),
        }
    }

    /// Compile an expression and return chunk
    fn compile(expr: Expr) -> Chunk {
        let mut compiler = Compiler::new();
        let declaration = Declaration {
            inner: DeclarationKind::Statement(Statement::ExprStatement(expr)),
            line: 0,
        };
        let declarations = vec![declaration];
        compiler.compile(&declarations)
    }

    /// Get the opcode at a specific index in the chunk
    fn opcode_at(chunk: &Chunk, index: usize) -> OpCode {
        chunk.instruction_at(index)
    }

    /// If the opcode at the given index is a Constant, return its value
    fn constant_value_at(chunk: &Chunk, index: usize) -> Option<Value> {
        if let OpCode::Constant(constant_index) = chunk.instruction_at(index) {
            Some(chunk.constant_at(constant_index))
        } else {
            None
        }
    }

    // ---------- Tests ----------

    /// Test compiling a literal number
    /// Expr: 42.0

    #[test]
    fn test_literal_number() {
        let expr = Expr::Literal(Literal::Number(42.0));
        let chunk = compile(expr);

        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(42.0)); // Check if constant value is 42.0
        assert_eq!(opcode_at(&chunk, 1), OpCode::Pop); // Check if second opcode is Pop
        assert_eq!(opcode_at(&chunk, 2), OpCode::Return);
    }

    /// Test compiling literal booleans and nil
    /// Expr: true, false, nil

    #[test]
    fn test_literal_bool_true_false_nil() {
        let chunk_true = compile(Expr::Literal(Literal::Bool(true)));
        let chunk_false = compile(Expr::Literal(Literal::Bool(false)));
        let chunk_nil = compile(Expr::Literal(Literal::Nil));

        assert_eq!(opcode_at(&chunk_true, 0), OpCode::True); // Check if first opcode is True
        assert!(matches!(opcode_at(&chunk_true, 1), OpCode::Pop)); // Check if second opcode is Pop
        assert!(matches!(opcode_at(&chunk_true, 2), OpCode::Return));

        assert_eq!(opcode_at(&chunk_false, 0), OpCode::False); // Check if first opcode is False
        assert!(matches!(opcode_at(&chunk_false, 1), OpCode::Pop)); // Check if second opcode is Pop
        assert!(matches!(opcode_at(&chunk_false, 2), OpCode::Return));

        assert_eq!(opcode_at(&chunk_nil, 0), OpCode::Nil); // Check if first opcode is Nil
        assert!(matches!(opcode_at(&chunk_nil, 1), OpCode::Pop)); // Check if second opcode is Pop
        assert!(matches!(opcode_at(&chunk_nil, 2), OpCode::Return));
    }

    /// Test compiling a unary minus expression
    /// Expr: -3.0

    /// Chunk: [CONSTANT 0, NEGATE, POP, RETURN]

    #[test]
    fn test_unary_minus() {
        let expr = Expr::Unary {
            operator: token(TokenType::Minus),
            right: Box::new(Expr::Literal(Literal::Number(3.0))),
        };
        let chunk = compile(expr);

        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(3.0)); // Check if constant value is 3.0
        assert_eq!(opcode_at(&chunk, 1), OpCode::Negate); // Check if second opcode is Negate
        assert_eq!(opcode_at(&chunk, 2), OpCode::Pop); // Check if third opcode is Pop
        assert_eq!(opcode_at(&chunk, 3), OpCode::Return);
    }

    /// Test compiling a unary not expression
    /// Expr: !false

    /// Chunk: [FALSE, NOT, POP, RETURN]

    #[test]
    fn test_unary_not() {
        let expr = Expr::Unary {
            operator: token(TokenType::Bang),
            right: Box::new(Expr::Literal(Literal::Bool(false))),
        };
        let chunk = compile(expr);

        assert_eq!(opcode_at(&chunk, 0), OpCode::False); // Check if first opcode is False
        assert_eq!(opcode_at(&chunk, 1), OpCode::Not); // Check if second opcode is Not
        assert_eq!(opcode_at(&chunk, 2), OpCode::Pop); // Check if third opcode is Pop
        assert_eq!(opcode_at(&chunk, 3), OpCode::Return);
    }

    /// Test compiling a binary addition expression
    /// Expr: 1.0 + 2.0

    /// Chunk: [CONSTANT 0, CONSTANT 1, ADD, POP, RETURN]

    #[test]
    fn test_binary_addition() {
        let expr = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Number(1.0))),
            operator: token(TokenType::Plus),
            right: Box::new(Expr::Literal(Literal::Number(2.0))),
        };
        let chunk = compile(expr);

        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(1.0)); // Check if first constant value is 1.0
        assert_eq!(constant_value_at(&chunk, 1).unwrap(), Value::Number(2.0)); // Check if second constant value is 2.0
        assert_eq!(opcode_at(&chunk, 2), OpCode::Add); // Check if third opcode is Add
        assert_eq!(opcode_at(&chunk, 3), OpCode::Pop); // Check if fourth opcode is Pop
        assert_eq!(opcode_at(&chunk, 4), OpCode::Return);
    }

    /// Test compiling binary comparisons: ==, !=
    /// Expr: 1.0 == 1.0, 1.0 != 2.0

    /// Chunk: [CONSTANT 0, CONSTANT 1, EQUAL, POP, RETURN]
    /// Chunk: [CONSTANT 0, CONSTANT 1, EQUAL, NOT, POP, RETURN]

    #[test]
    fn test_binary_comparison_equal_not_equal() {
        let expr_eq = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Number(1.0))),
            operator: token(TokenType::EqualEqual),
            right: Box::new(Expr::Literal(Literal::Number(1.0))),
        };
        let chunk_eq = compile(expr_eq);

        assert!(matches!(opcode_at(&chunk_eq, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_eq, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_eq, 0).unwrap(), Value::Number(1.0)); // Check if first constant value is 1.0
        assert_eq!(constant_value_at(&chunk_eq, 1).unwrap(), Value::Number(1.0)); // Check if second constant value is 1.0
        assert_eq!(opcode_at(&chunk_eq, 2), OpCode::Equal); // Check if third opcode is Equal
        assert_eq!(opcode_at(&chunk_eq, 3), OpCode::Pop); // Check if fourth opcode is Pop
        assert_eq!(opcode_at(&chunk_eq, 4), OpCode::Return);

        let expr_ne = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Number(1.0))),
            operator: token(TokenType::BangEqual),
            right: Box::new(Expr::Literal(Literal::Number(2.0))),
        };
        let chunk_ne = compile(expr_ne);

        assert!(matches!(opcode_at(&chunk_ne, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_ne, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_ne, 0).unwrap(), Value::Number(1.0)); // Check if first constant value is 1.0
        assert_eq!(constant_value_at(&chunk_ne, 1).unwrap(), Value::Number(2.0)); // Check if second constant value is 2.0
        assert_eq!(opcode_at(&chunk_ne, 2), OpCode::Equal); // Check if third opcode is Equal
        assert_eq!(opcode_at(&chunk_ne, 3), OpCode::Not); // Check if fourth opcode is Not
        assert_eq!(opcode_at(&chunk_ne, 4), OpCode::Pop); // Check if fifth opcode is Pop
        assert_eq!(opcode_at(&chunk_ne, 5), OpCode::Return);
    }

    /// Test compiling binary comparisons: >, <
    /// Expr: 2.0 > 1.0, 1.0 < 2.0

    /// Chunk: [CONSTANT 0, CONSTANT 1, GREATER, POP, RETURN]
    /// Chunk: [CONSTANT 0, CONSTANT 1, LESS, POP, RETURN]

    #[test]
    fn test_binary_comparison_greater_less() {
        let expr_gt = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Number(2.0))),
            operator: token(TokenType::Greater),
            right: Box::new(Expr::Literal(Literal::Number(1.0))),
        };
        let chunk_gt = compile(expr_gt);

        assert!(matches!(opcode_at(&chunk_gt, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_gt, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_gt, 0).unwrap(), Value::Number(2.0)); // Check if first constant value is 2.0
        assert_eq!(constant_value_at(&chunk_gt, 1).unwrap(), Value::Number(1.0)); // Check if second constant value is 1.0
        assert_eq!(opcode_at(&chunk_gt, 2), OpCode::Greater); // Check if third opcode is Greater
        assert_eq!(opcode_at(&chunk_gt, 3), OpCode::Pop); // Check if fourth opcode is Pop
        assert_eq!(opcode_at(&chunk_gt, 4), OpCode::Return);

        let expr_lt = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Number(1.0))),
            operator: token(TokenType::Less),
            right: Box::new(Expr::Literal(Literal::Number(2.0))),
        };
        let chunk_lt = compile(expr_lt);

        assert!(matches!(opcode_at(&chunk_lt, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_lt, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_lt, 0).unwrap(), Value::Number(1.0)); // Check if first constant value is 1.0
        assert_eq!(constant_value_at(&chunk_lt, 1).unwrap(), Value::Number(2.0)); // Check if second constant value is 2.0
        assert_eq!(opcode_at(&chunk_lt, 2), OpCode::Less); // Check if third opcode is Less
        assert_eq!(opcode_at(&chunk_lt, 3), OpCode::Pop); // Check if fourth opcode is Pop
        assert_eq!(opcode_at(&chunk_lt, 4), OpCode::Return);
    }

    /// Test compiling binary comparisons: >=, <=
    /// Expr: 2.0 >= 2.0, 2.0 <= 2.0

    /// Chunk: [CONSTANT 0, CONSTANT 1, LESS, NOT, POP, RETURN]
    /// Chunk: [CONSTANT 0, CONSTANT 1, GREATER, NOT, POP, RETURN]

    #[test]
    fn test_binary_comparison_greater_equal_less_equal() {
        let expr_ge = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Number(2.0))),
            operator: token(TokenType::GreaterEqual),
            right: Box::new(Expr::Literal(Literal::Number(2.0))),
        };
        let chunk_ge = compile(expr_ge);

        assert!(matches!(opcode_at(&chunk_ge, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_ge, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_ge, 0).unwrap(), Value::Number(2.0)); // Check if first constant value is 2.0
        assert_eq!(constant_value_at(&chunk_ge, 1).unwrap(), Value::Number(2.0)); // Check if second constant value is 2.0
        assert_eq!(opcode_at(&chunk_ge, 2), OpCode::Less); // Check if third opcode is Less
        assert_eq!(opcode_at(&chunk_ge, 3), OpCode::Not); // Check if fourth opcode is Not
        assert_eq!(opcode_at(&chunk_ge, 4), OpCode::Pop); // Check if fifth opcode is Pop
        assert_eq!(opcode_at(&chunk_ge, 5), OpCode::Return);

        let expr_le = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Number(2.0))),
            operator: token(TokenType::LessEqual),
            right: Box::new(Expr::Literal(Literal::Number(2.0))),
        };
        let chunk_le = compile(expr_le);

        assert!(matches!(opcode_at(&chunk_le, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_le, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_le, 0).unwrap(), Value::Number(2.0)); // Check if first constant value is 2.0
        assert_eq!(constant_value_at(&chunk_le, 1).unwrap(), Value::Number(2.0)); // Check if second constant value is 2.0
        assert_eq!(opcode_at(&chunk_le, 2), OpCode::Greater); // Check if third opcode is Greater
        assert_eq!(opcode_at(&chunk_le, 3), OpCode::Not); // Check if fourth opcode is Not
        assert_eq!(opcode_at(&chunk_le, 4), OpCode::Pop); // Check if fifth opcode is Pop
        assert_eq!(opcode_at(&chunk_le, 5), OpCode::Return);
    }
}

#[derive(Debug)]
pub enum CompilationError {
    DuplicateLocalVariable,
}
