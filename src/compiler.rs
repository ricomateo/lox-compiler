use std::usize;

use crate::{
    chunk::OpCode,
    declaration::{Declaration, DeclarationKind, Statement},
    expr::Expr,
    value::{Function, Value},
};

use crate::chunk::Chunk;

use crate::expr::Literal;
use crate::scanner::Token;
use crate::scanner::TokenType;

pub struct Compiler {
    function: Function,
    function_type: FunctionType,
    current_line: usize,
    locals: Vec<Local>,
    scope_depth: usize,
    /// Contains the identifiers of the defined global variables
    constant_identifiers: Vec<String>,
}

#[derive(Default)]
pub struct Local {
    pub name: String,
    pub depth: usize,
}

#[derive(PartialEq)]
pub enum FunctionType {
    /// Function body code
    Function,
    /// Top-level code
    Script,
}

impl Compiler {
    pub fn new() -> Self {
        // Claims the first stack slot for the VM’s internal use
        let local = Local::default();
        Self {
            function: Function::default(),
            function_type: FunctionType::Script,
            current_line: 0,
            locals: vec![local],
            scope_depth: 0,
            constant_identifiers: Vec::new(),
        }
    }

    pub fn with_enclosing(compiler: &Compiler) -> Self {
        // TODO: check if it is correct to copy current_line and scope_depth from the enclosing
        // Perhaps it is also necessary to copy constants?
        Self {
            function: Function::default(),
            function_type: FunctionType::Function,
            current_line: compiler.current_line,
            locals: Vec::new(),
            scope_depth: compiler.scope_depth,
            constant_identifiers: Vec::new(),
        }
    }

    pub fn compile(
        &mut self,
        declarations: &Vec<Declaration>,
    ) -> Result<Function, CompilationError> {
        for declaration in declarations {
            self.compile_declaration(declaration)?;
        }
        self.end_compiler();
        Ok(self.function.clone())
    }

    fn compile_declaration(&mut self, declaration: &Declaration) -> Result<(), CompilationError> {
        self.current_line = declaration.line;
        match &declaration.inner {
            DeclarationKind::Statement(Statement::PrintStatement(expr)) => {
                self.compile_expr(&expr)?;
                self.emit_byte(OpCode::Print, self.current_line);
            }
            DeclarationKind::Statement(Statement::ExprStatement(expr)) => {
                self.compile_expr(&expr)?;
                self.emit_byte(OpCode::Pop, self.current_line);
            }
            DeclarationKind::VariableDeclaration { name, initializer } => {
                if let Some(expr) = initializer {
                    self.compile_expr(&expr)?;
                } else {
                    self.emit_byte(OpCode::Nil, self.current_line);
                }
                self.declare_variable(name.clone())?;
                if self.scope_depth > 0 {
                    return Ok(());
                }
                let constant_index = self.identifier_constant(name.clone());
                self.emit_byte(OpCode::DefineGlobal(constant_index), self.current_line);
            }
            DeclarationKind::Block(declarations) => {
                self.begin_scope();
                for declaration in declarations {
                    self.compile_declaration(declaration)?;
                }
                self.end_scope();
            }
            DeclarationKind::Statement(Statement::IfStatement {
                condition,
                then_branch,
                else_branch,
            }) => {
                self.compile_expr(condition)?;

                // Emit jumpp if false (placeholder)
                let then_jump = self.emit_jump(OpCode::JumpIfFalse(0));

                // Pop the condition
                self.emit_byte(OpCode::Pop, self.current_line);

                // Compile then branch
                self.compile_declaration(then_branch)?;

                // Emit jump over else
                let else_jump = self.emit_jump(OpCode::Jump(0));

                // Patch the jump to else branch
                self.patch_jump(then_jump);

                // Pop the condition again if else exists
                self.emit_byte(OpCode::Pop, self.current_line);

                // Compile else branch if it exists
                if let Some(else_branch) = else_branch {
                    self.compile_declaration(else_branch)?;
                }

                // Patch else jump
                self.patch_jump(else_jump);
            }
            DeclarationKind::Statement(Statement::WhileStatement { condition, body }) => {
                // Remember the beginning of the loop
                let loop_start = self.current_chunk().chunk.len();

                self.compile_expr(condition)?;

                // Jump out of the loop if the condition is false
                let exit_jump = self.emit_jump(OpCode::JumpIfFalse(0));

                // Pop the condition
                self.emit_byte(OpCode::Pop, self.current_line);

                // Compile the loop body
                self.compile_declaration(body)?;

                // Jump back to the start of the loop
                self.emit_loop(loop_start);

                // Patch the exit jump
                self.patch_jump(exit_jump);

                // Pop the condition when exiting the loop
                self.emit_byte(OpCode::Pop, self.current_line);
            }
            DeclarationKind::Statement(Statement::ForStatement {
                initializer_clause,
                condition_clause,
                increment_clause,
                body,
            }) => {
                // Explanation: https://craftinginterpreters.com/image/jumping-back-and-forth/for.png
                self.begin_scope();
                if let Some(initializer_clause) = *initializer_clause.clone() {
                    self.compile_declaration(&initializer_clause)?;
                }
                let mut loop_start = self.current_chunk().chunk.len();
                let mut exit_jump = None;

                if let Some(condition_clause) = condition_clause {
                    self.compile_expr(condition_clause)?;
                    exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse(usize::MAX)));
                    self.emit_byte(OpCode::Pop, self.current_line); // Pop conditon clause
                }

                if let Some(increment_clause) = increment_clause {
                    // The increment clause must be executed at the end of each loop,
                    // thats why here we jump right to the body
                    let body_jump = self.emit_jump(OpCode::Jump(usize::MAX));
                    let increment_start = self.current_chunk().chunk.len();
                    self.compile_expr(increment_clause)?;
                    self.emit_byte(OpCode::Pop, self.current_line);
                    self.emit_loop(loop_start);
                    loop_start = increment_start;
                    self.patch_jump(body_jump);
                }

                self.compile_declaration(body)?;
                self.emit_loop(loop_start);

                if let Some(exit_jump) = exit_jump {
                    self.patch_jump(exit_jump);
                    self.emit_byte(OpCode::Pop, self.current_line);
                }
                self.end_scope();
            }
            DeclarationKind::Statement(Statement::FunctionDeclaration {
                name,
                parameters,
                body,
            }) => {
                let mut compiler = Compiler::with_enclosing(self);
                compiler.begin_scope();
                for param in parameters {
                    compiler.declare_variable(param.clone())?;
                }
                compiler.compile_declaration(body)?;
                compiler.end_scope();
                compiler.end_compiler();
                let mut function = compiler.function;
                function.name = name.clone();
                function.arity = parameters.len();
                let constant_index = self.make_constant(Value::Function(function));
                self.emit_byte(OpCode::Constant(constant_index), self.current_line);

                // TODO: check this
                let constant_index = self.identifier_constant(name.clone());
                self.emit_byte(OpCode::DefineGlobal(constant_index), self.current_line);
            }
            DeclarationKind::Statement(Statement::ReturnStatement { result }) => {
                if self.function_type == FunctionType::Script {
                    return Err(CompilationError::ReturnStatementInTopLevelCode(
                        self.current_line,
                    ));
                }
                if let Some(result) = result {
                    self.compile_expr(result)?;
                } else {
                    self.emit_byte(OpCode::Nil, self.current_line);
                }
                self.emit_byte(OpCode::Return, self.current_line);
            }
        }
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompilationError> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                self.compile_binary(operator, left, right)?;
            }
            Expr::Unary { operator, right } => {
                self.compile_unary(operator, right)?;
            }
            Expr::Literal(literal) => {
                self.compile_literal(literal);
            }
            Expr::Grouping { expression } => {
                self.compile_expr(expression)?;
            }
            Expr::Variable { name } => {
                if let Some(local_index) = self.resolve_local(name) {
                    self.emit_byte(OpCode::GetLocal(local_index), self.current_line);
                } else {
                    // If the variable does not exist as local nor global, then it is undefined
                    // if !self.global_variable_defined(name) {
                    //     return Err(CompilationError::UndefinedVariable(
                    //         name.clone(),
                    //         self.current_line,
                    //     ));
                    // }
                    let constant_index = self.identifier_constant(name.clone());
                    self.emit_byte(OpCode::GetGlobal(constant_index), self.current_line);
                }
            }
            Expr::VariableAssignment { name, value } => {
                // TODO: revisar bien esto
                self.compile_expr(value)?;
                if let Some(local_index) = self.resolve_local(name) {
                    self.emit_byte(OpCode::SetLocal(local_index), self.current_line);
                } else {
                    // If the variable does not exist as local nor global, then it is undefined
                    // if !self.global_variable_defined(name) {
                    //     return Err(CompilationError::UndefinedVariable(
                    //         name.clone(),
                    //         self.current_line,
                    //     ));
                    // }
                    let constant_index = self.identifier_constant(name.clone());
                    self.define_variable(constant_index);
                }
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                match operator.kind {
                    TokenType::And => {
                        self.compile_expr(left)?;

                        // If left is false, jump to end
                        let end_jump = self.emit_jump(OpCode::JumpIfFalse(0));

                        self.emit_byte(OpCode::Pop, self.current_line);

                        self.compile_expr(right)?;
                        self.patch_jump(end_jump);
                    }
                    TokenType::Or => {
                        self.compile_expr(left)?;

                        // If left is true, jump to else
                        let else_jump = self.emit_jump(OpCode::JumpIfFalse(0));

                        // If left is true, jump to end
                        let end_jump = self.emit_jump(OpCode::Jump(0));

                        self.patch_jump(else_jump);

                        self.emit_byte(OpCode::Pop, self.current_line);

                        self.compile_expr(right)?;
                        self.patch_jump(end_jump);
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Call {
                function_identifier,
                arguments,
            } => {
                self.compile_expr(&function_identifier)?;
                // Each argument expression generates code that leaves its value on the stack in preparation for the call
                for arg in arguments {
                    self.compile_expr(arg)?;
                }
                let arg_count = arguments.len();
                self.emit_byte(OpCode::Call(arg_count), self.current_line);
            }
        }
        Ok(())
    }

    fn global_variable_defined(&mut self, name: &String) -> bool {
        self.constant_identifiers.contains(&name)
    }

    fn define_variable(&mut self, constant_index: usize) {
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
                return Err(CompilationError::DuplicateLocalVariable(name));
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

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    // Emit a loop instruction to jump back to the given position (the index in the chunk where the loop starts)
    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.current_chunk().chunk.len() - loop_start + 1;
        self.emit_byte(OpCode::Loop(offset), self.current_line);
    }

    // Emit a jump instruction with a placeholder offset
    // Returns the location of the jump instruction in the chunk
    fn emit_jump(&mut self, jump_opcode: OpCode) -> usize {
        let offset = self.current_chunk().chunk.len();
        self.emit_byte(jump_opcode, self.current_line);
        offset
    }

    fn patch_jump(&mut self, jump_pos: usize) {
        let jump_offset = self.current_chunk().chunk.len() - jump_pos - 1;

        match &mut self.current_chunk().chunk[jump_pos] {
            OpCode::Jump(offset) | OpCode::JumpIfFalse(offset) => {
                *offset = jump_offset;
            }
            _ => panic!("Tried to patch a non-jump opcode"),
        }
    }

    /// Returns the index of the local variable with the given name, if exists.
    /// Otherwise returns None
    fn resolve_local(&self, name: &String) -> Option<usize> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == *name {
                return Some(i);
            }
        }
        None
    }

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
        self.current_chunk().write(byte, line);
    }

    fn make_constant(&mut self, value: Value) -> usize {
        self.current_chunk().add_constant(value)
    }

    fn emit_constant(&mut self, value: Value, line: usize) {
        let constant_index = self.make_constant(value);
        self.emit_byte(OpCode::Constant(constant_index), line);
    }

    fn end_compiler(&mut self) {
        self.emit_byte(OpCode::Nil, self.current_line);
        self.emit_byte(OpCode::Return, self.current_line);
    }

    fn identifier_constant(&mut self, name: String) -> usize {
        // Here we add the identifier in the constant_identifiers vec
        // This is then used to check whether the global variable exists
        self.constant_identifiers.push(name.clone());
        self.make_constant(Value::String(name))
    }

    fn compile_binary(
        &mut self,
        operator: &Token,
        left: &Expr,
        right: &Expr,
    ) -> Result<(), CompilationError> {
        self.compile_expr(left)?;
        self.compile_expr(right)?;

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
        Ok(())
    }

    fn compile_unary(&mut self, operator: &Token, right: &Expr) -> Result<(), CompilationError> {
        self.compile_expr(right)?;

        match operator.kind {
            TokenType::Minus => self.emit_byte(OpCode::Negate, operator.line),
            TokenType::Bang => self.emit_byte(OpCode::Not, operator.line),
            _ => unreachable!(),
        }
        Ok(())
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
                self.emit_constant(Value::String(string.clone()), line);
            }
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum CompilationError {
    #[error("Duplicate local variable: '{0}'")]
    DuplicateLocalVariable(String),
    #[error("Undefined variable: '{0}' at line {1}")]
    UndefinedVariable(String, usize),
    #[error("Cannot return from top level code (line {0})")]
    ReturnStatementInTopLevelCode(usize),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk::{Chunk, OpCode};
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    // ---------- Helpers ----------

    /// Compile source code into a bytecode chunk
    fn compile_source(source: String) -> Result<Chunk, CompilationError> {
        let tokens = Scanner::new(source).scan();
        let declarations = Parser::new(tokens).parse();
        Compiler::new().compile(&declarations).map(|f| f.chunk)
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

    // ---------- Tests: Types ----------

    /// Test compiling a literal number
    /// Expr: 42.0

    #[test]
    fn test_literal_number() {
        let chunk = compile_source("42.0;".to_string()).unwrap();

        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(42.0)); // Check if constant value is 42.0
        assert_eq!(opcode_at(&chunk, 1), OpCode::Pop); // Check if second opcode is Pop
        assert_eq!(opcode_at(&chunk, 2), OpCode::Return);
    }

    /// Test compiling literal booleans and nil
    /// Expr: true, false, nil

    #[test]
    fn test_literal_bool_true_false_nil() {
        let chunk_true = compile_source("true;".to_string()).unwrap();
        let chunk_false = compile_source("false;".to_string()).unwrap();
        let chunk_nil = compile_source("nil;".to_string()).unwrap();

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

    // ---------- Tests: Comparison operators ----------

    /// Test compiling a unary minus expression
    /// Expr: -3.0

    /// Chunk: [CONSTANT 0, NEGATE, POP, RETURN]

    #[test]
    fn test_unary_minus() {
        let source = "-3.0;".to_string();

        let chunk = compile_source(source).unwrap();

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
        let source = "!false;".to_string();

        let chunk = compile_source(source).unwrap();

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
        let source = "1.0 + 2.0;".to_string();

        let chunk = compile_source(source).unwrap();

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
        let source = "1.0 == 1.0;".to_string();

        let chunk_eq = compile_source(source).unwrap();

        assert!(matches!(opcode_at(&chunk_eq, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_eq, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_eq, 0).unwrap(), Value::Number(1.0)); // Check if first constant value is 1.0
        assert_eq!(constant_value_at(&chunk_eq, 1).unwrap(), Value::Number(1.0)); // Check if second constant value is 1.0
        assert_eq!(opcode_at(&chunk_eq, 2), OpCode::Equal); // Check if third opcode is Equal
        assert_eq!(opcode_at(&chunk_eq, 3), OpCode::Pop); // Check if fourth opcode is Pop
        assert_eq!(opcode_at(&chunk_eq, 4), OpCode::Return);

        let source = "1.0 != 2.0;".to_string();
        let chunk_ne = compile_source(source).unwrap();

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
        let source = "2.0 > 1.0;".to_string();

        let chunk_gt = compile_source(source).unwrap();

        assert!(matches!(opcode_at(&chunk_gt, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_gt, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_gt, 0).unwrap(), Value::Number(2.0)); // Check if first constant value is 2.0
        assert_eq!(constant_value_at(&chunk_gt, 1).unwrap(), Value::Number(1.0)); // Check if second constant value is 1.0
        assert_eq!(opcode_at(&chunk_gt, 2), OpCode::Greater); // Check if third opcode is Greater
        assert_eq!(opcode_at(&chunk_gt, 3), OpCode::Pop); // Check if fourth opcode is Pop
        assert_eq!(opcode_at(&chunk_gt, 4), OpCode::Return);

        let source = "1.0 < 2.0;".to_string();
        let chunk_lt = compile_source(source).unwrap();

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
        let source = "2.0 >= 2.0;".to_string();
        let chunk_ge = compile_source(source).unwrap();

        assert!(matches!(opcode_at(&chunk_ge, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_ge, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_ge, 0).unwrap(), Value::Number(2.0)); // Check if first constant value is 2.0
        assert_eq!(constant_value_at(&chunk_ge, 1).unwrap(), Value::Number(2.0)); // Check if second constant value is 2.0
        assert_eq!(opcode_at(&chunk_ge, 2), OpCode::Less); // Check if third opcode is Less
        assert_eq!(opcode_at(&chunk_ge, 3), OpCode::Not); // Check if fourth opcode is Not
        assert_eq!(opcode_at(&chunk_ge, 4), OpCode::Pop); // Check if fifth opcode is Pop
        assert_eq!(opcode_at(&chunk_ge, 5), OpCode::Return);

        let source = "2.0 <= 2.0;".to_string();
        let chunk_le = compile_source(source).unwrap();

        assert!(matches!(opcode_at(&chunk_le, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert!(matches!(opcode_at(&chunk_le, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk_le, 0).unwrap(), Value::Number(2.0)); // Check if first constant value is 2.0
        assert_eq!(constant_value_at(&chunk_le, 1).unwrap(), Value::Number(2.0)); // Check if second constant value is 2.0
        assert_eq!(opcode_at(&chunk_le, 2), OpCode::Greater); // Check if third opcode is Greater
        assert_eq!(opcode_at(&chunk_le, 3), OpCode::Not); // Check if fourth opcode is Not
        assert_eq!(opcode_at(&chunk_le, 4), OpCode::Pop); // Check if fifth opcode is Pop
        assert_eq!(opcode_at(&chunk_le, 5), OpCode::Return);
    }

    // ---------- Tests: Global variables ----------

    /// Source code: var a = 42;

    /// Tokens: [VAR, IDENTIFIER(a), EQUAL, NUMBER(42), SEMICOLON]

    /// Declaration: Statement(VariableDeclaration { name: "a", initializer: Some(Literal::Number(42)) })

    /// Chunk: [CONSTANT 0 (42), DEFINE_GLOBAL "a", RETURN]

    #[test]
    fn test_global_variable_declaration() {
        // ---------- Arrange ----------
        let source = "var a = 42;".to_string();

        // ---------- Act ----------
        let chunk = compile_source(source).unwrap();

        // ---------- Assert ----------
        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(42.0)); // Check if constant value is 42.0

        // DEFINE_GLOBAL (with constant "a")
        if let OpCode::DefineGlobal(idx) = opcode_at(&chunk, 1) {
            assert_eq!(chunk.constant_at(idx), Value::String("a".to_string())); // Check if global variable is "a"
        } else {
            panic!("Expected DefineGlobal opcode");
        }

        assert_eq!(opcode_at(&chunk, 2), OpCode::Return); // Check if third opcode is Return
    }

    /// Source code: var a;

    /// Tokens: [VAR, IDENTIFIER(a), SEMICOLON]

    /// Declaration: Statement(VariableDeclaration { name: "a", initializer: None })

    /// Chunk: [NIL, DEFINE_GLOBAL "a", RETURN]

    #[test]
    fn test_global_variable_declaration_without_initializer() {
        // ---------- Arrange ----------
        let source = "var a;".to_string();

        // ---------- Act ----------
        let chunk = compile_source(source).unwrap();

        // ---------- Assert ----------
        assert_eq!(opcode_at(&chunk, 0), OpCode::Nil); // Check if first opcode is Nil

        if let OpCode::DefineGlobal(idx) = opcode_at(&chunk, 1) {
            assert_eq!(chunk.constant_at(idx), Value::String("a".to_string())); // Check if global variable is "a"
        } else {
            panic!("Expected DefineGlobal opcode");
        }

        assert_eq!(opcode_at(&chunk, 2), OpCode::Return); // Check if third opcode is Return
    }

    /// Source code: var a = 1; print a;

    /// Tokens: [VAR, IDENTIFIER(a), EQUAL, NUMBER(1), SEMICOLON, PRINT, IDENTIFIER(a), SEMICOLON]

    /// Declaration: Statement(VariableDeclaration { name: "a", initializer: Some(Literal::Number(1)) }),
    ///              Statement(PrintStatement(Variable { name: "a" }))

    /// Chunk: [CONSTANT 0 (1), DEFINE_GLOBAL "a", GET_GLOBAL "a", PRINT, RETURN]
    #[test]
    fn test_global_variable_get_and_print() {
        // ---------- Arrange ----------
        let source = "var a = 1; print a;".to_string();

        // ---------- Act ----------
        let chunk = compile_source(source).unwrap();

        // ---------- Assert ----------
        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(1.0)); // Check if constant value is 1.0

        if let OpCode::DefineGlobal(idx) = opcode_at(&chunk, 1) {
            assert_eq!(chunk.constant_at(idx), Value::String("a".to_string())); // Check if global variable is "a"
        } else {
            panic!("Expected DefineGlobal opcode");
        }

        if let OpCode::GetGlobal(idx) = opcode_at(&chunk, 2) {
            assert_eq!(chunk.constant_at(idx), Value::String("a".to_string())); // Check if global variable is "a"
        } else {
            panic!("Expected GetGlobal opcode");
        }

        assert_eq!(opcode_at(&chunk, 3), OpCode::Print); // Check if fourth opcode is Print

        assert_eq!(opcode_at(&chunk, 4), OpCode::Return); // Check if fifth opcode is Return
    }

    /// Source code: var a = 1; a = 2;

    /// Tokens: [VAR, IDENTIFIER(a), EQUAL, NUMBER(1), SEMICOLON, IDENTIFIER(a), EQUAL, NUMBER(2), SEMICOLON]

    /// Declaration: Statement(VariableDeclaration { name: "a", initializer: Some(Literal::Number(1)) }),
    ///              Statement(ExprStatement(VariableAssignment { name: "a", value: Literal::Number(2) }))

    /// Chunk: [CONSTANT 0 (1), DEFINE_GLOBAL "a", CONSTANT 1 (2), SET_GLOBAL "a", POP, RETURN]

    #[test]
    fn test_global_variable_assignment() {
        // ---------- Arrange ----------
        let source = "var a = 1; a = 2;".to_string();

        // ---------- Act ----------
        let chunk = compile_source(source).unwrap();

        // ---------- Assert ----------
        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(1.0)); // Check if constant value is 1.0

        if let OpCode::DefineGlobal(idx) = opcode_at(&chunk, 1) {
            assert_eq!(chunk.constant_at(idx), Value::String("a".to_string())); // Check if global variable is "a"
        }

        assert!(matches!(opcode_at(&chunk, 2), OpCode::Constant(_))); // Check if third opcode is Constant
        assert_eq!(constant_value_at(&chunk, 2).unwrap(), Value::Number(2.0)); // Check if constant value is 2.0

        if let OpCode::SetGlobal(idx) = opcode_at(&chunk, 3) {
            assert_eq!(chunk.constant_at(idx), Value::String("a".to_string())); // Check if global variable is "a"
        }

        assert_eq!(opcode_at(&chunk, 4), OpCode::Pop); // Check if fifth opcode is Pop
        assert_eq!(opcode_at(&chunk, 5), OpCode::Return); // Check if sixth opcode is Return
    }

    // ---------- Tests: Local variables ----------

    /// Source code:
    /// {
    ///     var a = 42;
    ///     print a;
    /// }

    /// Tokens: [LEFT_BRACE, VAR, IDENTIFIER(a), EQUAL, NUMBER(42), SEMICOLON, PRINT, IDENTIFIER(a), SEMICOLON, RIGHT_BRACE]

    /// Declarations:
    /// Block([
    ///     VariableDeclaration { name: "a", initializer: Some(Literal::Number(42)) },
    ///     PrintStatement(Variable { name: "a" })
    /// ])

    /// Chunk: [CONSTANT 0 (42), GET_LOCAL 0, PRINT, POP, RETURN]

    #[test]
    fn test_local_variable_declaration_and_print() {
        // ---------- Arrange ----------
        let source = "{ var a = 42; print a; }".to_string();

        // ---------- Act ----------
        let chunk = compile_source(source).unwrap();

        // ---------- Assert ----------
        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(42.0)); // Check if constant value is 42.0

        assert_eq!(opcode_at(&chunk, 1), OpCode::GetLocal(1)); // Check if third opcode is GetLocal 0

        assert_eq!(opcode_at(&chunk, 2), OpCode::Print); // Check if fourth opcode is Print

        assert_eq!(opcode_at(&chunk, 3), OpCode::Pop); // Check if fifth opcode is Pop

        assert_eq!(opcode_at(&chunk, 4), OpCode::Return); // Check if sixth opcode is Return
    }

    /// Source code:
    /// {
    ///     var a = 1;
    ///     a = 2;
    /// }

    /// Tokens: [LEFT_BRACE, VAR, IDENTIFIER(a), EQUAL, NUMBER(1), SEMICOLON, IDENTIFIER(a), EQUAL, NUMBER(2), SEMICOLON, RIGHT_BRACE]

    /// Declarations:
    /// Block([
    ///     VariableDeclaration { name: "a", initializer: Some(Literal::Number(1)) },
    ///     ExprStatement(VariableAssignment { name: "a", value: Literal::Number(2) })
    /// ])

    /// Chunk: [CONSTANT 0 (1), CONSTANT 1 (2), SET_LOCAL 0, POP, POP, RETURN]

    #[test]
    fn test_local_variable_assignment() {
        // ---------- Arrange ----------
        let source = "{ var a = 1; a = 2; }".to_string();

        // ---------- Act ----------

        let chunk = compile_source(source).unwrap();

        // ---------- Assert ----------

        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(1.0)); // Check if constant value is 1.0

        assert!(matches!(opcode_at(&chunk, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk, 1).unwrap(), Value::Number(2.0)); // Check if constant value is 2.0

        assert_eq!(opcode_at(&chunk, 2), OpCode::SetLocal(1)); // Check if third opcode is Set local variable 'a' at index 0

        assert_eq!(opcode_at(&chunk, 3), OpCode::Pop); // Check if fourth opcode is Pop

        assert_eq!(opcode_at(&chunk, 4), OpCode::Pop); // Check if fifth opcode is Pop fifth

        assert_eq!(opcode_at(&chunk, 5), OpCode::Return); // Check if sixth opcode is Return
    }

    /// Source code:
    /// {
    ///     var a = 1;
    ///     {
    ///         var b = 2;
    ///         print a;
    ///         print b;
    ///     }
    /// }

    /// Tokens: [LEFT_BRACE, VAR, IDENTIFIER(a), EQUAL, NUMBER(1), SEMICOLON,
    ///          LEFT_BRACE, VAR, IDENTIFIER(b), EQUAL, NUMBER(2), SEMICOLON,
    ///          PRINT, IDENTIFIER(a), SEMICOLON,
    ///          PRINT, IDENTIFIER(b), SEMICOLON,
    ///          RIGHT_BRACE, RIGHT_BRACE]

    /// Declarations:
    /// Block([
    ///     VariableDeclaration { name: "a", initializer: Some(Literal::Number(1)) },
    ///     Block([
    ///         VariableDeclaration { name: "b", initializer: Some(Literal::Number( 2)) },
    ///         PrintStatement(Variable { name: "a" }),
    ///         PrintStatement(Variable { name: "b" })
    ///     ])
    /// ])

    /// Chunk: [CONSTANT 0 (1), CONSTANT 1 (2), GET_LOCAL 0, PRINT, GET_LOCAL 1, PRINT, POP, POP, RETURN]

    #[test]
    fn test_nested_local_scopes() {
        // ---------- Arrange ----------
        let source = "{ var a = 1; { var b = 2; print a; print b; } }".to_string();

        // ---------- Act ----------

        let chunk = compile_source(source).unwrap();

        // ---------- Assert ----------

        assert!(matches!(opcode_at(&chunk, 0), OpCode::Constant(_))); // Check if first opcode is Constant
        assert_eq!(constant_value_at(&chunk, 0).unwrap(), Value::Number(1.0)); // Check if constant value is 1.0

        assert!(matches!(opcode_at(&chunk, 1), OpCode::Constant(_))); // Check if second opcode is Constant
        assert_eq!(constant_value_at(&chunk, 1).unwrap(), Value::Number(2.0)); // Check if constant value is 2.0

        assert_eq!(opcode_at(&chunk, 2), OpCode::GetLocal(1)); // Check if third opcode is Get local variable 'a' at index 0

        assert_eq!(opcode_at(&chunk, 3), OpCode::Print); // Check if fourth opcode is Print

        assert_eq!(opcode_at(&chunk, 4), OpCode::GetLocal(2)); // Check if fifth opcode is Get local variable 'b' at index 1

        assert_eq!(opcode_at(&chunk, 5), OpCode::Print); // Check if sixth opcode is Print

        assert_eq!(opcode_at(&chunk, 6), OpCode::Pop); // Check if seventh opcode is Pop

        assert_eq!(opcode_at(&chunk, 7), OpCode::Pop); // Check if eighth opcode is Pop

        assert_eq!(opcode_at(&chunk, 8), OpCode::Return); // Check if ninth opcode is Return
    }

    // ---------- Tests: Local variable errors ----------

    #[test]
    fn test_duplicate_local_variable_error() {
        let source = "{
            var foo = 1;
            var foo = 2;
        }";

        let error = compile_source(source.to_string()).expect_err("Expected compilation error");
        let expected_error = CompilationError::DuplicateLocalVariable("foo".to_string());
        assert_eq!(error, expected_error);
    }

    #[test]
    fn test_undefined_variable_error() {
        // Test variable declaration with undefined variable
        let source = "{
            var a = a;
        }";

        let error = compile_source(source.to_string()).expect_err("Expected compilation error");
        let line = 2;
        let expected_error = CompilationError::UndefinedVariable("a".to_string(), line);
        assert_eq!(error, expected_error);

        // Test assigning an undefined variable
        let source = "{
            a = 1;
        }";

        let error = compile_source(source.to_string()).expect_err("Expected compilation error");
        let expected_error = CompilationError::UndefinedVariable("a".to_string(), line);
        assert_eq!(error, expected_error);

        // Test assigning an undefined variable
        let source = "{
            var a = 1;
        }
        print a;";

        let error = compile_source(source.to_string()).expect_err("Expected compilation error");
        let line = 4;
        let expected_error = CompilationError::UndefinedVariable("a".to_string(), line);
        assert_eq!(error, expected_error);
    }

    // ---------- Tests: If else ----------

    // Source code: if (true) { print 1; } else { print 2; }

    // Tokens: [IF, LEFT_PAREN, TRUE, RIGHT_PAREN, LEFT_BRACE, PRINT, NUMBER(1), SEMICOLON, RIGHT_BRACE, ELSE, LEFT_BRACE, PRINT, NUMBER(2), SEMICOLON, RIGHT_BRACE]

    // Declarations:
    // Statement(IfStatement {
    //     condition: Literal::Bool(true),
    //     then_branch: Block([
    //         PrintStatement(Literal::Number(1))
    //     ]),
    //     else_branch: Some(Block([
    //         PrintStatement(Literal::Number(2))
    //     ]))
    // })

    // Chunk: [TRUE,
    //         JUMP_IF_FALSE 4,
    //         POP,
    //         CONSTANT 0 (1),
    //         PRINT,
    //         JUMP 3,
    //         POP,
    //         CONSTANT 1 (2),
    //         PRINT,
    //         RETURN]

    // Explanation of jumps:
    // - The first jump (JUMP_IF_FALSE 4) skips the then branch if the condition is false. It jumps over the POP, CONSTANT 0, PRINT instructions (3 instructions) plus the POP before the else branch (1 instruction), totaling 4.
    // - The second jump (JUMP 3) skips the else branch after executing the then branch. It jumps over the POP, CONSTANT 1, PRINT instructions (3 instructions).

    // In the example, since the condition is always true, the else branch will never be executed, but the jumps are necessary for correct control flow.

    // The following are executed:
    // 1. TRUE
    // 2. JUMP_IF_FALSE 4 (not taken because condition is true)
    // 3. POP
    // 4. CONSTANT 0 (1)
    // 5. PRINT
    // 6. JUMP 3 (skips else branch)
    // 7. RETURN

    #[test]
    fn test_if_else() {
        // ---------- Arrange ----------
        let source = "if (true) { print 1; } else { print 2; }";

        // ---------- Act ----------
        let chunk = compile_source(source.to_string()).unwrap();

        // ---------- Assert ----------
        assert_eq!(opcode_at(&chunk, 0), OpCode::True); // Check if first opcode is True

        assert_eq!(opcode_at(&chunk, 1), OpCode::JumpIfFalse(4)); // Check if second opcode is JumpIfFalse with correct offset

        assert_eq!(opcode_at(&chunk, 2), OpCode::Pop); // Check if third opcode is Pop

        assert!(matches!(opcode_at(&chunk, 3), OpCode::Constant(_))); // Check if fourth opcode is Constant
        assert_eq!(constant_value_at(&chunk, 3).unwrap(), Value::Number(1.0)); // Check if constant value is 1.0

        assert_eq!(opcode_at(&chunk, 4), OpCode::Print); // Check if fifth opcode is Print

        assert_eq!(opcode_at(&chunk, 5), OpCode::Jump(3)); // Check if sixth opcode is Jump with correct offset

        assert_eq!(opcode_at(&chunk, 6), OpCode::Pop); // Check if seventh opcode is Pop

        assert!(matches!(opcode_at(&chunk, 7), OpCode::Constant(_))); // Check if eighth opcode is Constant
        assert_eq!(constant_value_at(&chunk, 7).unwrap(), Value::Number(2.0)); // Check if constant value is 2.0

        assert_eq!(opcode_at(&chunk, 8), OpCode::Print); // Check if ninth opcode is Print

        assert_eq!(opcode_at(&chunk, 9), OpCode::Return); // Check if tenth opcode is Return
    }

    // ---------- Tests: Logical operators ----------

    // Source code: true and false;

    // Tokens: [TRUE, AND, FALSE, SEMICOLON]

    // Declarations:
    // Statement(ExprStatement(Logical {
    //     left: Literal::Bool(true),
    //     operator: And,
    //     right: Literal::Bool(false)
    // }))

    // Chunk:
    // [TRUE,
    //  JUMP_IF_FALSE 2,  // jump over POP + right expr
    //  POP,
    //  FALSE,
    //  POP,
    //  RETURN]

    // Explanation:
    // - If left is false, JumpIfFalse skips POP and the right expression.

    // The following are executed:
    // 1. TRUE
    // 2. JUMP_IF_FALSE 2 (not taken because condition is true)
    // 3. POP
    // 4. FALSE
    // 5. POP
    // 6. RETURN

    #[test]
    fn test_logical_and() {
        // ---------- Arrange ----------
        let source = "true and false;";

        // ---------- Act ----------
        let chunk = compile_source(source.to_string()).unwrap();

        // ---------- Assert ----------
        assert_eq!(opcode_at(&chunk, 0), OpCode::True);
        assert_eq!(opcode_at(&chunk, 1), OpCode::JumpIfFalse(2));
        assert_eq!(opcode_at(&chunk, 2), OpCode::Pop);
        assert_eq!(opcode_at(&chunk, 3), OpCode::False);
        assert_eq!(opcode_at(&chunk, 4), OpCode::Pop);
        assert_eq!(opcode_at(&chunk, 5), OpCode::Return);
    }

    // Source code: false and true;

    // Tokens: [FALSE, AND, TRUE, SEMICOLON]

    // Declarations:
    // Statement(ExprStatement(Logical {
    //     left: Literal::Bool(false),
    //     operator: And,
    //     right: Literal::Bool(TRUE)
    // }))

    // Chunk:
    // [FALSE,
    //  JUMP_IF_FALSE 2,  // jump over POP + right expr
    //  POP,
    //  TRUE,
    //  POP,
    //  RETURN]

    // Explanation:
    // - If left is false, JumpIfFalse skips POP and the right expression.

    // The following are executed:
    // 1. FALSE
    // 2. JUMP_IF_FALSE 2 (taken because condition is false, jumps over POP and TRUE)
    // 3. POP
    // 4. RETURN

    #[test]
    fn test_logical_and_short_circuit() {
        // ---------- Arrange ----------
        let source = "false and true;";

        // ---------- Act ----------
        let chunk = compile_source(source.to_string()).unwrap();

        // ---------- Assert ----------
        assert_eq!(opcode_at(&chunk, 0), OpCode::False);
        assert_eq!(opcode_at(&chunk, 1), OpCode::JumpIfFalse(2));
        assert_eq!(opcode_at(&chunk, 2), OpCode::Pop);
        assert_eq!(opcode_at(&chunk, 3), OpCode::True);
        assert_eq!(opcode_at(&chunk, 4), OpCode::Pop);
        assert_eq!(opcode_at(&chunk, 5), OpCode::Return);
    }

    // Source code: true or false;

    // Tokens: [TRUE, OR, FALSE, SEMICOLON]

    // Declarations:
    // Statement(ExprStatement(Logical {
    //     left: Literal::Bool(true),
    //     operator: Or,
    //     right: Literal::Bool(false)
    // }))

    // Chunk:
    // [TRUE,
    //  JUMP_IF_FALSE 1,  // if false, skip the following JUMP and evaluate right
    //  JUMP 2,           // if true, skip POP + right
    //  POP,
    //  FALSE,
    //  POP,
    //  RETURN]

    // Explanation of jumps:
    // - JUMP_IF_FALSE 1 jumps over the next JUMP when left is false (so we evaluate right).
    // - JUMP 2 skips POP and the right expression when left is true.

    // The following are executed:
    // 1. TRUE
    // 2. JUMP_IF_FALSE 1 (not taken)
    // 3. JUMP 2 (skips POP and FALSE)
    // 4. POP
    // 5. RETURN

    #[test]
    fn test_logical_or() {
        // ---------- Arrange ----------
        let source = "true or false;";

        // ---------- Act ----------
        let chunk = compile_source(source.to_string()).unwrap();

        // ---------- Assert ----------
        assert_eq!(opcode_at(&chunk, 0), OpCode::True);
        assert_eq!(opcode_at(&chunk, 1), OpCode::JumpIfFalse(1));
        assert_eq!(opcode_at(&chunk, 2), OpCode::Jump(2));
        assert_eq!(opcode_at(&chunk, 3), OpCode::Pop);
        assert_eq!(opcode_at(&chunk, 4), OpCode::False);
        assert_eq!(opcode_at(&chunk, 5), OpCode::Pop);
        assert_eq!(opcode_at(&chunk, 6), OpCode::Return);
    }

    // Source code: false or true;

    // Tokens: [FALSE, OR, TRUE, SEMICOLON]

    // Declarations:
    // Statement(ExprStatement(Logical {
    //     left: Literal::Bool(false),
    //     operator: Or,
    //     right: Literal::Bool(true)
    // }))

    // Chunk:
    // [FALSE,
    //  JUMP_IF_FALSE 1,  // if false, skip the following JUMP and evaluate right
    //  JUMP 2,           // if true, skip POP + right
    //  POP,
    //  TRUE,
    //  POP,
    //  RETURN]

    // Explanation (short-circuit):
    // - Left is false, so JUMP_IF_FALSE is taken and skips the JUMP.
    // - We POP the left and evaluate the right, leaving right’s value on the stack.

    // Executed steps:
    // 1. FALSE
    // 2. JUMP_IF_FALSE 1 (taken; skips JUMP)
    // 3. POP
    // 4. TRUE
    // 5. POP
    // 6. RETURN

    #[test]
    fn test_logical_or_short_circuit() {
        // ---------- Arrange ----------
        let source = "false or true;";

        // ---------- Act ----------
        let chunk = compile_source(source.to_string()).unwrap();

        // ---------- Assert ----------
        assert_eq!(opcode_at(&chunk, 0), OpCode::False);
        assert_eq!(opcode_at(&chunk, 1), OpCode::JumpIfFalse(1));
        assert_eq!(opcode_at(&chunk, 2), OpCode::Jump(2));
        assert_eq!(opcode_at(&chunk, 3), OpCode::Pop);
        assert_eq!(opcode_at(&chunk, 4), OpCode::True);
        assert_eq!(opcode_at(&chunk, 5), OpCode::Pop);
        assert_eq!(opcode_at(&chunk, 6), OpCode::Return);
    }

    #[test]
    fn test_empty_for_loop() {
        let source = "for (;;) {}";
        let chunk = compile_source(source.to_string()).unwrap();
        assert_eq!(chunk.chunk, vec![OpCode::Loop(1), OpCode::Return]);
    }

    #[test]
    fn test_for_loop() {
        let source = "
        for (var i = 0; i < 5; i = i + 1) {
            print i;
        }
        ";
        let chunk = compile_source(source.to_string()).unwrap();

        assert_eq!(
            chunk.constants,
            vec![Value::Number(0.0), Value::Number(5.0), Value::Number(1.0)]
        );
        assert_eq!(
            chunk.chunk,
            vec![
                OpCode::Constant(0), // initializer clause
                //
                OpCode::GetLocal(1), // i  \
                OpCode::Constant(1), // 5  | condition clause i < 5
                OpCode::Less,        // <  /
                //
                OpCode::JumpIfFalse(11), // Jump out of the loop
                OpCode::Pop,             // Pop condition clause
                OpCode::Jump(6),         // Jump to the body
                //
                OpCode::GetLocal(1), //  \
                OpCode::Constant(2), //  | increment clause
                OpCode::Add,         //  | i = i + 1
                OpCode::SetLocal(1), //  /
                //
                OpCode::Pop,      //
                OpCode::Loop(12), // jump back to the loop start
                //
                OpCode::GetLocal(1), // body
                OpCode::Print,       // print i;
                //
                OpCode::Loop(9), // jump to the increment clause
                OpCode::Pop,
                OpCode::Pop,
                OpCode::Return
            ]
        );
    }

    #[test]
    fn test_cannot_access_for_loop_variable_from_outside() {
        let source = "
        for (var i = 0; i < 5; i = i + 1) {
            print i;
        }
        print i; // Should fail
        ";
        let error = compile_source(source.to_string()).expect_err("Expected compilation error");
        let line = 5;
        let expected_error = CompilationError::UndefinedVariable("i".to_string(), line);
        assert_eq!(error, expected_error);
    }
}
