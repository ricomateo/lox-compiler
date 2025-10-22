use std::collections::HashMap;

use crate::chunk::{Chunk, OpCode, Value};

#[derive(Debug)]
pub struct Vm {
    chunk: Chunk,
    instruction_pointer: usize,
    stack: Vec<Value>,
    pub globals: HashMap<String, Value>,
}

impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            instruction_pointer: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        loop {
            if std::env::var("DEBUG_TRACE").is_ok() {
                self.debug_trace();
            };
            let instruction = self.chunk.instruction_at(self.instruction_pointer);
            self.instruction_pointer += 1;
            match instruction {
                OpCode::Constant(constant_index) => {
                    let constant = self.chunk.constant_at(constant_index);
                    self.stack.push(constant);
                }
                OpCode::Negate => {
                    // let value = self.stack.pop().unwrap();
                    // match value {
                    //     Value::Number(value) => {
                    //         let negated_value = Value::Number(-value);
                    //         self.stack.push(negated_value);
                    //     }
                    //     _ => return Err(VmError::RuntimeError),
                    // }

                    match self.peek(0) {
                        Some(Value::Number(_)) => {
                            let value = self.stack.pop().unwrap();
                            if let Value::Number(v) = value {
                                let negated_value = Value::Number(-v);
                                self.stack.push(negated_value);
                            }
                        }
                        _ => return self.runtime_error("Operand must be a number."),
                    }
                }
                OpCode::Return => {
                    return Ok(());
                }
                // TODO: remove unwraps
                OpCode::Add => {
                    // let b = self.stack.pop().unwrap();
                    // let a = self.stack.pop().unwrap();
                    // match (a, b) {
                    //     (Value::Number(a), Value::Number(b)) => {
                    //         let result = Value::Number(a + b);
                    //         self.stack.push(result);
                    //     }
                    //     _ => return Err(VmError::RuntimeError),
                    // }
                    if self.stack_operands_are_numbers() {
                        self.binary_op_number(|a, b| a + b)?;
                    } else if self.stack_operands_are_strings() {
                        self.concatenate_strings()?;
                    }
                }
                OpCode::Subtract => {
                    // let b = self.stack.pop().unwrap();
                    // let a = self.stack.pop().unwrap();
                    // match (a, b) {
                    //     (Value::Number(a), Value::Number(b)) => {
                    //         let result = Value::Number(a - b);
                    //         self.stack.push(result);
                    //     }
                    //     _ => return Err(VmError::RuntimeError),
                    // }
                    self.binary_op_number(|a, b| a - b)?;
                }
                OpCode::Multiply => {
                    // let b = self.stack.pop().unwrap();
                    // let a = self.stack.pop().unwrap();
                    // match (a, b) {
                    //     (Value::Number(a), Value::Number(b)) => {
                    //         let result = Value::Number(a * b);
                    //         self.stack.push(result);
                    //     }
                    //     _ => return Err(VmError::RuntimeError),
                    // }
                    self.binary_op_number(|a, b| a * b)?;
                }
                OpCode::Divide => {
                    // let b = self.stack.pop().unwrap();
                    // let a = self.stack.pop().unwrap();
                    // match (a, b) {
                    //     (Value::Number(a), Value::Number(b)) => {
                    //         let result = Value::Number(a / b);
                    //         self.stack.push(result);
                    //     }
                    //     _ => return Err(VmError::RuntimeError),
                    // }
                    self.binary_op_number(|a, b| a / b)?;
                }
                OpCode::Nil => {
                    self.stack.push(Value::Nil);
                }
                OpCode::True => {
                    self.stack.push(Value::Bool(true));
                }
                OpCode::False => {
                    self.stack.push(Value::Bool(false));
                }
                OpCode::Not => {
                    let value = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(Self::is_falsey(&value)));
                }
                OpCode::Equal => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(Self::values_equal(&a, &b)));
                }
                // TODO: extract repeated code
                OpCode::Greater => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        self.stack.push(Value::Bool(a > b));
                    } else {
                        return self.runtime_error("Operands must be numbers.");
                    }
                }
                OpCode::Less => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        self.stack.push(Value::Bool(a < b));
                    } else {
                        return self.runtime_error("Operands must be numbers.");
                    }
                }
                OpCode::Print => {
                    let value = self.stack.pop().unwrap();
                    self.print_value(value);
                }
                OpCode::Pop => {
                    // Pop the top of the stack
                    self.stack.pop();
                }
                OpCode::DefineGlobal(constant_index) => {
                    let name = self.get_variable_name(constant_index)?;
                    let value = self.stack.pop().unwrap();
                    self.globals.insert(name, value);
                }
                OpCode::GetGlobal(constant_index) => {
                    let name = self.get_variable_name(constant_index)?;
                    let Some(value) = self.globals.get(&name) else {
                        return self.runtime_error(&format!("Undefined variable '{name}'."));
                    };
                    self.stack.push(value.clone());
                }
                OpCode::SetGlobal(constant_index) => {
                    let name = self.get_variable_name(constant_index)?;
                    if !self.globals.contains_key(&name) {
                        return self.runtime_error(&format!("Undefined variable '{name}'."));
                    }
                    let new_value = self.peek(0).unwrap().clone();
                    self.globals.insert(name, new_value);
                }
                OpCode::GetLocal(slot) => {
                    self.stack.push(self.stack[slot].clone());
                }
                OpCode::SetLocal(slot) => {
                    // Takes the assigned value from the top of the stack
                    // and stores it in the stack slot corresponding to the local variable
                    self.stack[slot] = self.peek(0).unwrap().clone();
                }
                OpCode::Jump(offset) => {
                    self.instruction_pointer += offset;
                }
                OpCode::JumpIfFalse(offset) => {
                    let condition = self.peek(0).unwrap();
                    if Self::is_falsey(&condition) {
                        self.instruction_pointer += offset;
                    }
                }
                OpCode::Loop(offset) => {
                    self.instruction_pointer -= offset;
                }
            }
        }
    }

    fn get_variable_name(&mut self, constant_index: usize) -> Result<String, VmError> {
        let constant = self.chunk.constant_at(constant_index as usize);
        let Value::String(string) = constant else {
            // TODO: refactor this
            eprintln!("Variable name must be a string.");
            return Err(VmError::RuntimeError);
        };
        Ok(string)
    }

    fn print_value(&self, value: Value) {
        match value {
            Value::Number(number) => println!("{number}"),
            Value::Bool(bool) => println!("{bool}"),
            Value::Nil => println!("nil"),
            Value::String(string) => println!("{string}"),
        }
    }

    /// Returns a reference to a value on the stack without popping it.
    /// distance 0 is top of stack
    /// distance 1 is one below the top of the stack
    fn peek(&self, distance: usize) -> Option<&Value> {
        if distance >= self.stack.len() {
            None
        } else {
            let idx = self.stack.len() - 1 - distance;
            self.stack.get(idx)
        }
    }

    fn runtime_error(&mut self, message: &str) -> Result<(), VmError> {
        eprintln!("Runtime error: {message}");
        self.stack.clear();
        Err(VmError::RuntimeError)
    }

    /// In the book, the BINARY_OP macro checks operand types with peek(distance) before popping the values.
    /// This function mirrors that macro: it validates peek(0) and peek(1) are numbers and only then pops and applies the op.
    fn binary_op_number<F>(&mut self, op: F) -> Result<(), VmError>
    where
        F: Fn(f64, f64) -> f64,
    {
        // Check types first using peek.
        if !matches!(self.peek(0), Some(Value::Number(_)))
            || !matches!(self.peek(1), Some(Value::Number(_)))
        {
            return self.runtime_error("Operands must be numbers.");
        }

        // Safe to pop because we just checked with peek.
        let b = match self.stack.pop() {
            Some(Value::Number(v)) => v,
            _ => return self.runtime_error("Operands must be numbers."),
        };
        let a = match self.stack.pop() {
            Some(Value::Number(v)) => v,
            _ => return self.runtime_error("Operands must be numbers."),
        };

        let result = Value::Number(op(a, b));
        self.stack.push(result);
        Ok(())
    }

    /// nil and false are "falsey", everything else is "truthy"
    fn is_falsey(value: &Value) -> bool {
        matches!(value, Value::Nil) || matches!(value, Value::Bool(false))
    }

    fn values_equal(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::String(a), Value::String(b)) => *a == *b,
            _ => false,
        }
    }

    /// Checks the two first stack operands and returns whether both of them are numbers
    fn stack_operands_are_numbers(&mut self) -> bool {
        let b = self.peek(0);
        let a = self.peek(1);
        matches!(a, Some(Value::Number(_))) && matches!(b, Some(Value::Number(_)))
    }

    /// Checks the two first stack operands and returns whether both of them are strings
    fn stack_operands_are_strings(&mut self) -> bool {
        let b = self.peek(0);
        let a = self.peek(1);
        matches!(a, Some(Value::String(_))) && matches!(b, Some(Value::String(_)))
    }

    fn concatenate_strings(&mut self) -> Result<(), VmError> {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        let (Value::String(a), Value::String(b)) = (a, b) else {
            return self.runtime_error("Operands must be strings.");
        };
        let concatenated_string = Value::String(format!("{a}{b}"));
        self.stack.push(concatenated_string);
        Ok(())
    }

    fn debug_trace(&self) {
        print!("          ");
        for value in &self.stack {
            // let Value::Number(value) = value;
            // print!("[ {value} ]");
            match value {
                Value::Number(v) => print!("[ {v} ]"),
                Value::Bool(v) => print!("[ {v} ]"),
                Value::Nil => print!("[ nil ]"),
                Value::String(string) => print!("[ \"{string}\" ]"),
            }
        }
        println!("");
        self.chunk.disassemble_instruction(self.instruction_pointer);
    }
}

#[derive(Debug)]
pub enum VmError {
    CompileError,
    RuntimeError,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        chunk::{Chunk, OpCode, Value},
        compiler::Compiler,
        parser::Parser,
        scanner::Scanner,
    };

    /// Creates and run a vm with the given chunk, checks its result is ok
    /// and returns the value at the top of the stack
    fn run_chunk_and_return_stack_top(chunk: Chunk) -> Option<Value> {
        let mut vm = Vm::new(chunk);
        let result = vm.run();
        assert!(result.is_ok());
        vm.peek(0).cloned()
    }

    /// Creates a chunk with the opcodes to represent the operation formed by the
    /// given operands and operator, and adds the Return opcode at the end.
    fn chunk_with_operands_and_operator(a: Value, b: Value, operator: OpCode) -> Chunk {
        let mut chunk = Chunk::new();
        // Add operand `a`
        let constant_index = chunk.add_constant(a);
        chunk.write(OpCode::Constant(constant_index), 0);

        // Add operand `b`
        let constant_index = chunk.add_constant(b);
        chunk.write(OpCode::Constant(constant_index), 0);

        // Add operator
        chunk.write(operator, 0);
        chunk.write(OpCode::Return, 0);
        chunk
    }

    #[test]
    fn test_number_addition() {
        // Test 2 + 3 equals 5
        let a = Value::Number(2.0);
        let b = Value::Number(3.0);
        let operator = OpCode::Add;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Number(5.0);
        assert_eq!(stack_top, expected_value);
    }

    #[test]
    fn test_number_subtraction() {
        // Test 2 - 3 equals -1
        let a = Value::Number(2.0);
        let b = Value::Number(3.0);
        let operator = OpCode::Subtract;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Number(-1.0);
        assert_eq!(stack_top, expected_value);
    }

    #[test]
    fn test_number_multiplication() {
        // Test 2 * 3 equals 6
        let a = Value::Number(2.0);
        let b = Value::Number(3.0);
        let operator = OpCode::Multiply;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Number(6.0);
        assert_eq!(stack_top, expected_value);
    }

    #[test]
    fn test_number_division() {
        // Test 8 / 4 equals 2
        let a = Value::Number(8.0);
        let b = Value::Number(4.0);
        let operator = OpCode::Divide;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Number(2.0);
        assert_eq!(stack_top, expected_value);
    }

    #[test]
    fn test_string_concatenation() {
        // Test "hello" + "world" equals "helloworld"
        let a = Value::String("hello".into());
        let b = Value::String("world".into());
        let operator = OpCode::Add;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::String("helloworld".into());
        assert_eq!(stack_top, expected_value);
    }

    #[test]
    fn test_number_greater() {
        // Test 2 > 1 returns true
        let a = Value::Number(2.0);
        let b = Value::Number(1.0);
        let operator = OpCode::Greater;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Bool(true);
        assert_eq!(stack_top, expected_value);

        // Test 1 > 2 returns false
        let a = Value::Number(1.0);
        let b = Value::Number(2.0);
        let operator = OpCode::Greater;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Bool(false);
        assert_eq!(stack_top, expected_value);
    }

    #[test]
    fn test_number_less() {
        // Test 1 < 2 returns true
        let a = Value::Number(1.0);
        let b = Value::Number(2.0);
        let operator = OpCode::Less;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Bool(true);
        assert_eq!(stack_top, expected_value);

        // Test 2 < 1 returns false
        let a = Value::Number(2.0);
        let b = Value::Number(1.0);
        let operator = OpCode::Less;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Bool(false);
        assert_eq!(stack_top, expected_value);
    }

    #[test]
    fn test_numbers_equal() {
        // Test 2 equals 2 returns true
        let a = Value::Number(2.0);
        let b = Value::Number(2.0);
        let operator = OpCode::Equal;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Bool(true);
        assert_eq!(stack_top, expected_value);

        // Test 2 equals 0 returns false
        let a = Value::Number(2.0);
        let b = Value::Number(0.0);
        let operator = OpCode::Equal;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Bool(false);
        assert_eq!(stack_top, expected_value);
    }

    #[test]
    fn test_strings_equal() {
        // Test "hello" equals "hello" returns true
        let a = Value::String("hello".into());
        let b = Value::String("hello".into());
        let operator = OpCode::Equal;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Bool(true);
        assert_eq!(stack_top, expected_value);

        // Test "hello" equals "world" returns false
        let a = Value::String("hello".into());
        let b = Value::String("world".into());
        let operator = OpCode::Equal;
        let chunk = chunk_with_operands_and_operator(a, b, operator);

        let stack_top = run_chunk_and_return_stack_top(chunk).unwrap();
        let expected_value = Value::Bool(false);
        assert_eq!(stack_top, expected_value);
    }

    fn compile_source(source: String) -> Chunk {
        let tokens = Scanner::new(source).scan();
        let declarations = Parser::new(tokens).parse();
        Compiler::new().compile(&declarations).unwrap()
    }

    #[test]
    fn test_variable_declaration() {
        let source = "var foo = 42;";
        let chunk = compile_source(source.into());
        let mut vm = Vm::new(chunk);
        let result = vm.run();
        assert!(result.is_ok());

        let expected_value = Some(&Value::Number(42.0));
        assert_eq!(vm.globals.get("foo"), expected_value);
    }

    #[test]
    fn test_variable_access() {
        let source = "
            var foo = 2;
            var result = 2 + foo;
        ";
        let chunk = compile_source(source.into());
        let mut vm = Vm::new(chunk);
        let result = vm.run();
        assert!(result.is_ok());

        let expected_value = Some(&Value::Number(4.0));
        assert_eq!(vm.globals.get("result"), expected_value);
    }

    #[test]
    fn test_variable_assignment() {
        let source = "
            var foo = 42;
            foo = 1;
        ";
        let chunk = compile_source(source.into());
        let mut vm = Vm::new(chunk);
        let result = vm.run();
        assert!(result.is_ok());

        let expected_value = Some(&Value::Number(1.0));
        assert_eq!(vm.globals.get("foo"), expected_value);
    }

    #[test]
    fn test_for_loop_increment_variable() {
        // Increment a global variable from a for loop
        // and check its value
        let source = "
            var global = 0;
            for (var i = 0; i < 5; i = i + 1) {
                global = global + 1;
            }
        ";
        let chunk = compile_source(source.into());
        let mut vm = Vm::new(chunk);
        let result = vm.run();
        assert!(result.is_ok());

        let expected_value = Some(&Value::Number(5.0));
        assert_eq!(vm.globals.get("global"), expected_value);
    }

    #[test]
    fn test_for_loop_decrementing_variable() {
        // Perform the same test but with different condition clause and increment clause
        let source = "
            var global = 5;
            for (var i = 5; i > 0; i = i - 1) {
                global = global - 1;
            }
        ";
        let chunk = compile_source(source.into());
        let mut vm = Vm::new(chunk);
        let result = vm.run();
        assert!(result.is_ok());

        let expected_value = Some(&Value::Number(0.0));
        assert_eq!(vm.globals.get("global"), expected_value);
    }
}
