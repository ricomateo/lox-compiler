use std::collections::HashMap;

use crate::{
    chunk::{Chunk, OpCode},
    value::{Function, Value},
};

const STACK_MAX: usize = 256;
const FRAMES_MAX: usize = 64;

#[derive(Debug)]
pub struct Vm {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    pub globals: HashMap<String, Value>,
}

#[derive(Debug)]
pub struct CallFrame {
    pub function: Function,
    instruction_pointer: usize,
    // Points to the VM's value stack at the first slot this function can use
    // TODO: consider using a reference to the stack (like &vm.stack[slot_index..])
    slot_index: usize,
}

impl Vm {
    pub fn new(function: Function) -> Self {
        let frame = CallFrame {
            function,
            instruction_pointer: 0,
            slot_index: 0,
        };

        let mut stack = Vec::with_capacity(STACK_MAX);
        // TODO: Check this
        // Initialize the stack with Nil value to avoid index out of bounds in tests
        stack.push(Value::Nil);

        Self {
            stack,
            frames: vec![frame],
            globals: HashMap::new(),
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        // Fix: borrow the frame inside the loop in short scopes right where it’s used.

        // let frame_count = self.frames.len();
        // let frame = self.frames.get_mut(frame_count - 1).unwrap();

        loop {
            if std::env::var("DEBUG_TRACE").is_ok() {
                self.debug_trace();
            };

            let instruction = {
                let frame = self.frames.last().unwrap();
                frame
                    .function
                    .chunk
                    .instruction_at(frame.instruction_pointer)
            };

            // let instruction = frame
            //     .function
            //     .chunk
            //     .instruction_at(frame.instruction_pointer);

            // frame.instruction_pointer += 1;

            {
                let frame = self.frames.last_mut().unwrap();
                frame.instruction_pointer += 1;
            }

            match instruction {
                OpCode::Constant(constant_index) => {
                    // let constant = frame.function.chunk.constant_at(constant_index);
                    let constant = {
                        let frame = self.frames.last().unwrap();
                        frame.function.chunk.constant_at(constant_index)
                    };
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
                    // Pop return value
                    let result = self.stack.pop().unwrap();
                    // Pop the returning function callframe
                    let frame = self.frames.pop().unwrap();
                    // If there are no remaining callframes, we finished execution
                    if self.frames.is_empty() {
                        self.stack.pop();
                        return Ok(());
                    }
                    let new_stack_top = frame.slot_index - 1;
                    // Discard all of the slots the callee was using for its parameters and local variables
                    self.stack = self.stack[..new_stack_top].into();
                    self.stack.push(result);
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
                    let frame = self.frames.last().unwrap();
                    let slot = frame.slot_index + slot;
                    self.stack.push(self.stack[slot].clone());
                }
                OpCode::SetLocal(slot) => {
                    // Takes the assigned value from the top of the stack
                    // and stores it in the stack slot corresponding to the local variable
                    let frame = self.frames.last().unwrap();
                    let slot = frame.slot_index + slot;
                    self.stack[slot] = self.peek(0).unwrap().clone();
                }
                OpCode::Jump(offset) => {
                    // frame.instruction_pointer += offset;
                    let frame = self.frames.last_mut().unwrap();
                    frame.instruction_pointer += offset;
                }
                OpCode::JumpIfFalse(offset) => {
                    let condition = self.peek(0).unwrap();
                    if Self::is_falsey(&condition) {
                        // frame.instruction_pointer += offset;
                        let frame = self.frames.last_mut().unwrap();
                        frame.instruction_pointer += offset;
                    }
                }
                OpCode::Loop(offset) => {
                    // frame.instruction_pointer -= offset;
                    let frame = self.frames.last_mut().unwrap();
                    frame.instruction_pointer -= offset;
                }
                OpCode::Call(arg_count) => {
                    // Add a new frame
                    let callee = self.peek(arg_count).unwrap().clone();
                    self.call_value(callee, arg_count)?;
                }
            }
        }
    }

    fn current_chunk(&mut self) -> Chunk {
        self.frames.last().unwrap().function.chunk.clone()
    }

    fn get_variable_name(&mut self, constant_index: usize) -> Result<String, VmError> {
        let constant = self.current_chunk().constant_at(constant_index as usize);
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
            Value::Function(function) => {
                if &function.name == "" {
                    println!("<script>");
                    return;
                }
                println!("<fn {}>", function.name);
            }
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

    fn call(&mut self, function: Function, arg_count: usize) -> Result<(), VmError> {
        if arg_count != function.arity {
            self.runtime_error(&format!(
                "Expected {} arguments but got {}.",
                function.arity, arg_count,
            ))?;
        }
        if self.frames.len() == FRAMES_MAX {
            self.runtime_error("Stack overflow.")?;
        }

        let stack_top = self.stack.len();
        let slot_index = stack_top - arg_count;
        let frame = CallFrame {
            function,
            instruction_pointer: 0,
            slot_index,
        };
        self.frames.push(frame);
        Ok(())
    }

    fn call_value(&mut self, callee: Value, arg_count: usize) -> Result<(), VmError> {
        match callee {
            Value::Function(function) => self.call(function, arg_count),
            _ => self.runtime_error("Can only call functions and classes."),
        }
    }

    fn runtime_error(&mut self, message: &str) -> Result<(), VmError> {
        eprintln!("Runtime error: {message}");

        // Print the stack trace from top to bottom
        for frame in self.frames.iter().rev() {
            let function = &frame.function;
            let line = function.chunk.line_at(frame.instruction_pointer);
            if function.name == "" {
                eprint!("[line {line}] in script");
            } else {
                eprint!("[line {line}] in {}()", function.name);
            }
        }

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
                Value::Function(function) => print!("[ <fn {}> ]", function.name),
            }
        }
        println!("");
        let frame = self.frames.last().unwrap();
        frame
            .function
            .chunk
            .disassemble_instruction(frame.instruction_pointer);
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
        chunk::{Chunk, OpCode},
        compiler::Compiler,
        parser::Parser,
        scanner::Scanner,
    };

    /// Creates and run a vm with the given chunk, checks its result is ok
    /// and returns the value at the top of the stack
    fn run_chunk_and_return_stack_top(function: Function) -> Option<Value> {
        let mut vm = Vm::new(function);
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

    fn function_from_chunk(chunk: Chunk) -> Function {
        Function {
            arity: 0,
            chunk,
            name: "".into(),
        }
    }

    #[test]
    fn test_number_addition() {
        // Test 2 + 3 equals 5
        let a = Value::Number(2.0);
        let b = Value::Number(3.0);
        let operator = OpCode::Add;
        let chunk = chunk_with_operands_and_operator(a, b, operator);
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
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
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
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
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
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
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
        let expected_value = Value::Number(2.0);
        assert_eq!(stack_top, expected_value);
    }

    fn compile_source(source: String) -> Function {
        let tokens = Scanner::new(source).scan();
        let declarations = Parser::new(tokens).parse();
        Compiler::new().compile(&declarations).unwrap()
    }

    #[test]
    fn test_string_concatenation() {
        // Test "hello" + "world" equals "helloworld"
        let a = Value::String("hello".into());
        let b = Value::String("world".into());
        let operator = OpCode::Add;
        let chunk = chunk_with_operands_and_operator(a, b, operator);
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
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
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
        let expected_value = Value::Bool(true);
        assert_eq!(stack_top, expected_value);

        // Test 1 > 2 returns false
        let a = Value::Number(1.0);
        let b = Value::Number(2.0);
        let operator = OpCode::Greater;
        let chunk = chunk_with_operands_and_operator(a, b, operator);
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
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
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
        let expected_value = Value::Bool(true);
        assert_eq!(stack_top, expected_value);

        // Test 2 < 1 returns false
        let a = Value::Number(2.0);
        let b = Value::Number(1.0);
        let operator = OpCode::Less;
        let chunk = chunk_with_operands_and_operator(a, b, operator);
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
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
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
        let expected_value = Value::Bool(true);
        assert_eq!(stack_top, expected_value);

        // Test 2 equals 0 returns false
        let a = Value::Number(2.0);
        let b = Value::Number(0.0);
        let operator = OpCode::Equal;
        let chunk = chunk_with_operands_and_operator(a, b, operator);
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
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
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
        let expected_value = Value::Bool(true);
        assert_eq!(stack_top, expected_value);

        // Test "hello" equals "world" returns false
        let a = Value::String("hello".into());
        let b = Value::String("world".into());
        let operator = OpCode::Equal;
        let chunk = chunk_with_operands_and_operator(a, b, operator);
        let function = function_from_chunk(chunk);

        let stack_top = run_chunk_and_return_stack_top(function).unwrap();
        let expected_value = Value::Bool(false);
        assert_eq!(stack_top, expected_value);
    }

    #[test]
    fn test_variable_declaration() {
        let source = "var foo = 42;";
        let function = compile_source(source.into());
        let mut vm = Vm::new(function);
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
        let function = compile_source(source.into());
        let mut vm = Vm::new(function);
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
        let function = compile_source(source.into());
        let mut vm = Vm::new(function);
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
        let function = compile_source(source.into());
        let mut vm = Vm::new(function);
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
        let function = compile_source(source.into());
        let mut vm = Vm::new(function);
        let result = vm.run();
        assert!(result.is_ok());

        let expected_value = Some(&Value::Number(0.0));
        assert_eq!(vm.globals.get("global"), expected_value);
    }
}
