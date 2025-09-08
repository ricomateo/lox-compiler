use crate::chunk::{Chunk, OpCode, Value};
use crate::compiler::compile;

#[derive(Debug)]
pub struct Vm {
    chunk: Chunk,
    instruction_pointer: usize,
    stack: Vec<Value>,
}

impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            instruction_pointer: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), VmError> {
        compile(source);
        return Ok(());
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
                    let value = self.stack.pop().unwrap();
                    match value {
                        Value::Number(value) => {
                            let negated_value = Value::Number(-value);
                            self.stack.push(negated_value);
                        }
                    }
                }
                OpCode::Return => {
                    let value = self.stack.pop().unwrap();
                    println!("{value:?}");
                    return Ok(());
                }
                // TODO: extract repeated code and remove unwraps
                OpCode::Add => {
                    let Value::Number(b) = self.stack.pop().unwrap();
                    let Value::Number(a) = self.stack.pop().unwrap();
                    let result = Value::Number(a + b);
                    self.stack.push(result);
                }
                OpCode::Subtract => {
                    let Value::Number(b) = self.stack.pop().unwrap();
                    let Value::Number(a) = self.stack.pop().unwrap();
                    let result = Value::Number(a - b);
                    self.stack.push(result);
                }
                OpCode::Multiply => {
                    let Value::Number(b) = self.stack.pop().unwrap();
                    let Value::Number(a) = self.stack.pop().unwrap();
                    let result = Value::Number(a * b);
                    self.stack.push(result);
                }
                OpCode::Divide => {
                    let Value::Number(b) = self.stack.pop().unwrap();
                    let Value::Number(a) = self.stack.pop().unwrap();
                    let result = Value::Number(a / b);
                    self.stack.push(result);
                }
            }
        }
    }

    fn debug_trace(&self) {
        print!("          ");
        for value in &self.stack {
            let Value::Number(value) = value;
            print!("[ {value} ]");
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
