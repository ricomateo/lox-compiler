use crate::chunk::{Chunk, OpCode, Value};

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

    pub fn interpret(&mut self) -> Result<(), VmError> {
        self.run()
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        loop {
            if let Ok(_) = std::env::var("DEBUG") {
                self.debug_trace();
            };
            let instruction = self.chunk.instruction_at(self.instruction_pointer);
            self.instruction_pointer += 1;
            match instruction {
                OpCode::Constant(constant_index) => {
                    let constant = self.chunk.constant_at(constant_index);
                    self.stack.push(constant);
                }
                OpCode::Return => {
                    self.stack.pop().unwrap();
                    return Ok(());
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
