use crate::chunk::{Chunk, OpCode, Value};

#[derive(Debug)]
pub struct Vm {
    chunk: Chunk,
    instruction_pointer: usize,
}

impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            instruction_pointer: 0,
        }
    }

    pub fn interpret(&mut self) -> Result<(), VmError> {
        self.run()
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        loop {
            let instruction = self.chunk.instruction_at(self.instruction_pointer);
            self.instruction_pointer += 1;
            match instruction {
                OpCode::Constant(constant_index) => {
                    let constant = self.chunk.constant_at(constant_index);
                    let Value::Number(value) = constant;
                    println!("'{value}'");
                }
                OpCode::Return => return Ok(()),
            }
        }
    }
}

#[derive(Debug)]
pub enum VmError {
    CompileError,
    RuntimeError,
}
