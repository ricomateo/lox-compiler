#[derive(Debug)]
pub enum Value {
    Number(f64),
}

#[derive(Debug)]
pub enum OpCode {
    Constant(usize),
    Return,
}

pub struct Chunk {
    // TODO: consider storing a vector of u8
    chunk: Vec<OpCode>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            chunk: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn write(&mut self, byte: OpCode) {
        self.chunk.push(byte);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");
        let count = self.chunk.len();
        let mut offset = 0;
        while offset < count {
            offset = self.disassemble_instruction(offset);
        }
    }

    fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        let instruction = &self.chunk[offset];
        match instruction {
            OpCode::Return => Chunk::simple_instruction("OP_RETURN", offset),
            OpCode::Constant(index) => 0 // TODO: implement constant instruction
        }
    }

    fn simple_instruction(name: &str, offset: usize) -> usize {
        println!("{name}");
        offset + 1
    }
}
