#[derive(Debug)]
pub enum OpCode {
    Return,
}

pub struct Chunk {
    // TODO: consider storing a vector of u8
    chunk: Vec<OpCode>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { chunk: Vec::new() }
    }

    pub fn write(&mut self, byte: OpCode) {
        self.chunk.push(byte);
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
        }
    }

    fn simple_instruction(name: &str, offset: usize) -> usize {
        println!("{name}");
        offset + 1
    }
}
