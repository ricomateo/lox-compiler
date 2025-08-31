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
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            chunk: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write(&mut self, byte: OpCode, line: usize) {
        self.chunk.push(byte);
        self.lines.push(line);
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
        let same_line_as_previous_opcode =
            offset > 0 && self.lines[offset] == self.lines[offset - 1];
        if same_line_as_previous_opcode {
            print!("   | ");
        } else {
            print!(" {} ", self.lines[offset]);
        }

        match instruction {
            OpCode::Return => self.simple_instruction("OP_RETURN", offset),
            OpCode::Constant(constant_index) => {
                self.constant_instruction("OP_CONSTANT", offset, *constant_index)
            }
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{name}");
        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize, constant_index: usize) -> usize {
        let Value::Number(value) = &self.constants[constant_index];
        println!("{:<16} {:>4} '{}'", name, constant_index, value);
        // In the book, this function returns offset + 2 because the constant index is stored separately from Opcode::Constant
        // In our implementation, we combine them into a single element (Opcode::Constant(constant_index)), so we return offset + 1
        offset + 1
    }
}
