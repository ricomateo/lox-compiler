use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    Constant(usize),
    Nil,
    True,
    False,
    Pop,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    // TODO: Create instructions for NotEqual, GreaterEqual and LessEqual
    // We could create instructions for NotEqual, GreaterEqual and LessEqual but we can implement them using existing instructions as syntactic sugar
    // The VM would execute faster if we did
    Equal,
    Greater,
    Less,
    DefineGlobal(usize),
    GetLocal(usize),
    GetGlobal(usize),
    SetLocal(usize),
    SetGlobal(usize),
    Jump(usize),
    JumpIfFalse(usize),
    Loop(usize),
    Call(usize),
    Return,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Chunk {
    pub chunk: Vec<OpCode>,
    pub constants: Vec<Value>,
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
        println!("== {} ==", if name.is_empty() { "script" } else { name });
        let count = self.chunk.len();
        let mut offset = 0;
        while offset < count {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn instruction_at(&self, index: usize) -> OpCode {
        self.chunk[index].clone()
    }

    pub fn constant_at(&self, constant_index: usize) -> Value {
        self.constants[constant_index].clone()
    }

    pub fn line_at(&self, index: usize) -> usize {
        self.lines[index]
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
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
            OpCode::Negate => self.simple_instruction("OP_NEGATE", offset),
            OpCode::Add => self.simple_instruction("OP_ADD", offset),
            OpCode::Subtract => self.simple_instruction("OP_SUBTRACT", offset),
            OpCode::Multiply => self.simple_instruction("OP_MULTIPLY", offset),
            OpCode::Divide => self.simple_instruction("OP_DIVIDE", offset),
            OpCode::Nil => self.simple_instruction("OP_NIL", offset),
            OpCode::True => self.simple_instruction("OP_TRUE", offset),
            OpCode::False => self.simple_instruction("OP_FALSE", offset),
            OpCode::Not => self.simple_instruction("OP_NOT", offset),
            OpCode::Equal => self.simple_instruction("OP_EQUAL", offset),
            OpCode::Greater => self.simple_instruction("OP_GREATER", offset),
            OpCode::Less => self.simple_instruction("OP_LESS", offset),
            OpCode::Print => self.simple_instruction("OP_PRINT", offset),
            OpCode::Pop => self.simple_instruction("OP_POP", offset),
            OpCode::DefineGlobal(index) => {
                self.constant_instruction("OP_DEFINE_GLOBAL", offset, *index)
            }
            OpCode::GetGlobal(index) => self.constant_instruction("OP_GET_GLOBAL", offset, *index),
            OpCode::SetGlobal(index) => self.constant_instruction("OP_SET_GLOBAL", offset, *index),
            OpCode::GetLocal(slot) => self.byte_instruction("OP_GET_LOCAL", offset, *slot),
            OpCode::SetLocal(slot) => self.byte_instruction("OP_SET_LOCAL", offset, *slot),
            OpCode::Jump(jump_offset) => self.jump_instruction("OP_JUMP", *jump_offset, offset, 1),
            OpCode::JumpIfFalse(jump_offset) => {
                self.jump_instruction("OP_JUMP_IF_FALSE", *jump_offset, offset, 1)
            }
            OpCode::Call(arg_count) => self.byte_instruction("OP_CALL", offset, *arg_count),
            // TODO: Check jump instruction implementation
            OpCode::Loop(loop_offset) => self.jump_instruction("OP_LOOP", *loop_offset, offset, -1),
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{name}");
        offset + 1
    }

    fn byte_instruction(&self, name: &str, offset: usize, slot: usize) -> usize {
        println!("{:<16} {:>4}", name, slot);
        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize, constant_index: usize) -> usize {
        let value = &self.constants[constant_index];

        match value {
            Value::Number(v) => println!("{:<16} {:>4} '{}'", name, constant_index, v),
            Value::Bool(v) => println!("{:<16} {:>4} '{}'", name, constant_index, v),
            Value::Nil => println!("{:<16} {:>4} 'nil'", name, constant_index),
            Value::String(v) => {
                println!("{:<16} {:>4} '{}'", name, constant_index, v)
            }
            Value::Function(f) => {
                println!("{:<16} {:>4} '<fn {}>'", name, constant_index, f.name)
            }
            Value::NativeFunction(_) => {
                println!("'<native fn>'")
            }
        }

        // In the book, this function returns offset + 2 because the constant index is stored separately from Opcode::Constant
        // In our implementation, we combine them into a single element (Opcode::Constant(constant_index)), so we return offset + 1
        offset + 1
    }

    fn jump_instruction(&self, name: &str, jump_offset: usize, offset: usize, sign: i32) -> usize {
        let from = offset as isize + 1;
        let dist = jump_offset as isize;
        let target = if sign >= 0 { from + dist } else { from - dist };
        println!("{:<16} {:>4} -> {}", name, offset, target);
        offset + 1
    }
}
