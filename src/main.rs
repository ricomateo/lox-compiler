use lox_compiler::{
    chunk::{Chunk, OpCode, Value},
    vm::Vm,
};

fn main() {
    let mut chunk = Chunk::new();
    let line = 123;
    let constant_index = chunk.add_constant(Value::Number(1.2));
    chunk.write(OpCode::Constant(constant_index), line);
    let constant_index = chunk.add_constant(Value::Number(3.4));
    chunk.write(OpCode::Constant(constant_index), line);
    chunk.write(OpCode::Add, line);
    let constant_index = chunk.add_constant(Value::Number(5.6));
    chunk.write(OpCode::Constant(constant_index), line);
    chunk.write(OpCode::Divide, line);

    chunk.write(OpCode::Negate, line);
    chunk.write(OpCode::Return, line);

    let mut vm = Vm::new(chunk);
    vm.interpret().unwrap();
    // chunk.disassemble("test chunk");
}
