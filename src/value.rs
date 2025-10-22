use crate::chunk::Chunk;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    String(String),
    Function(Function),
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Function {
    arity: usize,
    pub chunk: Chunk,
    pub name: String,
}
