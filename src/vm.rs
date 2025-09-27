use crate::chunk::{Chunk, Object, OpCode, Value};

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
                    let value = self.stack.pop().unwrap();
                    println!("{value:?}");
                    return Ok(());
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

    fn runtime_error(&mut self, message: &str) -> Result<(), VmError> {
        eprintln!("Runtime error: {message}");
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
            (Value::Object(Object::String(a)), Value::Object(Object::String(b))) => *a == *b,
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
        matches!(a, Some(Value::Object(Object::String(_))))
            && matches!(b, Some(Value::Object(Object::String(_))))
    }

    fn concatenate_strings(&mut self) -> Result<(), VmError> {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        let (Value::Object(Object::String(a)), Value::Object(Object::String(b))) = (a, b) else {
            return self.runtime_error("Operands must be strings.");
        };
        let concatenated_string = Value::Object(Object::String(format!("{a}{b}")));
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
                Value::Object(Object::String(string)) => print!("[ \"{string}\" ]"),
            }
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
