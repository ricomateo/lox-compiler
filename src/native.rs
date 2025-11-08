use crate::value::Value;
extern crate libc;

// We need to call C code because there is no clock function in Rust
unsafe extern "C" {
    fn clock() -> ::libc::clock_t;
}

pub fn clock_native(_arg_count: usize, _args: Vec<Value>) -> Value {
    let clock = unsafe { clock() } as f64;
    Value::Number(clock)
}
