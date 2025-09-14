# Lox Compiler

Compilador de [lox](https://craftinginterpreters.com/the-lox-language.html) a modo de trabajo práctico para la materia Lenguajes y Compiladores I (FIUBA).

Previo a la ejecución, compila el código a secuencias de bytecode y luego las ejecuta en una máquina virtual (VM).

## Uso

### REPL

```bash
$ cargo run
```

### Ejecutar archivo línea por línea

```bash
$ cargo run examples/calc.lox
```

### Debugging

La VM se puede ejecutar en modo debugging seteando la variable de entorno `DEBUG_TRACE`:

```bash
export DEBUG_TRACE=true && cargo run
```

### Debugging con log + env_logger

```bash
RUST_LOG=debug cargo run
```
