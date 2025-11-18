# Lox Compiler

Compilador de [lox](https://craftinginterpreters.com/the-lox-language.html) a modo de trabajo práctico para la materia Lenguajes y Compiladores I (FIUBA).

Previo a la ejecución, compila el código a secuencias de bytecode y luego las ejecuta en una máquina virtual (VM).

Se puede acceder a la presentacion del TP: [Slides](https://docs.google.com/presentation/d/1FyM-nl8sC7lRgMgru87uVzur-gM--kwyu6buONDd5TI/edit?slide=id.g3a2a591218e_0_22#slide=id.g3a2a591218e_0_22).

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
