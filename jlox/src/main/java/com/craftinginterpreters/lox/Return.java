package com.craftinginterpreters.lox;

class Return extends RuntimeException {
  final Object value;

  Return(Object value) {
    // disable some JVM machinery that we don't need - stack traces etc
    super(null, null, false, false);
    this.value = value;
  }
}
