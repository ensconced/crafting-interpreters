#include "vm.h"

#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"

VM vm;

static void resetStack() {
  // stackTop points to where the next value to be pushed will go.
  // Since the stack array is declared directly inline in the VM struct, we
  // don't need to allocate it. We don't even need to clear the unused cells in
  // the array - we simply won't access them until after values have been stored
  // in them. The only initialization we need is to set stackTop to point to the
  // beginning of the array to indicate that the stack is empty.
  vm.stackTop = vm.stack;
}

void initVM() { resetStack(); }

void freeVM() {}

void push(Value value) {
  // store value in the array element at the top of the stack
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// We need this macro to expand to a series of statements. To be careful macro
// authors, we want to ensure those statements all end up in the same scope when
// the macro is expanded. Hence the weird do/while trick.
#define BINARY_OP(op) \
  do {                \
    double b = pop(); \
    double a = pop(); \
    push(a op b);     \
  } while (false)

  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    // show the current contents of the stack
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");

    disassembleInstruction(
        vm.chunk,
        // a little bit of pointer arithmetic to get the offset
        (int)(vm.ip - vm.chunk->code));
#endif

    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        push(constant);
        break;
      }
      case OP_ADD:
        BINARY_OP(+);
        break;
      case OP_SUBTRACT:
        BINARY_OP(-);
        break;
      case OP_MULTIPLY:
        BINARY_OP(*);
        break;
      case OP_DIVIDE:
        BINARY_OP(/);
        break;
      case OP_NEGATE:
        push(-pop());
        break;
      case OP_RETURN: {
        printValue(pop());
        printf("\n");
        return INTERPRET_OK;
      }
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  // The instruction pointer is an actual pointer into the middle of the
  // bytecode array. This is better than using an integer index because it's
  // faster to dereference a pointer than to look up an element in an array by
  // index.
  // We initialize ip by pointing it at the first byte of code in the chunk. We
  // haven't executed that instruction yet, so ip points to the instruction
  // *about to be executed*. This will be true during the entire time the VM is
  // running; the IP always points to the next instruction, not the one
  // currently being handled.
  compile(source);
  return INTERPRET_OK;
}
