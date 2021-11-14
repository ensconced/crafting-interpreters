#include "vm.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"

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

static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  size_t instruction = vm.ip - vm.chunk->code - 1;
  int line = vm.chunk->lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);
  resetStack();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  initTable(&vm.globals);
  initTable(&vm.strings);
}

void freeVM() {
  /*
    The process will free everything on exit, but it feels undignified to
    require the operating system to clean up our mess.
  */
  freeTable(&vm.globals);
  freeTable(&vm.strings);
  freeObjects();
}

void push(Value value) {
  // store value in the array element at the top of the stack
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

// return a value from the stack but don't pop it
static Value peek(int distance) { return vm.stackTop[-1 - distance]; }

static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !(AS_BOOL(value)));
}

static void concatenate() {
  ObjString* b = AS_STRING(pop());
  ObjString* a = AS_STRING(pop());

  int length = a->length + b->length;
  char* chars = ALLOCATE(char, length + 1);
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString* result = takeString(chars, length);
  push(OBJ_VAL(result));
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_SHORT() (vm.ip += 2, (uint16_t)((vm.ip[-2] << 8) | vm.ip[-1]))
#define READ_STRING() AS_STRING(READ_CONSTANT())

// We need this macro to expand to a series of statements. To be careful macro
// authors, we want to ensure those statements all end up in the same scope when
// the macro is expanded. Hence the weird do/while trick.
#define BINARY_OP(valueType, op)                      \
  do {                                                \
    if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
      runtimeError("Operands must be numbers");       \
      return INTERPRET_RUNTIME_ERROR;                 \
    }                                                 \
    double b = AS_NUMBER(pop());                      \
    double a = AS_NUMBER(pop());                      \
    push(valueType(a op b));                          \
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
      case OP_NIL:
        push(NIL_VAL);
        break;
      case OP_TRUE:
        push(BOOL_VAL(true));
        break;
      case OP_FALSE:
        push(BOOL_VAL(false));
        break;
      case OP_POP:
        pop();
        break;
      case OP_GET_LOCAL: {
        uint8_t slot = READ_BYTE();
        // It might seem redundant to push the local's value onto the stack
        // since it's already on the stack lower down somewhere. The problem is
        // that the other bytecode instructions only look for data at the *top*
        // of the stack - this is the core aspect that makes our bytecode
        // instruction set "stack-based".
        push(vm.stack[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE();
        // Note again, we don't pop the value from the stack. Assignment is an
        // expression, and every expression produces a value. The value of an
        // assignment expression is the assigned value itself, so the VM just
        // leaves the value on the stack.
        vm.stack[slot] = peek(0);
        break;
      }
      case OP_GET_GLOBAL: {
        ObjString* name = READ_STRING();
        Value value;
        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);
        break;
      }
      case OP_DEFINE_GLOBAL: {
        ObjString* name = READ_STRING();
        // Not that we don't pop the value until after we add it to the hash
        // table. That ensures the VM can still find the value if a garbage
        // collection is triggered right in the middle of adding it to the hash
        // table, which is a distinct possibility since the hash table requires
        // dynamic allocation when it resizes.
        tableSet(&vm.globals, name, peek(0));
        pop();
        break;
      }
      case OP_SET_GLOBAL: {
        ObjString* name = READ_STRING();
        // tableSet returns true if it's setting a new key,
        // false if overwriting an existing one.
        if (tableSet(&vm.globals, name, peek(0))) {
          // Lox doesn't do implicit variable declaration, so it's a runtime
          // error to try to assign to a variable that doesn't yet exist.
          // The call to tableSet will have stored the value in the global
          // variable table even though it wasn't previously defined - this fact
          // would be visible in a REPL session, since it keeps running even
          // after the runtime error is reported. So we also take care to delete
          // that zombie value from the table.
          tableDelete(&vm.globals, name);
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        // Assignemtn is an expression, so unlike OP_DEFINE_GLOBAL, we don't pop
        // the value off the stack - we want to leave it there in case the
        // assignment is nested inside some larger expression.
        break;
      }
      case OP_EQUAL: {
        Value b = pop();
        Value a = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }
      case OP_GREATER:
        BINARY_OP(BOOL_VAL, >);
        break;
      case OP_LESS:
        BINARY_OP(BOOL_VAL, <);
        break;
      case OP_ADD:
        if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
          concatenate();
        } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
          double b = AS_NUMBER(pop());
          double a = AS_NUMBER(pop());
          push(NUMBER_VAL(a + b));
        } else {
          runtimeError("Operands must be two numbers or two strings.");
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      case OP_SUBTRACT:
        BINARY_OP(NUMBER_VAL, -);
        break;
      case OP_MULTIPLY:
        BINARY_OP(NUMBER_VAL, *);
        break;
      case OP_DIVIDE:
        BINARY_OP(NUMBER_VAL, /);
        break;
      case OP_NOT:
        push(BOOL_VAL(isFalsey(pop())));
        break;
      case OP_NEGATE:
        if (!IS_NUMBER(peek(0))) {
          runtimeError("Operand must be a number");
          return INTERPRET_RUNTIME_ERROR;
        }
        push(NUMBER_VAL(-AS_NUMBER(pop())));
        break;
      case OP_PRINT: {
        printValue(pop());
        printf("\n");
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();
        if (isFalsey(peek(0))) vm.ip += offset;
        break;
      }
      case OP_RETURN: {
        // Exit interpreter
        return INTERPRET_OK;
      }
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  Chunk chunk;
  initChunk(&chunk);

  if (!compile(source, &chunk)) {
    freeChunk(&chunk);
    return INTERPRET_COMPILE_ERROR;
  }

  vm.chunk = &chunk;
  // The instruction pointer is an actual pointer into the middle of the
  // bytecode array. This is better than using an integer index because it's
  // faster to dereference a pointer than to look up an element in an array by
  // index.
  // We initialize ip by pointing it at the first byte of code in the chunk. We
  // haven't executed that instruction yet, so ip points to the instruction
  // *about to be executed*. This will be true during the entire time the VM is
  // running; the IP always points to the next instruction, not the one
  // currently being handled.
  vm.ip = vm.chunk->code;

  InterpretResult result = run();
  freeChunk(&chunk);
  return result;
}
