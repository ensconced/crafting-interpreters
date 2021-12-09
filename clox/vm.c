#include "vm.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"

VM vm;

static Value clockNative(int argCount, Value* args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
  // stackTop points to where the next value to be pushed will go.
  // Since the stack array is declared directly inline in the VM struct, we
  // don't need to allocate it. We don't even need to clear the unused cells in
  // the array - we simply won't access them until after values have been stored
  // in them. The only initialization we need is to set stackTop to point to the
  // beginning of the array to indicate that the stack is empty.
  vm.stackTop = vm.stack;
  vm.frameCount = 0;
  vm.openUpvalues = NULL;
}

static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  // print stack trace
  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame* frame = &vm.frames[i];
    ObjFunction* function = frame->closure->function;
    size_t instruction = frame->ip - function->chunk.code - 1;
    fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }
  resetStack();
}

static void defineNative(const char* name, NativeFn function) {
  // The pushing and popping here looks weird but this is the kind of thing you
  // have to do when garbage collection is involved. Both copyString and
  // newNative dynamically allocate memory. That means they can potentially
  // trigger GC. So we need to ensure the collector knows we're not done with
  // the name and ObjFunction so it doesn't free them out from under us. Storing
  // them onto the value stack accomplishes that.
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function)));
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  pop();
  pop();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  vm.bytesAllocated = 0;
  // This starting threshold is arbitrary. The goal is to not trigger the first
  // gew GCs *too* quickly but also to not wait too long.
  vm.nextGC = 1024 * 1024;
  vm.grayCount = 0;
  vm.grayCapacity = 0;
  vm.grayStack = NULL;

  initTable(&vm.globals);
  initTable(&vm.strings);
  vm.initString = copyString("init", 4);

  defineNative("clock", clockNative);
}

void freeVM() {
  /*
    The process will free everything on exit, but it feels undignified to
    require the operating system to clean up our mess.
  */
  freeTable(&vm.globals);
  freeTable(&vm.strings);
  vm.initString = NULL;
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

static bool call(ObjClosure* closure, int argCount) {
  if (argCount != closure->function->arity) {
    runtimeError("Expected %d arguments but got %d.", closure->function->arity,
                 argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }

  // load the function into a new CallFrame at the top of the frame stack
  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;
  frame->slots = vm.stackTop - argCount - 1;
  return true;
}

static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      case OBJ_BOUND_METHOD: {
        ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
        vm.stackTop[-argCount - 1] = bound->receiver;
        return call(bound->method, argCount);
      }
      case OBJ_CLASS: {
        ObjClass* klass = AS_CLASS(callee);
        vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
        Value initializer;
        if (tableGet(&klass->methods, vm.initString, &initializer)) {
          return call(AS_CLOSURE(initializer), argCount);
        } else if (argCount != 0) {
          runtimeError("Expected 0 arguuments but got %d.", argCount);
          return false;
        }
        return true;
      }
      case OBJ_CLOSURE:
        return call(AS_CLOSURE(callee), argCount);
      case OBJ_NATIVE: {
        NativeFn native = AS_NATIVE(callee);
        Value result = native(argCount, vm.stackTop - argCount);
        vm.stackTop -= argCount + 1;
        push(result);
        return true;
      }
      default:
        break;  // Non-callable object type.
    }
  }
  runtimeError("Can only call functions and classes.");
  return false;
}

static bool bindMethod(ObjClass* klass, ObjString* name) {
  Value method;
  if (!tableGet(&klass->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }

  ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));

  pop();
  push(OBJ_VAL(bound));
  return true;
}

static ObjUpvalue* captureUpvalue(Value* local) {
  // We need to ensure that there is only ever a single ObjUpvalue for any given
  // local slot. If two closures capture the same variable, they must get the
  // same upvalue.
  ObjUpvalue* prevUpvalue = NULL;
  ObjUpvalue* upvalue = vm.openUpvalues;

  // iterate past all the slots above the one we're looking for
  while (upvalue != NULL && upvalue->location > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue->next;
  }

  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }

  ObjUpvalue* createdUpvalue = newUpvalue(local);
  createdUpvalue->next = upvalue;

  // insert the new upvalue into the list
  if (prevUpvalue == NULL) {
    vm.openUpvalues = createdUpvalue;
  } else {
    prevUpvalue->next = createdUpvalue;
  }

  return createdUpvalue;
}

static void closeUpvalues(Value* last) {
  // Close every open upvalue we can find that points to *last* or any slot
  // above it on the stack.
  while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
    ObjUpvalue* upvalue = vm.openUpvalues;
    // The way an upvalue gets closed is pretty cool. First, we copy the
    // variable's value into the *closed* field in the ObjUpvalue. That's where
    // closed-over variables live in the heap...
    upvalue->closed = *upvalue->location;
    // The OP_GET_UPVALUE and OP_SET_UPVALUE instructions need to look for the
    // variable therer after it's been moved. Those instructions dereference the
    // *location* pointer to get to the variable's value. When the variable
    // moves from the stack to the *closed* field, we simply update that
    // *location* to the address of the ObjUpvalue's own *closed* field.
    upvalue->location = &upvalue->closed;
    vm.openUpvalues = upvalue->next;
  }
}

static void defineMethod(ObjString* name) {
  // The method closure is no top of the stack, above the class it will be bound
  // to. We read those two stack slots and store the closure in the class's
  // method table. Then we pop the closure since we're done with it. Note that
  // we don't do any runtime type checking on the closure or class object. The
  // AS_CLASS call is safe because the compiler itself generated the code that
  // causes the class to be in that stack slot. The VM trusts its own compiler.
  Value method = peek(0);
  ObjClass* klass = AS_CLASS(peek(1));
  tableSet(&klass->methods, name, method);
  pop();
}

static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !(AS_BOOL(value)));
}

static void concatenate() {
  // peek instead of popping to prevent strings from getting GC'd
  ObjString* b = AS_STRING(peek(0));
  ObjString* a = AS_STRING(peek(1));

  int length = a->length + b->length;
  char* chars = ALLOCATE(char, length + 1);
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString* result = takeString(chars, length);
  pop();
  pop();
  push(OBJ_VAL(result));
}

static InterpretResult run() {
  // We could access the current frame by going through the CallFrame array
  // every time, but that's verbose. Most importantly, storing the frame in a
  // local variable encourages the C compiler to keep that pointer in a
  // register. That speeds up access to the frame's ip.
  CallFrame* frame = &vm.frames[vm.frameCount - 1];
#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
  (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() \
  (frame->closure->function->chunk.constants.values[READ_BYTE()])
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
        &frame->closure->function->chunk,
        // a little bit of pointer arithmetic to get the offset
        (int)(frame->ip - frame->closure->function->chunk.code));
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
        push(frame->slots[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE();
        // Note again, we don't pop the value from the stack. Assignment is an
        // expression, and every expression produces a value. The value of an
        // assignment expression is the assigned value itself, so the VM just
        // leaves the value on the stack.
        frame->slots[slot] = peek(0);
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
      case OP_GET_UPVALUE: {
        // The operand is the index into the current function's upvalue array.
        uint8_t slot = READ_BYTE();
        push(*frame->closure->upvalues[slot]->location);
        break;
      }
      case OP_SET_UPVALUE: {
        uint8_t slot = READ_BYTE();
        // The set instruction doesn't pop the value from the stack, because
        // assignment is an expression in Lox. So the result of the assignment -
        // the assigned value - needs to remain on the stack for the surrounding
        // expression.
        *frame->closure->upvalues[slot]->location = peek(0);
        break;
      }
      case OP_GET_PROPERTY: {
        if (!IS_INSTANCE(peek(0))) {
          runtimeError("Only instances have properties.");
          return INTERPRET_RUNTIME_ERROR;
        }
        ObjInstance* instance = AS_INSTANCE(peek(0));
        ObjString* name = READ_STRING();
        Value value;
        if (tableGet(&instance->fields, name, &value)) {
          pop();  // instance
          push(value);
          break;
        }

        if (!bindMethod(instance->klass, name)) {
          return INTERPRET_RUNTIME_ERROR;
        }

        break;
      }
      case OP_SET_PROPERTY: {
        if (!IS_INSTANCE(peek(1))) {
          runtimeError("Only instances have fields");
          return INTERPRET_RUNTIME_ERROR;
        }
        ObjInstance* instance = AS_INSTANCE(peek(1));
        tableSet(&instance->fields, READ_STRING(), peek(0));
        Value value = pop();
        pop();
        push(value);
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
      case OP_JUMP: {
        uint16_t offset = READ_SHORT();
        frame->ip += offset;
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();
        if (isFalsey(peek(0))) frame->ip += offset;
        break;
      }
      case OP_LOOP: {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        break;
      }
      case OP_CALL: {
        int argCount = READ_BYTE();
        if (!callValue(peek(argCount), argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_CLASS:
        push(OBJ_VAL(newClass(READ_STRING())));
        break;
      case OP_METHOD:
        defineMethod(READ_STRING());
        break;
      case OP_CLOSURE: {
        // Load the compiled function from the constant table, wrap it in a new
        // ObjClosure and push the result onto the stack.
        ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
        ObjClosure* closure = newClosure(function);
        push(OBJ_VAL(closure));
        // Waslk through all of the operands after OP_CLOSURE to see what kind
        // of upvalue each slot captures.
        for (int i = 0; i < closure->upvalueCount; i++) {
          uint8_t isLocal = READ_BYTE();
          uint8_t index = READ_BYTE();
          if (isLocal) {
            closure->upvalues[i] = captureUpvalue(frame->slots + index);
          } else {
            // Capture an upvalue from the surrounding function. An OP_CLOSURE
            // instruction is emitted at the end of a function declaration. At
            // the moment that we are executing that declaration, the *current*
            // function is the surrounding one. That means the current
            // function's closure is stored in the CallFrame at the top of the
            // callstack. So, to grab an upvalue from the enclosing function, we
            // can read it right from the frame local variable, which caches a
            // reference to that CallFrame.
            closure->upvalues[i] = frame->closure->upvalues[index];
          }
        }
        break;
      }
      case OP_CLOSE_UPVALUE:
        // The variable we're hoisting will be right on top of the stack.
        closeUpvalues(vm.stackTop - 1);
        // We have now moved the local from the stack to the heap, so we're free
        // to discard the stack slot.
        pop();
        break;
      case OP_RETURN: {
        // When a function returns a value, that value will be on top of the
        // stack. We're about the discard the called function's entire stack
        // window, so we pop that return value off and hang on to it.
        Value result = pop();
        // Close every remaining open upvalue owned by the returning function.
        closeUpvalues(frame->slots);
        // Then we discard the CallFrame for the returning function.
        vm.frameCount--;
        if (vm.frameCount == 0) {
          // We've finished executing the top-level code. The entire program is
          // done, so pop the main script function from the stack and exit the
          // interpreter.
          pop();
          return INTERPRET_OK;
        }
        // Discard all the slots the callee was using for its parameters and
        // local variables. That includes the same slots the caller used to pass
        // the arguments. This means the top of the stack ends up right at the
        // beginning of the returning function's stack window.
        vm.stackTop = frame->slots;
        // Push the return value back onto the stack at this new, lower
        // location.
        push(result);
        // Update the run function's cached pointer to the current frame. Just
        // like when we began a call, on the next iteration of the bytecode
        // dispatch loop, the VM will read ip from that frame, and execution
        // will jump back to the caller, right where it left off, immediately
        // after the OP_CALL instruction.
        frame = &vm.frames[vm.frameCount - 1];
        break;
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
  ObjFunction* function = compile(source);
  if (function == NULL) return INTERPRET_COMPILE_ERROR;
  // push function onto stack to prevent GC for freeing it...
  push(OBJ_VAL(function));
  ObjClosure* closure = newClosure(function);
  pop();
  push(OBJ_VAL(closure));
  call(closure, 0);
  return run();
}
