#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64

// We define the value stack's size in terms of FRAMES_MAX to make sure we have
// plenty of stack slots even in very deep call trees. It's still possible to
// overflow the stack if enough function calls use enough temporaries in
// addition to locals. A robust implementation would guard against this.
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// A CallFrame represents a single ongoing function call. Each time a function
// is called, we create on of these structs. We could dynamically allocate them
// on the heap, but that's slow. Function calls are a core operation, so they
// need to be as fast as possible.
typedef struct {
  // the closure that is executing
  ObjClosure* closure;
  // instead of storing the return address in the callee's frame, the caller
  // stores its own ip. When we return from a function, the VM will jump to the
  // ip of the caller's CallFrame and resume from there.
  uint8_t* ip;
  // The slots field points into the VM's value stack at the first slot that
  // this function can use.
  Value* slots;
} CallFrame;

typedef struct {
  CallFrame frames[FRAMES_MAX];
  // The current height of the CallFrame stack - the number of ongoing function
  // calls. To keep things simple, the array's capacity is fixed - as in many
  // language implementations, there is a maximum call depth we can handle.
  int frameCount;

  Value stack[STACK_MAX];
  Value* stackTop;
  Table globals;
  Table strings;
  ObjUpvalue* openUpvalues;
  size_t bytesAllocated;
  size_t nextGC;
  Obj* objects;
  int grayCount;
  int grayCapacity;
  Obj** grayStack;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
void push(Value value);
Value pop();

InterpretResult interpret(const char* source);

#endif
