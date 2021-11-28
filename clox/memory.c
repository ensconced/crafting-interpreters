#include "memory.h"

#include <stdlib.h>

#include "compiler.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>

#include "debug.h"
#endif

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
    collectGarbage();
#endif
  }

  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  void* result = realloc(pointer, newSize);
  if (result == NULL) exit(1);
  return result;
}

void markObject(Obj* object) {
  if (object == NULL) return;
  // prevent infinite recursion for cycles
  if (object->isMarked) return;
#ifdef DEBUG_LOG_GC
  printf("%p mark ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif
  object->isMarked = true;

  if (vm.grayCapacity < vm.grayCount + 1) {
    // This stack works mostly like other dynamic arrays we use in Lox, except
    // that is uses the system realloc function and not our own *reallocate*
    // wrapper. The memory for the gray stack itself is not managed by the
    // garbage collector; we don't want growing the gray stack during a GC to
    // cause the GC to recursively start a new GC.
    vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
    vm.grayStack = (Obj**)realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);

    // If we can't create or grow the gray stack, then we can't finish the
    // garbage collection. This is bad news for the VM, but fortunately rare
    // since the gray stack tends to be pretty small. It would be nice to do
    // something more graceful. For example, we could allocate a "rainy day
    // fund" block of memory when we start the VM. If the gray stack allocation
    // fails, we free the rainy day block and try again. That may give us enough
    // wiggle room on the heap to create the gray stack, finish the GC, and free
    // up more memory.
    if (vm.grayStack == NULL) exit(1);
  }
  vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value) {
  // Some Lox values - numbers, Booleans, and nil - are stored directly inline
  // in Value and require no heap allocation. The GC doesn't need to worry about
  // them at all.
  if (IS_OBJ(value)) markObject(AS_OBJ(value));
}

static void markArray(ValueArray* array) {
  for (int i = 0; i < array->count; i++) {
    markValue(array->values[i]);
  }
}

static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p free type %d\n", (void*)object, object->type);
#endif

  switch (object->type) {
    case OBJ_CLOSURE: {
      ObjClosure* closure = (ObjClosure*)object;
      // ObjClosure does not own the ObjUpvalue objects themselves, but it does
      // own *the array* containing pointers to those upvalues.
      FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
      // We free only the closure, not the contained function. That's because
      // the closure doesn't *own* the function. There may be multiple closures
      // that all reference the same function, and none of them claims any
      // special privilege over it. We can't free the function until all objects
      // referencing it are gone - including even the surrounding function whose
      // constant table contains it. That's a job for the GC.
      FREE(ObjClosure, object);
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      freeChunk(&function->chunk);
      FREE(ObjFunction, object);
      // We don't need to explicity free the function's name because it's an
      // ObjString. That means we can let the garbage collector manage its
      // lifetime for us.
      break;
    }
    case OBJ_NATIVE: {
      FREE(ObjNative, object);
      break;
    }
    case OBJ_STRING: {
      ObjString* string = (ObjString*)object;
      // We aren't only freeing the Obj itself. Since some object types also
      // allocate other memory that they own, we also need a little
      // type-specific code to handle each object type's special needs. Here,
      // that means we free the character array and then free the ObjString.
      FREE_ARRAY(char, string->chars, string->length + 1);
      FREE(ObjString, object);
      break;
    }
    case OBJ_UPVALUE:
      // Multiple closures can close over the same variable, so ObjUpvalue
      // does not own the variable it references. Thus, the only thing to free
      // is the ObjUpvalue itself.
      FREE(ObjUpvalue, object);
      break;
  }
}

static void markRoots() {
  for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
    markValue(*slot);
  }
  for (int i = 0; i < vm.frameCount; i++) {
    markObject((Obj*)vm.frames[i].closure);
  }

  for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL;
       upvalue = upvalue->next) {
    markObject((Obj*)upvalue);
  };

  markTable(&vm.globals);
  // A collection can begin during any allocation. Those don't just happen while
  // the user's program is running - the compiler itself periodically grabs
  // memory from the heap for literals and the constant table. If the GC runs
  // while we're in the middle of compiling, then any values the compiler
  // directly accesses need to be treated as roots too.
  markCompilerRoots();
}

static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p blacken ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif
  // We don't set any state in the traversed object itself - there is no direct
  // encoding of "black" in the object's state. A black object is any object
  // whose isMarked field is set and that is no longer in the gray stack.
  switch (object->type) {
    // strings and native function objects have no outgoing references so there
    // is nothing to traverse
    case OBJ_NATIVE:
    case OBJ_STRING:
      break;
    case OBJ_UPVALUE:
      // When an upvalue is closed, it contains a reference to the closed-over
      // value. Since the value is no longer on the stack, we need to make sure
      // we trace the reference to it from the upvalue.
      markValue(((ObjUpvalue*)object)->closed);
      break;
    case OBJ_CLOSURE: {
      // Each closure has a reference to the bare function it wraps, as well as
      // an array of pointers to the upvalues it captures.
      ObjClosure* closure = (ObjClosure*)object;
      markObject((Obj*)closure->function);
      for (int i = 0; i < closure->upvalueCount; i++) {
        markObject((Obj*)closure->upvalues[i]);
      }
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      // Each function has a reference to an ObjString containing the function's
      // name, and a constant table packed full of references to other objects.
      markObject((Obj*)function->name);
      markArray(&function->chunk.constants);
      break;
    }
  }
}

static void traceReferences() {
  while (vm.grayCount > 0) {
    Obj* object = vm.grayStack[--vm.grayCount];
    blackenObject(object);
  }
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
  printf("-- gc begin\n");
#endif

  markRoots();
  traceReferences();

#ifdef DEBUG_LOG_GC
  printf("-- gc end\n");
#endif
}

void freeObjects() {
  Obj* object = vm.objects;
  while (object != NULL) {
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }
  free(vm.grayStack);
}
