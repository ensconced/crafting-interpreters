#include "memory.h"

#include <stdlib.h>

#include "vm.h"

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  void* result = realloc(pointer, newSize);
  if (result == NULL) exit(1);
  return result;
}

static void freeObject(Obj* object) {
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

void freeObjects() {
  Obj* object = vm.objects;
  while (object != NULL) {
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }
}
