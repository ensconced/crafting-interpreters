#include "object.h"

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
  (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;
  object->isMarked = false;
  object->next = vm.objects;
  vm.objects = object;

#ifdef DEBUG_LOG_GC
  printf("%p allocate %zu for %d", (void*)object, size, type);
#endif

  return object;
}

ObjClass* newClass(ObjString* name) {
  ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
  klass->name = name;
  return klass;
}

ObjClosure* newClosure(ObjFunction* function) {
  // When we create an ObjClosure, we allocate an upvalue array of the proper
  // size, which we determined at compile time and stored in the ObjFunction.
  ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);

  // Initialize them all to NULL. This weird ceremony around memory is a careful
  // dance to please the GC deities. It ensures the memory manager never sees
  // uninitialized memory.
  for (int i = 0; i < function->upvalueCount; i++) {
    upvalues[i] = NULL;
  }

  ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
  closure->function = function;
  closure->upvalues = upvalues;
  closure->upvalueCount = function->upvalueCount;
  return closure;
}

ObjFunction* newFunction() {
  ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
  function->arity = 0;
  function->upvalueCount = 0;
  function->name = NULL;
  initChunk(&function->chunk);
  return function;
}

ObjInstance* newInstance(ObjClass* klass) {
  ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
  instance->klass = klass;
  initTable(&instance->fields);
  return instance;
}

ObjNative* newNative(NativeFn function) {
  ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
  native->function = function;
  return native;
}

static ObjString* allocateString(char* chars, int length, uint32_t hash) {
  ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;
  // push/pop to avoid string getting GC'd while growing the table
  push(OBJ_VAL(string));
  // String interning - here we're using the table more like a set than a
  // map/table. The keys are the strings and those are all we care about, so we
  // just use nil for the values.
  // This gets a string into the table assuming that it's unique, but we need to
  // actually check for duplication before we get here. We do that in the two
  // higher-level functions that call allocateString.
  tableSet(&vm.strings, string, NIL_VAL);
  pop();
  return string;
}

static uint32_t hashString(const char* key, int length) {
  // This algorithm is called FNV-1a and is one of the simplest decent hash
  // functions.
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    // xor the hash with the character...
    hash ^= (uint8_t)key[i];
    // then multiply by this prime
    hash *= 16777619;
  }
  return hash;
}

ObjString* takeString(char* chars, int length) {
  uint32_t hash = hashString(chars, length);
  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) {
    FREE_ARRAY(char, chars, length + 1);
    return interned;
  }

  return allocateString(chars, length, hash);
}

ObjString* copyString(const char* chars, int length) {
  uint32_t hash = hashString(chars, length);
  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) return interned;

  char* heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';
  return allocateString(heapChars, length, hash);
}

ObjUpvalue* newUpvalue(Value* slot) {
  ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
  upvalue->closed = NIL_VAL;
  upvalue->location = slot;
  upvalue->next = NULL;
  return upvalue;
}

static void printFunction(ObjFunction* function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }
  printf("<fn %s>", function->name->chars);
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
    case OBJ_CLASS:
      printf("%s", AS_CLASS(value)->name->chars);
      break;
    case OBJ_CLOSURE:
      // Closures are first-class objects, so you can print them. They display
      // exactly as ObjFunction does. From the user's perspective, the
      // difference between ObjFunction and ObjClosure is purely a hidden
      // implementation detail.
      printFunction(AS_CLOSURE(value)->function);
      break;
    case OBJ_FUNCTION:
      printFunction(AS_FUNCTION(value));
      break;
    case OBJ_INSTANCE:
      printf("%s instance", AS_INSTANCE(value)->klass->name->chars);
      break;
    case OBJ_NATIVE:
      printf("<native fn>");
      break;
    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;
    case OBJ_UPVALUE:
      // Printing isn't useful to end users. Upvalues are objects only so that
      // we can take advantage of the VM's memory management. They aren't
      // first-class values that a Lox user can directly access in a program. So
      // this code will never actually execute...but it keeps the compiler from
      // yelling at us about an unhandled switch case, so here we are.
      printf("upvalue");
      break;
  }
}
