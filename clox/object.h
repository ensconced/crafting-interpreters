#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)AS_OBJ(value))
#define AS_CLASS(value) ((ObjClass*)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance*)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
  OBJ_BOUND_METHOD,
  OBJ_CLASS,
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_INSTANCE,
  OBJ_NATIVE,
  OBJ_STRING,
  OBJ_UPVALUE,
} ObjType;

struct Obj {
  ObjType type;
  bool isMarked;
  struct Obj* next;
};

typedef struct {
  // Functions are first class in Lox, so they need to be actual Lox objects.
  // Thus ObjFunction has the same Obj header that all object types share.
  Obj obj;
  // The number of parameters the function expects.
  int arity;
  int upvalueCount;
  Chunk chunk;
  ObjString* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
  Obj obj;
  NativeFn function;
} ObjNative;

struct ObjString {
  Obj obj;
  int length;
  char* chars;
  // Each objString stores the hash code for its string. Since strings are
  // immutable in Lox, we can calculate the hash code once up front and be
  // certain that it will never get invalidated. Caching it eagerly makes a kind
  // of sense: allocating the string and copying its characters over is already
  // an O(n) operation, so it's a good time to also do the O(n) calculation of
  // the string's hash.
  uint32_t hash;
};

typedef struct ObjUpvalue {
  Obj obj;
  // The location field points to the closed-over variable. Note that this is a
  // *pointer* to a Value, not a Value itself. It's a reference to a *variable*,
  // not a *value*. This is important because it means that when we assign to
  // the variable the upvalue captures, we're assigning to the actual variable,
  // not a copy.
  Value* location;
  Value closed;
  struct ObjUpvalue* next;
} ObjUpvalue;

typedef struct {
  Obj obj;
  ObjFunction* function;
  // Different closures may have different numbers of upvalues, so we need a
  // dynamic array. The upvalues themselves are dynamically allocated too, so we
  // end up with a double pointer - a pointer to a dynamically allocated array
  // of pointers to upvalues.
  ObjUpvalue** upvalues;
  // Storing the upvalue count in the closure is redundant because the
  // ObjFunction that the ObjClosure references also keeps that count. As uaual,
  // this weird code is to appease the GC. The collector may need to know an
  // ObjClosure's upvalue array size after the closure's corresponding
  // ObjFunction has already been freed.
  int upvalueCount;
} ObjClosure;

typedef struct {
  Obj obj;
  // The name isn't strictly needed for the user's program but it lets us show
  // the name at runtime for things like stack traces.
  ObjString* name;
  Table methods;
} ObjClass;

typedef struct {
  Obj obj;
  ObjClass* klass;
  Table fields;
} ObjInstance;

typedef struct {
  Obj obj;
  // The receiver's type is Value even though methods can only be called on
  // ObjInstances. Since the VM doesn't care what kind of receiver it has
  // anyway, using Value means we don't have to keep converting the pointer back
  // to a Value when it gets passed to more general functions.
  Value receiver;
  ObjClosure* method;
} ObjBoundMethod;

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method);

ObjClass* newClass(ObjString* name);
ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjInstance* newInstance(ObjClass* klass);
ObjNative* newNative(NativeFn function);

// copyString assumes it cannot take ownership of the characters you pass in.
// Instead, it conservatively creates a copy of the characters on the heap that
// the ObjString can own. That's the right thing for string literals where the
// passed-in characters are in the middle of the source string.
// But for concatenation, we've already dynamically allocated a character array
// on the heap. Making another copy of that would be redundant (and would mean
// concatenate has to remember to free its copy). Instead, this takeString
// claims ownership of the string you give it.
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
ObjUpvalue* newUpvalue(Value* slot);

void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
