#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_NATIVE,
  OBJ_STRING,
} ObjType;

struct Obj {
  ObjType type;
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

typedef struct {
  Obj obj;
  ObjFunction* function;
} ObjClosure;

ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
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

void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
