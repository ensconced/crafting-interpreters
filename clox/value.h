#ifndef clox_value_h
#define clox_value_h
#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

// The cases here cover each kind of value that has built-in support in the VM.
typedef enum {
  VAL_BOOL,
  VAL_NIL,
  VAL_NUMBER,
  // when a value's type is VAL_OBJ, the payload is a pointer to heap memory.
  VAL_OBJ,
} ValueType;

// tagged union
typedef struct {
  ValueType type;
  // Using "as" for the name of the union field means is reads nicely, almost
  // like a cast, when you pull the various values out.
  union {
    bool boolean;
    double number;
    Obj* obj;
  } as;
} Value;

// macros for promoting native C values into clox values - here we're using
// designated initializer syntax for unions - see
// https://en.cppreference.com/w/c/language/struct_initialization
#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object) ((Value){VAL_OBJ, {.obj = (Obj*)object}})

// Macros for unpacking a clox value to get the native C value back out.
// Any time we call one of these AS_ macros, we need to guard it behind a call
// to one of the IS_ macros (see below) first.
// There is no AS_NIL macro because there is only one nil value, so the value
// doesn't carry any extra data.
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)

// and we have a few macros for checking a value's type
#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif
