#ifndef clox_value_h
#define clox_value_h
#include <string.h>

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING

#define SIGN_BIT ((uint64_t)0x8000000000000000)
// All of the exponent bits, plus the quiet NaN bit, plus one extra to dodge
// that Intel value.
#define QNAN ((uint64_t)0x7ffc000000000000)

#define TAG_NIL 1    // 01
#define TAG_FALSE 2  // 10
#define TAG_TRUE 3   // 11

typedef uint64_t Value;

#define IS_BOOL(value) (((value) | 1) == TRUE_VAL)
#define IS_NIL(value) ((value) == NIL_VAL)
// We know that every Value that is *not* a number will use a special quiet NaN
// representation. And we presume we have correctly avoided any of the
// meaningful NaN representations that may actually be produced by doing
// arithmetic on numbers.
#define IS_NUMBER(value) (((value)&QNAN) != QNAN)
#define IS_OBJ(value) (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value) ((value) == TRUE_VAL)
#define AS_OBJ(value) ((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))
#define AS_NUMBER(value) valueToNum(value)

#define BOOL_VAL(b) ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NIL_VAL ((Value)(uint64_t)(QNAN | TAG_NIL))

#define NUMBER_VAL(num) numToValue(num)
#define OBJ_VAL(obj) (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

static inline double valueToNum(Value value) {
  double num;
  memcpy(&num, &value, sizeof(Value));
  return num;
}

static inline Value numToValue(double num) {
  Value value;
  memcpy(&value, &num, sizeof(double));
  return value;
}

#else

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

#endif

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
