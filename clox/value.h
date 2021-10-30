#ifndef clox_value_h
#define clox_value_h
#include "common.h"

// The cases here cover each kind of value that has built-in support in the VM.
typedef enum {
  VAL_BOOL,
  VAL_NIL,
  VAL_NUMBER,
} ValueType;

// tagged union
typedef struct {
  ValueType type;
  // Using "as" for the name of the union field means is reads nicely, almost
  // like a cast, when you pull the various values out.
  union {
    bool boolean;
    double number;
  } as;
} Value;

typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif
