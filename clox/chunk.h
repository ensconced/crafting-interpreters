#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
  OP_CONSTANT,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_RETURN,
  OP_NOT,
  OP_NEGATE,
  OP_PRINT,
} OpCode;

typedef struct {
  int count;
  int capacity;
  uint8_t* code;
  // We store a separate array of integers that parallels the byte code - each
  // number in the array is the line number in the source for the corresponding
  // byte in the bytecode. When a runtime error occurs, we look up the line
  // number at the same index as the current instruction's offset in the code
  // array.
  int* lines;
  ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif
