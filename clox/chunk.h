#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum { OP_CONSTANT, OP_RETURN, OP_NEGATE } OpCode;

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
