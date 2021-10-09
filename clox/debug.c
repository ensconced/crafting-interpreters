#include "debug.h"

#include <stdio.h>

static int simpleInstruction(const char* name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

void disassembleChunk(Chunk* chunk, const char* name) {
  printf("== %s ==\n", name);

  for (int offset = 0; offset < chunk->count;) {
    // instructions can have different sizes so it makes sense to let disasseble
    // increment offset by the required amount
    offset = disassembleInstruction(chunk, offset);
  }
}

int disassembleInstruction(Chunk* chunk, int offset) {
  printf("%04d ", offset);

  uint8_t instruction = chunk->code[offset];
  switch (instruction) {
    case OP_RETURN:
      return simpleInstruction("OP_RETURN", offset);
    default:
      printf("unknown opcode %d\n", instruction);
      return offset + 1;
  }
}
