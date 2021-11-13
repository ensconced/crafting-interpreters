#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// When this flag is defined, we print out the bytecode.
#define DEBUG_PRINT_CODE

// When this flag is defined, the VM disassembles and prints each instruction
// right before executing it.
#define DEBUG_TRACE_EXECUTION

#define UINT8_COUNT (UINT8_MAX + 1)

#endif
