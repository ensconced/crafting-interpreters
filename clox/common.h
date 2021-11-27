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

// Optional stress-test mode for the GC. When this flag is defined, the GC runs
// as often as it possibly can. This is, obviously, horrendous for performance.
// But's it's great for flushing out memory management bugs that occur only when
// a GX is triggered at just the right moment.
#define DEBUG_STRESS_GC
#define DEBUG_LOG_GC

#define UINT8_COUNT (UINT8_MAX + 1)

#endif
