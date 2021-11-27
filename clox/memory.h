#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "object.h"

#define ALLOCATE(type, count) (type*)reallocate(NULL, 0, sizeof(type) * (count))

// Why not just call free? If all allocation and freeing goes through
// reallocate, it's easy to keep a running count of the number of bytes of
// allocated memory, to be tracked by the VM.
#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)

#define GROW_ARRAY(type, pointer, oldCount, newCount)    \
  ((type*)reallocate(pointer, sizeof(type) * (oldCount), \
                     sizeof(type) * (newCount)))

#define FREE_ARRAY(type, pointer, oldCount) \
  reallocate(pointer, sizeof(type) * (oldCount), 0)

void* reallocate(void* pointer, size_t oldSize, size_t newSize);
void collectGarbage();
void freeObjects();

#endif
