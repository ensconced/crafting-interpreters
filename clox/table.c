#include "table.h"

#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

/*
  This function is the real core of the hash table. It's responsible for taking
  a key and an array of buckets, and figuring out which bucket the entry belongs
  in. This function is also where linear probing and collision handling come
  into play. We use findEntry both to look up existing entries in the hash table
  and to decide where to insert new ones.
*/
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
  uint32_t index = key->hash % capacity;
  Entry* tombstone = NULL;
  for (;;) {
    Entry* entry = &entries[index];
    /*
      If the key for the Entry at this array index is NULL, then the bucket is
      empty. If we're using findEntry to look up something in the hash table,
      this means it isn't there. If we're using it to insert, it means we've
      found a place to add the new entry.

      If the key in the bucket is equal to the key we're looking for, then that
      key is already present in the table. If we're doing a lookup, that's good
      - we've found the key we seek. If we're an insert, this means we'll be
      replacing the value for that key instead of adding a new entry. NB it
      looks like we're using == to see if two strings are equal here...which of
      course doesn't work in C - there could be two copies of the same string
      at different places in memory. We'll solve this later on...
    */
    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {
        // Empty entry - the key isn't present. If we have passed a tombstone,
        // we return its bucket instead of the later empty one. If we're calling
        // findEntry in order to insert a node, that lets us treate the
        // tombstone bucket as empty and reuse it for the new entry.
        // Reusing tombstone slots automatically like this helps reduce the
        // number of tombstones wasting space in the bucket array.
        return tombstone != NULL ? tombstone : entry;
      } else {
        // We found a tombstone
        if (tombstone == NULL) tombstone = entry;
      }
    } else if (entry->key == key) {
      // We found the key
      return entry;
    }
    /*
      The bucket has an entry in it, but with a different key. This is a
      collision. So we start doing linear probing - that's what the for loop
      does.
    */
    index = (index + 1) % capacity;
  }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
  // This isn't just an optimization - it also ensures that we don't try to
  // access the bucket array when the array is NULL.
  if (table->count == 0) return false;

  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) return false;
  *value = entry->value;
  return true;
}

static void adjustCapacity(Table* table, int capacity) {
  Entry* entries = ALLOCATE(Entry, capacity);
  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }
  /*
    If you're dealing with a simple dynamic array, you can just use realloc and
    let the C standard library copy everything over. That doesn't work for a
    hash table though, because to choose the bucket for each entry, we take its
    hash key *modulo the array size*. So when the array size changes, entries
    would end up in different buckets.
    The simplest way to get every entry where it belongs is to rebuild the
    table from scratch by re-inserting every entry into the new empty array.
  */
  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    if (entry->key == NULL) continue;

    Entry* dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
  }
  FREE_ARRAY(Entry, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}

bool tableSet(Table* table, ObjString* key, Value value) {
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }

  Entry* entry = findEntry(table->entries, table->capacity, key);
  bool isNewKey = entry->key == NULL;
  if (isNewKey) table->count++;

  entry->key = key;
  entry->value = value;
  return isNewKey;
}

bool tableDelete(Table* table, ObjString* key) {
  if (table->count == 0) return false;

  // Find the entry
  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) return false;

  // Place a tombstone in the entry
  entry->key = NULL;
  entry->value = BOOL_VAL(true);
  return true;
}

void tableAddAll(Table* from, Table* to) {
  for (int i = 0; i < from->capacity; i++) {
    Entry* entry = &from->entries[i];
    if (entry->key != NULL) {
      tableSet(to, entry->key, entry->value);
    }
  }
}
