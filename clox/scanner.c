#include "scanner.h"

#include <stdio.h>
#include <string.h>

#include "common.h"

typedef struct {
  const char* start;    // the beginning of the lexeme currently being scanned
  const char* current;  // the character currently being looked at
  int line;  // track what line the current lexeme is on for error reporting
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}
