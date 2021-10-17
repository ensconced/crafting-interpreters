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

static bool isAtEnd() { return *scanner.current == '\0'; }

static Token makeToken(TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner.start;
  token.length = (int)(scanner.current - scanner.start);
  token.line = scanner.line;
  return token;
}

static Token errorToken(const char* message) {
  Token token;
  token.type = TOKEN_ERROR;
  // For errors the "lexeme" points to the message string rather than into the
  // actual source code.
  token.start = message;
  token.length = (int)strlen(message);
  token.line = scanner.line;
  return token;
}

Token scanToken() {
  scanner.start = scanner.current;
  if (isAtEnd()) return makeToken(TOKEN_EOF);
  return errorToken("Unexpected character.");
}
