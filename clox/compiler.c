#include "compiler.h"

#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
  Token current;
  Token previous;
  bool hadError;
  bool panicMode;
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,
  PREC_OR,
  PREC_AND,
  PREC_EQUALITY,
  PREC_COMPARISON,
  PREC_TERM,
  PREC_FACTOR,
  PREC_UNARY,
  PREC_CALL,
  PREC_PRIMARY,
} Precedence;

typedef void (*ParseFn)();

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

Parser parser;
Chunk* compilingChunk;

static Chunk* currentChunk() { return compilingChunk; }

static void errorAt(Token* token, const char* message) {
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }
  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char* message) { errorAt(&parser.previous, message); }

static void errorAtCurrent(const char* message) {
  errorAt(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;
  // Remember, clox's scanner doesn't report lexical errors. Instead, it creates
  // special error tokens and leaves it up to the parser to report them. We do
  // that here.
  // We keep looping, reading tokens and reporting the errors, until we hit a
  // non-error one or reach the end. That way, the reset of the parser sees only
  // valid tokens.
  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR) break;
    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

static bool check(TokenType type) { return parser.current.type == type; }

static bool match(TokenType type) {
  if (!check(type)) return false;
  advance();
  return true;
}

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void emitReturn() { emitByte(OP_RETURN); }

static uint8_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);
  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk.");
    return 0;
  }
  return (uint8_t)constant;
}

static void endCompiler() {
  emitReturn();
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), "code");
  }
#endif
}

// forward declarations
static void expression();
static void statement();
static void declaration();

static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

// Take the given token and add its lexeme to the chunk's constant table as a
// string, returning the index of the constant in the constant table.
static uint8_t identifierConstant(Token* name) {
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);
  return identiferConstant(&parser.previous);
}

static void defineVariable(uint8_t global) {
  emitBytes(OP_DEFINE_GLOBAL, global);
}

static void binary() {
  TokenType operatorType = parser.previous.type;
  ParseRule* rule = getRule(operatorType);
  parsePrecedence((Precedence)(rule->precedence + 1));

  switch (operatorType) {
    case TOKEN_BANG_EQUAL:
      emitBytes(OP_EQUAL, OP_NOT);
      break;
    case TOKEN_EQUAL_EQUAL:
      emitByte(OP_EQUAL);
      break;
    case TOKEN_GREATER:
      emitByte(OP_GREATER);
      break;
    case TOKEN_GREATER_EQUAL:
      emitBytes(OP_LESS, OP_NOT);
      break;
    case TOKEN_LESS:
      emitByte(OP_LESS);
      break;
    case TOKEN_LESS_EQUAL:
      emitBytes(OP_GREATER, OP_NOT);
      break;
    case TOKEN_PLUS:
      emitByte(OP_ADD);
      break;
    case TOKEN_MINUS:
      emitByte(OP_SUBTRACT);
      break;
    case TOKEN_STAR:
      emitByte(OP_MULTIPLY);
      break;
    case TOKEN_SLASH:
      emitByte(OP_DIVIDE);
      break;
    default:
      return;  // Unreachable
  }
}

static void literal() {
  switch (parser.previous.type) {
    case TOKEN_FALSE:
      emitByte(OP_FALSE);
      break;
    case TOKEN_NIL:
      emitByte(OP_NIL);
      break;
    case TOKEN_TRUE:
      emitByte(OP_TRUE);
      break;
    default:
      return;  // unreachable
  }
}

static void grouping() {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static void number() {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void string() {
  // strip quotation marks, create string object, and wrap in a Value
  emitConstant(OBJ_VAL(
      copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void unary() {
  TokenType operatorType = parser.previous.type;
  // Compile the operand. We use the unary operator's own precedence here to
  // permit nested unary operations.
  parsePrecedence(PREC_UNARY);

  // Emit the operator instruction.
  switch (operatorType) {
    case TOKEN_BANG:
      emitByte(OP_NOT);
      break;
    case TOKEN_MINUS:
      emitByte(OP_NEGATE);
      break;
    default:
      return;  // Unreachable
  }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {NULL, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, NULL, PREC_NONE},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, NULL, PREC_NONE},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression");
    return;
  }
  prefixRule();

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule();
  }
}

static ParseRule* getRule(TokenType type) { return &rules[type]; }

static void expression() {
  // here we just parse the lowest precedence level, which subsumes all of the
  // higher-precedence expressions too
  parsePrecedence(PREC_ASSIGNMENT);
}

static void varDeclaration() {
  uint8_t global = parseVariable("Expect variable name.");
  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    // If the user doesn't initialize the variable, implicitly initialize it to
    // nil.
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON, "Expect, ';' after variable declaration.");
  defineVariable(global);
}

// An "expression statement" is simply an expression followed by a semicolon.
static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  emitByte(OP_POP);
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

static void synchronize() {
  parser.panicMode = false;

  // Skip tokens indiscriminately until we reach something that looks like a
  // statment boundary. We recognize the boundary by looking for a preceding
  // token that can end a statement, like a semicolon.
  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON) return;
    switch (parser.current.type) {
      case TOKEN_CLASS:
      case TOKEN_FUN:
      case TOKEN_VAR:
      case TOKEN_FOR:
      case TOKEN_IF:
      case TOKEN_WHILE:
      case TOKEN_PRINT:
      case TOKEN_RETURN:
        return;

      default:;  // Do nothing
    }

    advance();
  }
}

static void declaration() {
  if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    statement();
  }
  if (parser.panicMode) synchronize();
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else {
    expressionStatement();
  }
}

bool compile(const char* source, Chunk* chunk) {
  initScanner(source);
  compilingChunk = chunk;
  parser.hadError = false;
  parser.panicMode = false;
  advance();
  while (!match(TOKEN_EOF)) {
    declaration();
  }
  endCompiler();
  return !parser.hadError;
}
