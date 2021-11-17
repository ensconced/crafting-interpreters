#include "compiler.h"

#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "scanner.h"
#include "string.h"

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

typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

typedef struct {
  Token name;
  // Zero is the global scope, 1 is the first top-level block, two is inside
  // that, etc.
  int depth;
} Local;

typedef enum {
  TYPE_FUNCTION,
  TYPE_SCRIPT,
} FunctionType;

typedef struct {
  ObjFunction* function;
  FunctionType type;

  // A simple, flat array of all locals that are in scope during each point in
  // the compilation process. They are ordered in the array in the order that
  // their declarations appear in the code. (This is a single-pass compiler, so
  // it's not like we have too many other options for how to order them in the
  // array). Since the instruction operand we'll use to encode a local is a
  // single byte, our VM has a hard limit on the number of locals that can be in
  // scope at once. That means we can also give the locals array a fixed size.
  Local locals[UINT8_COUNT];
  int localCount;
  // The number of blocks surrounding the current bit of code we're compiling.
  int scopeDepth;
} Compiler;

Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;

static Chunk* currentChunk() { return &current->function->chunk; }

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

static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);
  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX) error("Loop body too large.");

  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
  emitByte(instruction);
  // Use two bytes for the jump offset operand. A 16 bit offset lets us jump
  // over up to 65,535 bytes of code, which should be plenty for our needs.
  emitByte(0xff);
  emitByte(0xff);
  return currentChunk()->count - 2;
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

static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself
  int jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  // set most significant byte
  currentChunk()->code[offset] = (jump >> 8) & 0xff;
  // set least significant byte
  currentChunk()->code[offset + 1] = jump & 0xff;
}

static ObjFunction* endCompiler() {
  emitReturn();
  ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), function->name != NULL
                                         ? function->name->chars
                                         : "<script>");
  }
#endif
  return function;
}

static void beginScope() { current->scopeDepth++; }

static void endScope() {
  current->scopeDepth--;
  // walk backward through the local array, looking for any variables declared
  // at the scope depth we just left. We discard them simply by decrementing the
  // length of the array. There's a runtime component to this too; local
  // variables occupy slots on the stack. When a local variable goes out of
  // scope, that slot is no longer needed and should be freed. So, for each
  // variable that we discard, we also emit an OP_POP instruction to pop it from
  // the stack.
  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth > current->scopeDepth) {
    emitByte(OP_POP);
    current->localCount--;
  }
}

// forward declarations
static void expression();
static void statement();
static void declaration();

static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

// Takes the given token and adds its lexeme to the chunk's constant table as a
// string, returning the index of the constant in the constant table.
static uint8_t identifierConstant(Token* name) {
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
  if (a->length != b->length) return false;
  return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local* local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1) {
        error("Can't read local variable in its own initializer.");
      }
      return i;
    }
  }
  return -1;
}

static void addLocal(Token name) {
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;
  // -1 is a special sentinel value meaning "uninitialized"
  local->depth = -1;
}

static void declareVariable() {
  // This is the point where the compiler records the existence of the variable.
  // We only do this for locals, so if we're in the top-level global scope, we
  // just bail out. Because global variables are late-bound, the compiler
  // doesn't keep track of which declarations for them it has seen.
  if (current->scopeDepth == 0) return;

  // Add the local variable to the compiler's list of variables in the current
  // scope.
  Token* name = &parser.previous;

  // At the top level, Lox allows redeclaring a variable with the same name as a
  // previous declaration because that's useful for the REPL. But inside a local
  // scope, that's a pretty weird thing to do. It's likely to be a mistake, so
  // we make it an error.
  // It's okay to have two variables with the same name in *different* scopes,
  // even when the scopes overlap such that both are visible at the same name
  // (that's shadowing). It's only an error to have two variables with the same
  // name in the *same* local scope.
  for (int i = current->localCount - 1; i >= 0; i--) {
    Local* local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break;
    }

    if (identifiersEqual(name, &local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }
  addLocal(*name);
}

static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable();
  // Exit the function if we're in a local scope. At runtime,
  // locals aren't looked up by name. There's no need to stuff the variable's
  // name into the constant table, so if the declaration is inside a local
  // scope, we return a dummy table index instead.
  if (current->scopeDepth > 0) return 0;

  return identifierConstant(&parser.previous);
}

static void markInitialized() {
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    markInitialized();
    // Local variable - at this point the VM has already executed the code for
    // the variable's initializer (or the implicit nil if the user omitted the
    // initialize), and that value is sitting right on top of the stack as the
    // only remaining temporary. We also know that new locals are allocated at
    // the top of the stack...right where that value already is. Thus there's
    // nothing more to do. The temporary simply becomes the local variable.
    return;
  }
  emitBytes(OP_DEFINE_GLOBAL, global);
}

static void and_(bool canAssign) {
  int endJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  parsePrecedence(PREC_AND);
  patchJump(endJump);
}

static void binary(bool canAssign) {
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

static void literal(bool canAssign) {
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

static void grouping(bool canAssign) {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void initCompiler(Compiler* compiler) {
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  compiler->function = newFunction();
  current = compiler;

  Local* local = &current->locals[current->localCount++];
  // The compiler implicitly claims stack slot zero for the VM's own internal
  // use. We give it an empty name so that the user can't write an identifier
  // that refers to it.
  local->depth = 0;
  local->name.start = "";
  local->name.length = 0;
}

static void number(bool canAssign) {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);

  patchJump(elseJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

static void string(bool canAssign) {
  // strip quotation marks, create string object, and wrap in a Value
  emitConstant(OBJ_VAL(
      copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  // First, try to find a local variable with the same name.
  int arg = resolveLocal(current, &name);

  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    // It must be a global variable then...
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    // assignment
    expression();
    emitBytes(setOp, arg);
  } else {
    // variable access
    emitBytes(getOp, arg);
  }
}

static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
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
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, and_, PREC_AND},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_OR},
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

  // Since assignment is the lowest-precedence expression, the only time we
  // allow an assignment is when parsing an assignment expression or top-level
  // expression like in an expression statement.
  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }
}

static ParseRule* getRule(TokenType type) { return &rules[type]; }

static void expression() {
  // here we just parse the lowest precedence level, which subsumes all of the
  // higher-precedence expressions too
  parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
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

static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

  // initializer
  if (match(TOKEN_SEMICOLON)) {
    // no initializer
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    // Use expressionStatement instead of just expression here so that it will
    // look for a semicolon too and emit an OP_POP to discard the value - we
    // don't want the initializer to leave anything on the stack.
    expressionStatement();
  }

  int loopStart = currentChunk()->count;
  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

    // Jump out of the loop if the condition is false.
    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);  // condition
  }

  if (!match(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;
    expression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  statement();
  emitLoop(loopStart);

  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP);
  }

  endScope();
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  // The closing ) doesn't actually do anything useful. It's just there to
  // because unmatched parens look bad to the human eye.
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  // OP_JUMP_IF_FALSE needs an operand to know how much to offset the ip i.e.
  // how many bytes of code to skip. If the condition is falsey it will adjust
  // the ip by that amount. But when we're writing the OP_JUMP_IF_FALSE
  // instruction, we haven't compiled the "then" branch yet, so we don't know
  // how much bytecode it contains.
  // To work around this, we use a classic trick called "backpatching". We emit
  // the jump instruction first with a placeholder offset operand. We keep track
  // of where that half-finished instruction is. Next, we compile the "then"
  // body. Once that's done, we know how far to jump. So we go back and replace
  // that placeholder offset with the real one now that we can calculate it.
  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  // pop the value that's left on the stack from evaluating the condition
  // expression - this will run if the condition was truthy...otherwise there's
  // another one below which will run instead.
  emitByte(OP_POP);
  statement();
  int elseJump = emitJump(OP_JUMP);

  patchJump(thenJump);
  // pop the value that's left on the stack from evaluating the condition
  // expression. If the user didn't write an "else" clause then this little
  // instruction is kind of like an implicit else clause which just serves to
  // remove the condition value from the stack.
  emitByte(OP_POP);

  if (match(TOKEN_ELSE)) statement();
  patchJump(elseJump);
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

static void whileStatement() {
  int loopStart = currentChunk()->count;
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();
  emitLoop(loopStart);

  patchJump(exitJump);
  emitByte(OP_POP);
}

static void synchronize() {
  parser.panicMode = false;

  // Skip tokens indiscriminately until we reach something that looks like a
  // statement boundary. We recognize the boundary by looking for a preceding
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
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

ObjFunction* compile(const char* source) {
  initScanner(source);
  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);
  compilingChunk = chunk;
  parser.hadError = false;
  parser.panicMode = false;
  advance();
  while (!match(TOKEN_EOF)) {
    declaration();
  }
  ObjFunction* function = endCompiler();
  return !parser.hadError ? NULL : function;
}
