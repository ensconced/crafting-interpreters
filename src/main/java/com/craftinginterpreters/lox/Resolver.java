package com.craftinginterpreters.lox;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

class Resolver implements Expr.Visitor<Void>, Stmt.Visitor<Void> {
  private final Interpreter interpreter;
  private final Stack<Map<String, Boolean>> scopes = new Stack<>();

  Resolver(Interpreter interpreter) {
    this.interpreter = interpreter;
  }

  @Override
  public Void visitBlockStmt(Stmt.Block stmt) {
    beginScope();
    resolve(stmt.statements);
    endScope();
    return null;
  }

  @Override
  public Void visitVarStmt(Stmt.Var stmt) {
    // binding is split into two steps: declaring and defining
    declare(stmt.name);
    if (stmt.initializer != null) {
      // Resolve the initializer expression in the same scope where the new variable
      // now exists but is unavilable.
      resolve(stmt.initializer);
    }
    // Once the initializer expression is done, the variable is ready fro prime
    // time.
    define(stmt.name);
    return null;
  }

  void resolve(List<Stmt> statements) {
    for (Stmt statement : statements) {
      resolve(statement);
    }
  }

  private void resolve(Stmt stmt) {
    stmt.accept(this);
  }

  private void resolve(Expr expr) {
    expr.accept(this);
  }

  private void beginScope() {
    scopes.push(new HashMap<String, Boolean>());
  }

  private void endScope() {
    scopes.pop();
  }

  private void declare(Token name) {
    if (scopes.isEmpty()) return;

    // Add the variable to the innermost scope so it shadows
    // any outer one and so that we know the variable exists. We mark it as "not
    // ready yet" by binding its name to `false` in the scope map. The value
    // associated with a key in the scope map represents whether or not we have
    // finished resolving that variable's initializer.
    Map<String, Boolean> scope = scopes.peek();
    scope.put(name.lexeme, false);
  }

  private void define(Token name) {
    if (scopes.isEmpty()) return;
    // Set the variable's value in the scope map to `true` to mark it as fully
    // initialized and available for use.
    scopes.peek().put(name.lexeme, true);
  }
}
