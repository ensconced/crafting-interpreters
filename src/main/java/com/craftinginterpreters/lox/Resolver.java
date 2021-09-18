package com.craftinginterpreters.lox;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import jdk.javadoc.internal.tool.resources.javadoc;

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
  public Void visitFunctionStmt(Stmt.Function stmt) {
    // Similar to visitVariableStmt, we declare and define the name of the function
    // in the current scope. Unlike variables, though, we define the name eagerly,
    // before resolving the function's body. This lets a function recursively refer
    // to itself inside its own body/
    declare(stmt.name);
    define(stmt.name);
    // Then resolve the function's body.
    resolveFunction(stmt);
    return null;
  }

  @Override
  public Void visitVarStmt(Stmt.Var stmt) {
    // binding is split into two steps: declaring and defining
    declare(stmt.name);
    if (stmt.initializer != null) {
      // Resolve the initializer expression in the same scope where the new variable
      // now exists but is unavailable.
      resolve(stmt.initializer);
    }
    // Once the initializer expression is done, the variable is ready for prime
    // time.
    define(stmt.name);
    return null;
  }

  @Override
  public Void visitAssignExpr(Expr.Assign expr) {
    // First, resolve the expression for the assigned value in case it also contains
    // references to other variables.
    resolve(expr.value);
    // Then resolve the variable that's being assigned to.
    resolveLocal(expr, expr.name);
    return null;
  }

  @Override
  public Void visitVariableExpr(Expr.Variable expr) {
    // If the variable exists in the current scope but with value of false, then we
    // have declared it but not yet defined it.
    if (!scopes.isEmpty() && scopes.peek().get(expr.name.lexeme) == Boolean.FALSE) {
      Lox.error(expr.name, "Can't read local variable in its own initializer.");
    }

    resolveLocal(expr, expr.name);
    return null;
  }

  void resolve(List<Stmt> statements) {
    for (Stmt statement : statements) {
      resolve(statement);
    }
  }

  private void resolveFunction(Stmt.Function function) {
    beginScope();
    for (Token param : function.params) {
      declare(param);
      define(param);
    }
    // N.B. this is different from how the interpreter handles function
    // declarations. At runtime, declaring a function doesn't do anything with the
    // function's body. The body doesn't get touched until later when the function
    // is called. But in this static anlysis, we immediately traverse into the body
    // right here.
    resolve(function.body);
    endScope();
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

  private void resolveLocal(Expr expr, Token name) {
    // Start at the innermost scope and work outwards, looking in each map for a
    // matching name. If we walk through all of the block scopes and never find the
    // variable, we leave it unresolved and assume it's global.
    for (int i = scopes.size() - 1; i >= 0; i--) {
      if (scopes.get(i).containsKey(name.lexeme)) {
        interpreter.resolve(expr, scopes.size() - 1 - i);
        return;
      }
    }
  }
}
