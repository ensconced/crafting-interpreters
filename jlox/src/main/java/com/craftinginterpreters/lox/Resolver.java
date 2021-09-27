package com.craftinginterpreters.lox;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

class Resolver implements Expr.Visitor<Void>, Stmt.Visitor<Void> {
  private final Interpreter interpreter;
  private final Stack<Map<String, Boolean>> scopes = new Stack<>();
  private FunctionType currentFunction = FunctionType.NONE;
  private ClassType currentClass = ClassType.NONE;

  Resolver(Interpreter interpreter) {
    this.interpreter = interpreter;
  }

  private enum FunctionType {
    NONE,
    FUNCTION,
    INITIALIZER,
    METHOD,
  }

  private enum ClassType {
    NONE,
    CLASS,
  }

  @Override
  public Void visitBlockStmt(Stmt.Block stmt) {
    beginScope();
    resolve(stmt.statements);
    endScope();
    return null;
  }

  @Override
  public Void visitClassStmt(Stmt.Class stmt) {
    ClassType enclosingClass = currentClass;
    currentClass = ClassType.CLASS;

    declare(stmt.name);
    define(stmt.name);

    if (stmt.superclass != null && stmt.name.lexeme.equals(stmt.superclass.name.lexeme)) {
      Lox.error(stmt.superclass.name, "A class can't inherit from itself.");
    }

    if (stmt.superclass != null) {
      // Since classes are usually declared at the top level, the superclass name
      // will most likely be a global variable, so this doesn't usually do anything
      // useful. However, Lox allows class declarations even inside blocks, so it's
      // possible the superclass name refers to a local variable. In that case, we
      // need to make sure it's resolved.
      resolve(stmt.superclass);
    }

    // If the class declaration has a superclass, then we create a new scope
    // surrounding all of its methods. In that scope, in that scope, we define the
    // name "super".
    if (stmt.superclass != null) {
      beginScope();
      scopes.peek().put("super", true);
    }

    // add new scope for "this"
    beginScope();
    scopes.peek().put("this", true);

    for (Stmt.Function method : stmt.methods) {
      FunctionType declaration = FunctionType.METHOD;
      if (method.name.lexeme.equals("init")) {
        declaration = FunctionType.INITIALIZER;
      }

      resolveFunction(method, declaration);
    }

    // end "this" scope
    endScope();

    // We're done resolving the class's methods, so discard this scope.
    if (stmt.superclass != null) endScope();

    // restore the old value
    currentClass = enclosingClass;
    return null;
  }

  @Override
  public Void visitExpressionStmt(Stmt.Expression stmt) {
    // An expression statement contains a single expression to traverse.
    resolve(stmt.expression);
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
    resolveFunction(stmt, FunctionType.FUNCTION);
    return null;
  }

  @Override
  public Void visitIfStmt(Stmt.If stmt) {
    resolve(stmt.condition);
    // Here we see how resolution is different from interpretation. When we resolve
    // an if statement, there is no control flow. We resolve the condition and
    // *both* branches. Where a dynamic execution steps only into the branch that is
    // run, a static analysis is conservative - it analyses any branch that *could*
    // run. Since either one could be reached at runtime, we resolve both.
    resolve(stmt.thenBranch);
    if (stmt.elseBranch != null) resolve(stmt.elseBranch);
    return null;
  }

  @Override
  public Void visitPrintStmt(Stmt.Print stmt) {
    // Like expression statements, a print statement contains a single
    // subexpression.
    resolve(stmt.expression);
    return null;
  }

  @Override
  public Void visitReturnStmt(Stmt.Return stmt) {
    if (currentFunction == FunctionType.NONE) {
      Lox.error(stmt.keyword, "Can't return from top-level code.");
    }
    if (stmt.value != null) {
      if (currentFunction == FunctionType.INITIALIZER) {
        Lox.error(stmt.keyword, "Can't return a value from an initializer.");
      }
      resolve(stmt.value);
    }
    return null;
  }

  @Override
  public Void visitWhileStmt(Stmt.While stmt) {
    resolve(stmt.condition);
    // We only need to resolve the body once.
    resolve(stmt.body);
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
  public Void visitBinaryExpr(Expr.Binary expr) {
    // Traverse into and resolve both operands.
    resolve(expr.left);
    resolve(expr.right);
    return null;
  }

  @Override
  public Void visitCallExpr(Expr.Call expr) {
    resolve(expr.callee);

    for (Expr argument : expr.arguments) {
      resolve(argument);
    }
    return null;
  }

  @Override
  public Void visitGetExpr(Expr.Get expr) {
    // Properties are looked up dynamically - they don't get resolved. So during
    // resolution we only recurse into the expression to the left of the dot. The
    // actual property access happens in the interpreter. i.e. property dispatch is
    // dynamic.
    resolve(expr.object);
    return null;
  }

  @Override
  public Void visitGroupingExpr(Expr.Grouping expr) {
    resolve(expr.expression);
    return null;
  }

  @Override
  public Void visitLiteralExpr(Expr.Literal expr) {
    // A literal expression doesn't mention any variables and doesn't contain any
    // subexpressions so there is no work to do.
    return null;
  }

  @Override
  public Void visitLogicalExpr(Expr.Logical expr) {
    // Since a static analysis does no control flow or short-circuiting, logcal
    // expressions are exactly the same as other binary operators.
    resolve(expr.left);
    resolve(expr.right);
    return null;
  }

  @Override
  public Void visitSetExpr(Expr.Set expr) {
    resolve(expr.value);
    resolve(expr.object);
    return null;
  }

  // We resolve `super` exactly as if it were a variable.
  @Override
  public Void visitSuperExpr(Expr.Super expr) {
    resolveLocal(expr, expr.keyword);
    return null;
  }

  @Override
  public Void visitThisExpr(Expr.This expr) {
    if (currentClass == ClassType.NONE) {
      Lox.error(expr.keyword, "Can't use 'this' outside of a class.");
    }

    resolveLocal(expr, expr.keyword);
    return null;
  }

  @Override
  public Void visitUnaryExpr(Expr.Unary expr) {
    resolve(expr.right);
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

  private void resolveFunction(Stmt.Function function, FunctionType type) {
    FunctionType enclosingFunction = currentFunction;
    currentFunction = type;
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
    currentFunction = enclosingFunction;
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
    if (scope.containsKey(name.lexeme)) {
      Lox.error(name, "Already a variable with this name in this scope.");
    }
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
