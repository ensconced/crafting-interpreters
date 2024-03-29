package com.craftinginterpreters.lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Interpreter implements Expr.Visitor<Object>, Stmt.Visitor<Void> {
  final Environment globals = new Environment();
  private Environment environment = globals;
  private final Map<Expr, Integer> locals = new HashMap<>();

  Interpreter() {
    globals.define("clock", new LoxCallable() {
      @Override
      public int arity() {
        return 0;
      }

      @Override
      public Object call(Interpreter interpreter, List<Object> arguments) {
        return (double) System.currentTimeMillis() / 1000.0;
      }

      @Override
      public String toString() {
        return "<native fn>";
      }
    });
  }

  void interpret(List<Stmt> statements) {
    try {
      for (Stmt statement : statements) {
        execute(statement);
      }
    } catch (RuntimeError error) {
      Lox.runtimeError(error);
    }
  }

  @Override
  public Object visitLiteralExpr(Expr.Literal expr) {
    // We already eagerly produced the runtime value during scanning and stuffed
    // it in to the token. So to evaluate it now, we simply pull that value back
    // out.
    return expr.value;
  }

  @Override
  public Object visitLogicalExpr(Expr.Logical expr) {
    Object left = evaluate(expr.left);

    // short-circuiting
    if (expr.operator.type == TokenType.OR) {
      if (isTruthy(left))
        return left;
    } else {
      if (!isTruthy(left))
        return left;
    }

    return evaluate(expr.right);
  }

  @Override
  public Object visitSetExpr(Expr.Set expr) {
    Object object = evaluate(expr.object);

    if (!(object instanceof LoxInstance)) {
      throw new RuntimeError(expr.name, "Only instances have fields.");
    }

    Object value = evaluate(expr.value);
    ((LoxInstance) object).set(expr.name, value);
    return value;
  }

  @Override
  public Object visitSuperExpr(Expr.Super expr) {
    int distance = locals.get(expr);
    LoxClass superclass = (LoxClass) environment.getAt(distance, "super");
    // Offsetting the distance by one looks up "this" in the inner environment. Not
    // the most elegant, but it works.
    LoxInstance object = (LoxInstance) environment.getAt(distance - 1, "this");

    LoxFunction method = superclass.findMethod(expr.method.lexeme);

    if (method == null) {
      throw new RuntimeError(expr.method, "Undefined property '" + expr.method.lexeme + "'.");
    }

    return method.bind(object);
  }

  @Override
  public Object visitThisExpr(Expr.This expr) {
    return lookUpVariable(expr.keyword, expr);
  }

  @Override
  public Object visitGroupingExpr(Expr.Grouping expr) {
    // A grouping node has a reference to an inner node for the expression
    // contained inside the parens. To evaluate the grouping expression itself,
    // we recursively evaluate that subexpression and return it.
    return evaluate(expr.expression);
  }

  private Object evaluate(Expr expr) {
    return expr.accept(this);
  }

  private void execute(Stmt stmt) {
    stmt.accept(this);
  }

  void resolve(Expr expr, int depth) {
    locals.put(expr, depth);
  }

  void executeBlock(List<Stmt> statements, Environment environment) {
    Environment previous = this.environment;
    try {
      this.environment = environment;

      for (Stmt statement : statements) {
        execute(statement);
      }
    } finally {
      this.environment = previous;
    }
  }

  @Override
  public Void visitBlockStmt(Stmt.Block stmt) {
    executeBlock(stmt.statements, new Environment(environment));
    return null;
  }

  @Override
  public Void visitClassStmt(Stmt.Class stmt) {
    Object superclass = null;
    if (stmt.superclass != null) {
      superclass = evaluate(stmt.superclass);
      if (!(superclass instanceof LoxClass)) {
        throw new RuntimeError(stmt.superclass.name, "Superclass must be a class.");
      }
    }

    // When we interpret a class declaration statement, we turn the syntactic
    // representation of the class - its AST node - into its runtime representation.
    // And each of the class's method declarations blossoms into a LoxFunction
    // object.

    // NB we use a two-stage variable binding process to allow references to the
    // class inside its own methods.
    environment.define(stmt.name.lexeme, null);

    if (stmt.superclass != null) {
      environment = new Environment(environment);
      environment.define("super", superclass);
    }

    Map<String, LoxFunction> methods = new HashMap<>();
    for (Stmt.Function method : stmt.methods) {
      LoxFunction function = new LoxFunction(method, environment, method.name.lexeme.equals("init"));
      methods.put(method.name.lexeme, function);
    }

    // turn the class syntax node into a LoxClass - the runtime representation of a
    // class.
    LoxClass klass = new LoxClass(stmt.name.lexeme, (LoxClass) superclass, methods);

    if (superclass != null) {
      environment = environment.enclosing;
    }

    environment.assign(stmt.name, klass);
    return null;
  }

  @Override
  public Void visitExpressionStmt(Stmt.Expression stmt) {
    evaluate(stmt.expression);
    return null;
  }

  @Override
  public Void visitFunctionStmt(Stmt.Function stmt) {
    LoxFunction function = new LoxFunction(stmt, environment, false);
    environment.define(stmt.name.lexeme, function);
    return null;
  }

  @Override
  public Void visitIfStmt(Stmt.If stmt) {
    if (isTruthy(evaluate(stmt.condition))) {
      execute(stmt.thenBranch);
    } else if (stmt.elseBranch != null) {
      execute(stmt.elseBranch);
    }
    return null;
  }

  @Override
  public Void visitPrintStmt(Stmt.Print stmt) {
    Object value = evaluate(stmt.expression);
    System.out.println(stringify(value));
    return null;
  }

  @Override
  public Void visitReturnStmt(Stmt.Return stmt) {
    Object value = null;
    if (stmt.value != null)
      value = evaluate(stmt.value);
    // use an exception to unwind the interpreter stack back to where we began
    // executing the body
    throw new Return(value);
  }

  @Override
  public Void visitVarStmt(Stmt.Var stmt) {
    Object value = null;
    if (stmt.initializer != null) {
      value = evaluate(stmt.initializer);
    }
    environment.define(stmt.name.lexeme, value);
    return null;
  }

  @Override
  public Void visitWhileStmt(Stmt.While stmt) {
    while (isTruthy(evaluate(stmt.condition))) {
      execute(stmt.body);
    }
    return null;
  }

  @Override
  public Object visitAssignExpr(Expr.Assign expr) {
    Object value = evaluate(expr.value);

    Integer distance = locals.get(expr);
    if (distance != null) {
      environment.assignAt(distance, expr.name, value);
    } else {
      globals.assign(expr.name, value);
    }

    return value;
  }

  @Override
  public Object visitBinaryExpr(Expr.Binary expr) {
    // NB we evaluate the operands in left-to-right order. If those operands have
    // side-effects, that choice is user-visible, so this isn't simply an
    // implementation detail.
    Object left = evaluate(expr.left);
    Object right = evaluate(expr.right);

    switch (expr.operator.type) {
    case GREATER:
      checkNumberOperands(expr.operator, left, right);
      return (double) left > (double) right;
    case GREATER_EQUAL:
      checkNumberOperands(expr.operator, left, right);
      return (double) left >= (double) right;
    case LESS:
      checkNumberOperands(expr.operator, left, right);
      return (double) left < (double) right;
    case LESS_EQUAL:
      checkNumberOperands(expr.operator, left, right);
      return (double) left <= (double) right;
    case BANG_EQUAL:
      return !isEqual(left, right);
    case EQUAL_EQUAL:
      return isEqual(left, right);
    case MINUS:
      checkNumberOperands(expr.operator, left, right);
      return (double) left - (double) right;
    case PLUS:
      if (left instanceof Double && right instanceof Double) {
        return (double) left + (double) right;
      }
      if (left instanceof String && right instanceof String) {
        return (String) left + (String) right;
      }
      throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings");
    case SLASH:
      checkNumberOperands(expr.operator, left, right);
      return (double) left / (double) right;
    case STAR:
      checkNumberOperands(expr.operator, left, right);
      return (double) left * (double) right;
    }

    // Unreachable
    return null;
  }

  @Override
  public Object visitCallExpr(Expr.Call expr) {
    Object callee = evaluate(expr.callee);

    List<Object> arguments = new ArrayList<>();
    for (Expr argument : expr.arguments) {
      arguments.add(evaluate(argument));
    }

    if (!(callee instanceof LoxCallable)) {
      throw new RuntimeError(expr.paren, "Can only call functions and classes.");
    }

    LoxCallable function = (LoxCallable) callee;

    // functions in lox have strict arity
    if (arguments.size() != function.arity()) {
      throw new RuntimeError(
          expr.paren,
          "Expected " + function.arity() + " arguments but got " + arguments.size() + "."
      );
    }

    return function.call(this, arguments);
  }

  @Override
  public Object visitGetExpr(Expr.Get expr) {
    Object object = evaluate(expr.object);
    if (object instanceof LoxInstance) {
      return ((LoxInstance) object).get(expr.name);
    }
    // If the object is some other type e.g. a number, invoking a getter on it is a
    // runtime error.
    throw new RuntimeError(expr.name, "Only instances have properties.");
  }

  @Override
  public Object visitUnaryExpr(Expr.Unary expr) {
    // Like grouping, unary expressions have a single subexpression that we must
    // evaluate first. The difference is that the unary expression itself does a
    // little work afterwards.
    Object right = evaluate(expr.right);

    switch (expr.operator.type) {
    case BANG:
      return !isTruthy(right);
    case MINUS:
      // The subexpression must be a number. We don't statically know that in
      // java, so we need a type cast. This cast happens at runtime - this is the core
      // of what makes Lox a dynamically typed language.
      checkNumberOperand(expr.operator, right);
      return -(double) right;
    }

    // Unreachable
    return null;
  }

  @Override
  public Object visitVariableExpr(Expr.Variable expr) {
    return lookUpVariable(expr.name, expr);
  }

  private Object lookUpVariable(Token name, Expr expr) {
    Integer distance = locals.get(expr);
    if (distance != null) {
      return environment.getAt(distance, name.lexeme);
    } else {
      return globals.get(name);
    }
  }

  private void checkNumberOperand(Token operator, Object operand) {
    if (operand instanceof Double)
      return;
    throw new RuntimeError(operator, "Operand must be a number.");
  }

  private void checkNumberOperands(Token operator, Object left, Object right) {
    if (left instanceof Double && right instanceof Double)
      return;
    throw new RuntimeError(operator, "Operands must be numbers");
  }

  private boolean isTruthy(Object object) {
    if (object == null)
      return false;
    if (object instanceof Boolean)
      return (boolean) object;
    return true;
  }

  private boolean isEqual(Object a, Object b) {
    if (a == null && b == null)
      return true;
    if (a == null)
      return false;
    return a.equals(b);
  }

  private String stringify(Object object) {
    if (object == null)
      return "nil";
    if (object instanceof Double) {
      String text = object.toString();
      if (text.endsWith(".0")) {
        text = text.substring(0, text.length() - 2);
      }
      return text;
    }
    return object.toString();
  }
}
