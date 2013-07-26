package net.janvsmachine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import net.janvsmachine.ScaLisp._

@RunWith(classOf[JUnitRunner])
class ScaLispSuite extends FunSuite {

  test("tokenize") {
    assert(tokenize("") === Array(), "Tokenize empty string should return empty list")
    assert(tokenize(" ") === Array(), "Tokenize string with only whitespace should return empty list")
    assert(tokenize("foo") === Array("foo"), "Tokenize string with single token")
    assert(tokenize("1.0") === Array("1.0"), "Tokenize string with single token")
    assert(tokenize("foo bar baz") === Array("foo", "bar", "baz"), "Tokenize string with multiple tokens")
    assert(tokenize("1 2 3") === Array("1", "2", "3"), "Tokenize string with numbers")
    assert(tokenize(" 1    2  3  ") === Array("1", "2", "3"), "Tokenize string with multiple spaces")
    assert(tokenize("+ 2 3") === Array("+", "2", "3"), "Tokenize string with other tokens")
    assert(tokenize("()") === Array("(", ")"), "Tokenize string with brackets only")
    assert(tokenize("(+ 2 3)") === Array("(", "+", "2", "3", ")"), "Tokenize string with brackets")
    assert(tokenize("(* (+ 2 3) 1") === Array("(", "*", "(", "+", "2", "3", ")", "1"), "Tokenize string with brackets")
  }

  test("parse simple cases") {
    assert(parse(List("")) === List(""), "Parse empty string")
    assert(parse(List("1")) === List(1), "Parse single integer")
    val parsedFloat = (parse(List("1.234"))(0).asInstanceOf[Float])
    assert(Math.abs(parsedFloat - 1.234) < 0.00001, "Parse single float")
    assert(parse(List("42")) === List(42), "Parse single number")
    assert(parse(List("true")) === List(true), "Parse true")
    assert(parse(List("false")) === List(false), "Parse false")
    assert(parse(List("x")) === List("x"), "Parse single character identifier")
    assert(parse(List("xyz")) === List("xyz"), "Parse multi-character identifier")
    assert(parse(List("+")) === List("+"), "Parse non-alphabetic identifier")
    assert(parse(List("set!")) === List("set!"), "Parse another non-alphabetic identifier")
    assert(parse(List("+", "1", "2")) === List("+", 1, 2), "Parse multiple identifiers")
  }

  test("parse more complex expressions, with lists") {
    assert(parse(List("(", ")")) === List(List()), "Parse empty list")
    assert(parse(List("(", "+", "1", "2", ")")) ===
      List(List("+", 1, 2)), "Parse simple list")
    assert(parse(List("(", "+", "1", "(", "+", "2", "3", ")", ")")) ===
      List(List("+", 1, List("+", 2, 3))), "Parse list of lists")
  }

  test("parse multiple lists at top level") {
    assert(parse(List("(", "+", "1", "2", ")", "3", "(", "4", "5", ")"))
      === List(List("+", 1, 2), 3, List(4, 5)), "Multiple root lists, should return all")
  }

  test("parse error cases") {
    // These are valid identifiers according to the current, simple implementation!
    // intercept[IllegalArgumentException] { parse(List("1b")) }
    // intercept[IllegalArgumentException] { parse(List("\"foo\"")) }
    intercept[IllegalArgumentException] { parse(List("(")) }

    // The following would be nice to catch but aren't essential for this exercise.
    // intercept[IllegalArgumentException] { parse(List(")")) }
    // intercept[IllegalArgumentException] { parse(List(")", "(")) }
  }

  test("Converting expression to string representation using toStr") {
    assert(toStr("") === "", "Empty string for empty expression")
    assert(toStr(123) === "123", "Single numeric literal")
    assert(toStr("x!") === "x!", "Single symbol")
    assert(toStr(true) === "true", "'true' literal")
    assert(toStr(false) === "false", "'false' literal")

    assert(toStr(List(123)) === "(123)", "List containing a single literal")
    assert(toStr(List(123, 456, "abc", 78)) === "(123 456 abc 78)", "List containing multiple literals")
    assert(toStr(List(true, List(123, 456))) === "(true (123 456))", "Nested lists with literals")
    assert(toStr(List("+", List("*", 3, "x"))) === "(+ (* 3 x))", "Nested lists representing an arithmetic expression")
  }

  test("Env find with unknown symbols") {
    intercept[Exception] { new Env(None).find("x") }
    intercept[Exception] { new Env(Some(new Env(None))).find("x") }
  }

  test("Env with pre-populated list of values") {
    assert(new Env(None, List("x"), List(42)).find("x") === 42, "Single defined variable")
    assert(new Env(None, List("x", "y"), List(42, 36)).find("x") === 42, "Find first variable")
    assert(new Env(None, List("x", "y"), List(42, 36)).find("y") === 36, "Find second variable")
    intercept[Exception] { new Env(None, List("x"), List(42)).find("y") }
  }

  test("Access unknown symbols") {
    intercept[Exception] { evalStr("x") }
    intercept[Exception] { evalStr("(begin (define x 42) y)") }
  }

  def evalStr(str: String): Any = eval(parse(tokenize(str))(0), ScaLisp.createGlobalEnv())

  def evalMultipleStr(str: String): Seq[Any] = {
    for (expr <- parse(tokenize(str)))
      yield eval(expr, ScaLisp.createGlobalEnv())
  }

  test("eval with literal argument") {
    assert(evalStr("42") === 42, "eval with numeric literal")
    assert(evalStr("true") === true, "eval with boolean value true")
    assert(evalStr("false") === false, "eval with boolean value false")
  }

  test("eval 'quote'") {
    val globalEnv = createGlobalEnv()
    assert(eval(List("q", List()), globalEnv) === List(), "eval quote with empty list")
    assert(eval(List("q", 42), globalEnv) === 42, "eval quote of numeric literal")
    assert(eval(List("q", List(1, 2, 3)), globalEnv) === List(1, 2, 3), "eval list of numeric literals")
    assert(eval(List("q", List(List(1, 2), List(3, 4))), globalEnv) === List(List(1, 2), List(3, 4)), "eval list of lists")
    assert(evalStr("(q ())") === List(), "eval string with empty list")
    assert(evalStr("(q (1 2 3))") === List(1, 2, 3), "eval string with list of numeric literals")
    assert(evalStr("(quote (1 2 3))") === List(1, 2, 3), "eval string with list of numeric literals, using long form of quote")
    assert(evalStr("(quote ((1 2) (3 4)))") === List(List(1, 2), List(3, 4)), "eval quote with nested lists")
    assert(evalStr("(quote ((1 2) (3 4) ((5 6) (7 8))))") === List(List(1, 2), List(3, 4), List(List(5, 6), List(7, 8))),
      "eval quote with deeply nested lists")
  }

  test("eval 'quote', invalid arguments") {
    intercept[IllegalArgumentException] { evalStr("(quote)") }
    intercept[IllegalArgumentException] { evalStr("(quote 1 2)") }
  }

  test("eval 'atom'") {
    assert(evalStr("(atom 42)") == true, "integer literal is an atom")
    assert(evalStr("(atom 3.14)") == true, "float literal is an atom")
    assert(evalStr("(atom false)") == true, "boolean literal is an atom")
    assert(evalStr("(atom (q ()))") == false, "empty list is an atom")
    assert(evalStr("(atom (q (1 2 3)))") == false, "list is not an atom")
  }

  test("eval 'atom', invalid cases") {
    intercept[IllegalArgumentException] { evalStr("(atom)") }
    intercept[IllegalArgumentException] { evalStr("(atom 21 42)") }
  }

  test("eval 'eq?'") {
    assert(evalStr("(eq? 42 42)") == true, "eq? with two equal numbers")
    assert(evalStr("(eq? 42 123)") == false, "eq? with two different numbers")
    assert(evalStr("(eq? (quote ()) (quote ()))") == true, "eq? with two empty lists")
    assert(evalStr("(eq? (quote (a b)) (quote ()))") == false, "eq? with non-empty and empty list")
    assert(evalStr("(eq? (quote ()) (quote (a b)))") == false, "eq? with empty and non-empty list")
    assert(evalStr("(eq? (quote (a b)) (quote (a b)))") == false, "eq? with two non-empty lists should return false even if lists have same entries")
    assert(evalStr("(eq? (+ 1 2) (+ 2 1))") == true, "eq? should evaluate arguments")
  }

  test("eval 'eq?', invalid arguments") {
    intercept[IllegalArgumentException] { evalStr("(eq?)") }
    intercept[IllegalArgumentException] { evalStr("(eq? 1)") }
    intercept[IllegalArgumentException] { evalStr("(eq? 1 2 3)") }
    intercept[IllegalArgumentException] { evalStr("(eq? 42 123 foo)") }
    intercept[IllegalArgumentException] { evalStr("(eq? (quote ()) (quote ()) 42)") }
  }

  test("eval 'car' with non-list should give an error") {
    intercept[IllegalArgumentException] { evalStr("(car 42)") }
    intercept[IllegalArgumentException] { eval(List("car", 42), createGlobalEnv()) }
  }

  test("eval 'car' with empty list should give an error") {
    intercept[IllegalArgumentException] { eval(List("car", List()), createGlobalEnv()) }
    intercept[IllegalArgumentException] { evalStr("(car (quote ()))") }
  }

  test("eval 'car', valid cases") {
    assert(evalStr("(car (quote (1)))") == 1, "car with single-item list")
    assert(evalStr("(car (quote (1 2 3)))") == 1, "car with multi-item list")
  }

  test("eval 'cdr' with non-list should give an error") {
    intercept[IllegalArgumentException] { evalStr("(cdr 42)") }
  }

  test("eval 'cdr' with empty list should give an error") {
    intercept[IllegalArgumentException] { evalStr("(cdr (quote ()))") }
  }

  test("eval 'cdr', valid cases") {
    assert(evalStr("(cdr (quote (1)))") === List(), "cdr with single-item list")
    assert(evalStr("(cdr (quote (1 2 3)))") === List(2, 3), "cdr with multi-item list")
    assert(evalStr("(cdr (quote ((1 2) (3 4))))") === List(List(3, 4)), "cdr with nested list")
    assert(evalStr("(cdr (quote (1)) 1)") === List(), "cdr with single-item list, should ignore extra arguments")
  }

  test("eval 'null?'") {
    assert(evalStr("(null? 0)") == false, "null? with single integer literal")
    assert(evalStr("(null? false)") == false, "null? with single boolean literal")
    assert(evalStr("(null? 42)") == false, "null? with single literal 42")
    assert(evalStr("(null? (quote (1 2 3)))") == false, "null? with non-empty list argument")
    assert(evalStr("(null? (quote ()))") == true, "null? with empty list argument")
  }

  test("eval 'null?' with missing argument") {
    intercept[IllegalArgumentException] { evalStr("(null?)") }
  }

  test("eval 'null?' with too many arguments") {
    intercept[IllegalArgumentException] { evalStr("(null? 1 2)") }
  }

  test("eval 'cons', valid cases") {
    assert(evalStr("(cons 1 (quote ()))") === List(1), "cons value to empty list")
    assert(evalStr("(cons 1 (quote (2)))") === List(1, 2), "cons value to list with single item")
    assert(evalStr("(cons 1 (quote (2 3 4)))") === List(1, 2, 3, 4), "cons value to list with multiple items")
    assert(evalStr("(cons (quote (1)) (quote (2 3 4)))") === List(List(1), 2, 3, 4), "cons list to list with multiple items")
  }

  test("eval 'cons', invalid arguments") {
    intercept[IllegalArgumentException] { evalStr("(cons)") }
    intercept[IllegalArgumentException] { evalStr("(cons 1)") }
    intercept[IllegalArgumentException] { evalStr("(cons 1 2)") }
  }

  test("eval 'if', valid cases") {
    assert(evalStr("(if true 42 36)") === 42, "'if' with literal 'true' value")
    assert(evalStr("(if false 42 36)") === 36, "'if' with literal 'false' value")
    assert(evalStr("(if 1 42 36)") === 42, "'if' with 1 literal")
    assert(evalStr("(if 0 42 36)") === 42, "'if' with 0 literal")
    assert(evalStr("(if (quote ()) 42 36)") === 42, "'if' with list value")

    assert(evalStr("(if true (quote (1 2)) 0)") === List(1, 2), "'if' with evaluted consequence")
    assert(evalStr("(if false 0 (quote (1 2)))") === List(1, 2), "'if' with evaluted alternative")

    assert(evalStr("(if (< 1 2) 42 36)") === 42, "'if' with calculated false predicate")
    assert(evalStr("(if (> 1 2) 42 36)") === 36, "'if' with calculated true predicate")
  }

  test("eval 'if' with pre-defined operators and combinations of types") {
    assert(evalStr("(if (< 1 2.0) 42 36)") === 42, "'<' comparing integer and float")
    assert(evalStr("(if (> 1.0 2) 42 36)") === 36, "'>' comparing float and integer")
  }

  test("eval 'if', invalid arguments") {
    intercept[IllegalArgumentException] { evalStr("(if)") }
    intercept[IllegalArgumentException] { evalStr("(if false)") }
    intercept[IllegalArgumentException] { evalStr("(if false 42)") }
    intercept[Exception] { evalStr("(if false 42 36 666)") }
  }

  test("eval 'cond', valid cases") {
    assert(evalStr("(cond (true 42) (false 36))") === 42, "'cond' with first condition matching")
    assert(evalStr("(cond (false 42) (true 36))") === 36, "'cond' with second condition matching")
    assert(evalStr("(cond ((< 1 2) 42))") === 42, "'cond' with calculated predicate")
    assert(evalStr("(cond ((> 1 2) 36) ((< 1 2) 42))") === 42, "'cond' with calculated predicates")
    assert(evalStr("(cond (true (* 2 21)))") === 42, "'cond' with calculated result expression")
  }

  test("eval 'cond', no valid condition matching") {
    intercept[Exception] { evalStr("(cond (false 42))") }
    intercept[Exception] { evalStr("(cond (false 42) (false 36))") }
  }

  test("eval 'cond', invalid arguments") {
    intercept[Exception] { evalStr("(cond)") }
    intercept[IllegalArgumentException] { evalStr("(cond (true))") }
    intercept[IllegalArgumentException] { evalStr("(cond (false 42) (true))") }
  }

  test("eval 'begin', simple cases") {
    assert(evalStr("(begin 1)") === 1, "single expression with literal value")
    assert(evalStr("(begin 1 2 3)") === 3, "multiple expressions with literal values")
    assert(evalStr("(begin (quote (1 2)) (quote (3 4)))") === List(3, 4), "multiple expressions with list values")
  }

  test("eval 'begin', error cases") {
    intercept[Exception] { evalStr("(begin)") }
  }

  test("eval of built-in operators with matching types") {
    assert(evalStr("(< 1 2)") === true, "'<' comparing ints")
    assert(evalStr("(> 1 2)") === false, "'>' comparing ints")
    assert(evalStr("(< 1.0 2.0)") === true, "'<' comparing floats")
    assert(evalStr("(> 1.0 2.0)") === false, "'>' comparing floats")

    assert(evalStr("(+ 1 2)") === 3, "'+' with ints")
    assert(evalStr("(+ 1.0 2.0)") === 3.0, "'+' with floats")
    assert(evalStr("(* 2 3)") === 6, "'*' with ints")
    assert(evalStr("(* 2.0 3.0)") === 6.0, "'*' with floats")
  }

  test("eval of built-in operators with different but compatible types") {
    assert(evalStr("(< 1 2.0)") === true, "'1 < 2.0")
    assert(evalStr("(> 1 2.0)") === false, "1 > 2.0")
    assert(evalStr("(< 1.0 2)") === true, "1.0 < 2")
    assert(evalStr("(> 1.0 2)") === false, "1.0 > 2")

    assert(evalStr("(+ 1 2.0)") === 3.0, "1 + 2.0")
    assert(evalStr("(+ 1.0 2)") === 3.0, "1.0 + 2")
    assert(evalStr("(* 2.0 3)") === 6.0, "2.0 * 3")
    assert(evalStr("(* 2 3.0)") === 6.0, "2 * 3.0")
  }

  test("eval of built-in operators with incompatible types") {
    intercept[IllegalArgumentException] { evalStr("(< 1 false") }
  }

  test("eval of + operator with variable numbers of arguments") {
    assert(evalStr("(+)") === 0, "'+' with no arguments")
    assert(evalStr("(+ 3)") === 3, "'+' with a single argument")
    assert(evalStr("(+ 3 4)") === 7, "'+' with two arguments")
    assert(evalStr("(+ 3 4 5 6 7)") === 25, "'+' with five arguments")
  }

  test("eval of - operator with variable numbers of arguments") {
    intercept[IllegalArgumentException] { evalStr("(-)") }
    //    assert(evalStr("(- 3)") === 3, "'-' with a single argument")
    assert(evalStr("(- 1 2)") === -1, "'-' with two arguments")
    assert(evalStr("(- 1 2 3 4 5)") === 1 - 2 - 3 - 4 - 5, "'-' with five arguments")
  }

  test("eval of * operator with variable numbers of arguments") {
    assert(evalStr("(*)") === 1, "'*' with no arguments")
    assert(evalStr("(* 3)") === 3, "'*' with a single argument")
    assert(evalStr("(* 3 4)") === 3 * 4, "'*' with two arguments")
    assert(evalStr("(* 3 4 5 6 7)") === 3 * 4 * 5 * 6 * 7, "'*' with five arguments")
  }

  test("eval of / operator with variable numbers of arguments") {
    intercept[IllegalArgumentException] { evalStr("(/)") }
    assert(evalStr("(/ 8)") === 8, "'/' with a single argument")
    assert(evalStr("(/ 8 2)") === 4, "'/' with two arguments")
    assert(evalStr("(/ 8 2 2)") === 2, "'/' with five arguments")
  }

  test("eval of < operator with variable numbers of arguments") {
    assert(evalStr("(<)") === true, "'<' with no arguments")
    assert(evalStr("(< 1)") === true, "'<*' with a single argument")
    assert(evalStr("(< 1 2)") === true, "1 < 2")
    assert(evalStr("(< 1 2 3 4 5)") === true, "1 < 2 < 3 < 4 < 5")

    assert(evalStr("(< 2 1)") === false, "2 < 1")
    assert(evalStr("(< 1 2 4 3 5)") === false, "1 < 2 < 4 < 3 < 5")
  }

  test("eval 'define', check returned value") {
    // This behaviour differs from tiddlylisp; a (define) statement there returns nothing.
    // We match the behaviour of MIT/GNU Scheme instead.
    assert(evalStr("(define x 42)") === "x", "simple define expression, should return the defined symbol")
  }

  test("eval 'define', invalid arguments") {
    intercept[IllegalArgumentException] { evalStr("(define)") }
    intercept[IllegalArgumentException] { evalStr("(define x)") }
    intercept[IllegalArgumentException] { evalStr("(define 1 2)") }
    intercept[Exception] { evalStr("(define x 1 2 3)") }
  }

  test("eval 'set!', check returned value") {
    // As for 'define', this behaviour differs from tiddlylisp, a 'set!' expression there returns nothing.
    assert(evalStr("(begin (define x 42) (set! x 36))") === 42, "simple set! expression, should return the previous value")
  }

  test("eval 'set!' for undefined symbol") {
    intercept[IllegalArgumentException] { evalStr("(set! x 42)") }
  }

  test("eval 'set!', invalid arguments") {
    intercept[IllegalArgumentException] { evalStr("(set!)") }
    intercept[IllegalArgumentException] { evalStr("(set! x)") }
    intercept[IllegalArgumentException] { evalStr("(set! x 1 2)") }
  }

  test("eval 'define', 'set!' and symbol lookup") {
    assert(evalStr("(begin (define x 42) (set! x 36) x)") === 36, "define then set")
    assert(evalStr("(begin (define x 42) (define x 36) x)") === 36, "define twice, should overwrite value")
    assert(evalStr("(begin (define id (lambda (x) x)))") === "id", "Define for lambda should return its name")
    assert(evalStr("(begin (define id (lambda (x) x)) (id 1))") === 1, "Defined ID function should return same value")
  }

  test("eval 'lambda', simple cases") {
    val l: Lambda = evalStr("(lambda () 42)").asInstanceOf[Lambda]
    assert(l.apply(List()) === 42, "Lambda with no parameters, return number, called on returned closure")
    assert(evalStr("((lambda () 42))") === 42, "Lambda with no parameters, return number")
    assert(evalStr("((lambda (x) x) 42)") === 42, "Lambda with single parameter, returns that parameter (number)")
    assert(evalStr("((lambda (x) x) (quote (1 2 3)))") === List(1, 2, 3), "Lambda with single parameter, returns that parameter (list)")
  }

  test("eval 'lambda', invalid numbers of arguments") {
    // Missing argument.
    intercept[Exception] { evalStr("((lambda (x) x))") }

    // Could add checks for too many arguments
    // intercept[Exception] { evalStr("((lambda () 42) 123)") }
    // intercept[Exception] { evalStr("((lambda (x) x) 123 456)") }
  }

  test("eval 'lambda' with procedure calls") {
    assert(evalStr("((lambda (x) (* x x)) 4))") === 16, "Lambda with single parameter, returns square of parameter")
    assert(evalStr("(begin (define square (lambda (x) (* x x))) (square 4))") === 16, "Lambda assigned to symbol then used")
  }

  test("eval unknown procedure call") {
    intercept[IllegalArgumentException] { evalStr("(foo)") }
    intercept[IllegalArgumentException] { evalStr("(foo 1 2 3)") }
  }

  test("symbol defined in nested environment") {
    val code = """(begin
                    |(define x 1)
                    |(define foo (lambda (x) (* x 2)))
                    |(+ x (foo 3)))""".stripMargin
    assert(evalStr(code) === 7, "Argument to lambda is different from variable in outer environment")

    val code2 = """(begin
                    |(define x 1)
                    |(define foo (lambda (y) (begin (define x 2) (* x y))
                    |(+ x (foo 3)))"""
    assert(evalStr(code) === 7, "Variable defined inside lambda is different from variable in outer environment")
  }

  test("eval a program of multiple expressions") {
    assert(evalMultipleStr("1 2 3") === List(1, 2, 3), "Multiple literal expressions on one line")
  }

  test("square roots by newton's method") {
    val code = """(begin
                     |(define sqrt (lambda (x) (sqrt-iter 1 x)))
					 |(define sqrt-iter (lambda (guess x) (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x))))
	                 |(define good-enough? (lambda (guess x) (< (abs (- x (square guess))) 0.00001)))
	                 |(define abs (lambda (x) (if (< 0 x) x (- 0 x))))
	                 |(define square (lambda (x) (* x x)))
	                 |(define improve (lambda (guess x) (average guess (/ x guess))))
	                 |(define average (lambda (x y) (* 0.5 (+ x y))))
	                 |(sqrt 2))""".stripMargin
    val squareRootOfTwo = evalStr(code)
    assert(Math.abs(squareRootOfTwo.asInstanceOf[Float] - 1.41421) < 0.0001)
  }

}

