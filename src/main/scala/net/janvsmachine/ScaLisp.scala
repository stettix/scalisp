package net.janvsmachine

import java.io.EOFException
import scala.collection.mutable.HashMap
import scala.util.{ Try, Success, Failure }
import scala.io.Source

object ScaLisp extends App {

  type Expr = Any
  type Symbol = String
  type Lambda = PartialFunction[Seq[Expr], Expr]

  // The environment that stores symbols and optionally links to outer environments.
  class Env(outer: Option[Env], presets: Map[String, Expr]) extends HashMap[String, Expr] {
    def this(outer: Option[Env]) = this(outer, Map())
    def this(outer: Option[Env], vars: List[String], values: Seq[Expr]) = this(outer, Map() ++ vars.zip(values))

    this ++= presets

    def find(name: String): Expr = {
      if (contains(name)) apply(name) else outer match {
        case None => throw new IllegalArgumentException(s"Undefined symbol '$name'")
        case Some(e) => e.find(name)
      }
    }

    def define(name: String, value: Expr): Expr = {
      this += (name -> value)
      name
    }

    def set(name: String, value: Expr): Expr = {
      val previous: Expr = find(name)
      this += (name -> value)
      previous
    }
  }

  type BinaryOp = PartialFunction[(Expr, Expr), Expr]

  // Produce a binary operation that evaluates to the first suitable one in the chain.
  def chain(ops: BinaryOp*): BinaryOp = (PartialFunction.empty[(Expr, Expr), Expr] /: ops)(_ orElse _)

  // Convert partial function to function that fails if the partial function doesn't match.
  def applyOrFail(op: BinaryOp): (Expr, Expr) => Expr = (arg1: Expr, arg2: Expr) => op.lift(arg1, arg2) match {
    case Some(result) => result
    case None => throw new IllegalArgumentException(s"Unable to apply operation to arguments '$arg1' and '$arg2'")
  }

  def applyOrFailOp(op: BinaryOp): BinaryOp = {
    case (arg1, arg2) if op.isDefinedAt(arg1, arg2) => op(arg1, arg2)
    case (arg1, arg2) => throw new IllegalArgumentException(s"Unable to apply operation to arguments '$arg1' and '$arg2'")
  }

  // Apply binary operation to sequence of arguments, using given initial value.
  def opToLambda(initialValue: Expr, op: BinaryOp): Lambda = { case args => (initialValue /: args)(applyOrFail(op)) }

  // Apply binary operation to sequence of arguments, using first argument as initial value. 
  def opToLambda(op: BinaryOp): Lambda = {
    case arg1 :: args => (arg1 /: args)(applyOrFail(op))
    case _ => throw new IllegalArgumentException("At least one argument must be given")
  }

  // Apply binary operation to sequence of pairwise arguments, collect boolean results.
  def booleanOpToLambda(op: BinaryOp): Lambda = {
    case args if args.length >= 2 => {
      val pairwiseResults = args zip args.tail map applyOrFailOp(op)
      (true /: pairwiseResults)((overall, res) => { overall && (res == true) })
    }
    case _ => true
  }

  // Create the global environment, initialised with lambdas for the common numerical operations.
  def createGlobalEnv(): Env = new Env(None, Map[String, Lambda](
    "<" -> booleanOpToLambda(chain(
      { case (arg1: Int, arg2: Int) => arg1 < arg2 },
      { case (arg1: Int, arg2: Float) => arg1 < arg2 },
      { case (arg1: Float, arg2: Float) => arg1 < arg2 },
      { case (arg1: Float, arg2: Int) => arg1 < arg2 })),
    ">" -> booleanOpToLambda(chain(
      { case (arg1: Int, arg2: Int) => arg1 > arg2 },
      { case (arg1: Int, arg2: Float) => arg1 > arg2 },
      { case (arg1: Float, arg2: Float) => arg1 > arg2 },
      { case (arg1: Float, arg2: Int) => arg1 > arg2 })),
    "<=" -> booleanOpToLambda(chain(
      { case (arg1: Int, arg2: Int) => arg1 <= arg2 },
      { case (arg1: Int, arg2: Float) => arg1 <= arg2 },
      { case (arg1: Float, arg2: Float) => arg1 <= arg2 },
      { case (arg1: Float, arg2: Int) => arg1 <= arg2 })),
    ">=" -> booleanOpToLambda(chain(
      { case (arg1: Int, arg2: Int) => arg1 >= arg2 },
      { case (arg1: Int, arg2: Float) => arg1 >= arg2 },
      { case (arg1: Float, arg2: Float) => arg1 >= arg2 },
      { case (arg1: Float, arg2: Int) => arg1 >= arg2 })),
    "+" -> opToLambda(0, chain(
      { case (arg1: Int, arg2: Int) => arg1 + arg2 },
      { case (arg1: Int, arg2: Float) => arg1 + arg2 },
      { case (arg1: Float, arg2: Float) => arg1 + arg2 },
      { case (arg1: Float, arg2: Int) => arg1 + arg2 })),
    "*" -> opToLambda(1, chain(
      { case (arg1: Int, arg2: Int) => arg1 * arg2 },
      { case (arg1: Int, arg2: Float) => arg1 * arg2 },
      { case (arg1: Float, arg2: Float) => arg1 * arg2 },
      { case (arg1: Float, arg2: Int) => arg1 * arg2 })),
    "-" -> opToLambda(chain(
      { case (arg1: Int, arg2: Int) => arg1 - arg2 },
      { case (arg1: Int, arg2: Float) => arg1 - arg2 },
      { case (arg1: Float, arg2: Float) => arg1 - arg2 },
      { case (arg1: Float, arg2: Int) => arg1 - arg2 })),
    "/" -> opToLambda(chain(
      { case (arg1: Int, arg2: Int) => arg1 / arg2 },
      { case (arg1: Int, arg2: Float) => arg1 / arg2 },
      { case (arg1: Float, arg2: Float) => arg1 / arg2 },
      { case (arg1: Float, arg2: Int) => arg1 / arg2 }))))

  // Evaluate an expression.
  def eval(expr: Expr, env: Env): Expr = expr match {
    case s: Symbol => env.find(s)
    case ("q" | "quote") :: arg :: Nil => arg
    case ("q" | "quote") :: agrs => throw new IllegalArgumentException(s"Expected exactly one argument to quote, got: $args")
    case "atom" :: arg :: Nil => !eval(arg, env).isInstanceOf[List[Expr]]
    case "atom" :: _ => throw new IllegalArgumentException("Expected exactly one argument for 'atom'")
    case "eq?" :: expr1 :: expr2 :: Nil => {
      val v1 = eval(expr1, env)
      (!v1.isInstanceOf[List[Expr]] || v1.asInstanceOf[List[Expr]].isEmpty) && v1 == eval(expr2, env)
    }
    case "eq?" :: args => throw new IllegalArgumentException("Expected exactly two arguments for 'eq?', got: $args")
    case "car" :: arg :: tail => eval(arg, env) match {
      case head2 :: tail2 => head2
      case _ => throw new IllegalArgumentException(s"Illegal argument for 'car': $arg")
    }
    case "cdr" :: arg :: tail => eval(arg, env) match {
      case head2 :: tail2 => tail2
      case _ => throw new IllegalArgumentException(s"Illegal argument for 'cdr': $arg")
    }
    case "cons" :: arg1 :: arg2 :: Nil => eval(arg2, env) match {
      case l: List[Expr] => eval(arg1, env) :: l
      case arg => throw new IllegalArgumentException(s"Second argument to 'cons' should be list, was: '$arg'")
    }
    case "cons" :: args => throw new IllegalArgumentException(s"Invalid arguments to 'cons': $args")
    case "null?" :: head :: Nil => eval(head, env) == List()
    case "null?" :: _ => throw new IllegalArgumentException("Expected exactly one argument for 'null?'")
    case "if" :: test :: conseq :: alt :: Nil => if (eval(test, env) != false) eval(conseq, env) else eval(alt, env)
    case "if" :: args => throw new IllegalArgumentException(s"Invalid arguments given to 'if': $args")
    case "cond" :: exprs => {
      val conditionChecker = (expr: Expr) => expr match {
        case pred :: value :: Nil => eval(pred, env) == true
        case e => throw new IllegalArgumentException(s"Illegal argument given to 'cond': $e")
      }
      exprs.find(conditionChecker) match {
        case Some(pred :: value :: Nil) => eval(value, env)
        case Some(e) => throw new IllegalArgumentException(s"Illegal argument given to 'cond': $e")
        case None => throw new Exception("No matching clause in 'cond' expression")
      }
    }
    case "begin" :: exprs => exprs.map(eval(_, env)).last
    case "define" :: (symbol: Symbol) :: arg :: Nil => env.define(symbol, eval(arg, env))
    case "define" :: args => throw new IllegalArgumentException(s"Invalid arguments given to 'define': $args")
    case "set!" :: (symbol: Symbol) :: value :: Nil => env.set(symbol, value)
    case "set!" :: args => throw new IllegalArgumentException(s"Invalid arguments given to 'set!': $args")
    case "lambda" :: vars :: expr :: Nil => { case (args: Seq[Expr]) => eval(expr, new Env(Some(env), vars.asInstanceOf[List[String]], args)) }: Lambda
    case "lambda" :: args => throw new IllegalArgumentException(s"Invalid arguments given to 'lambda' : $args")
    case procedure :: args => {
      val evalArgs = args.map(eval(_, env))
      eval(procedure, env).asInstanceOf[Lambda].apply(evalArgs)
    }
    case value => value
  }

  // Convert list of tokens into a parse tree.
  def parse(tokens: Seq[String]): Seq[Expr] = {

    // Parse a list from the given sequence, returning this and the number of tokens consumed.
    def parseList(tokens: Seq[String], aggrCount: Int, aggrExprs: List[Expr]): (Int, List[Expr]) = tokens match {
      case Seq() => throw new IllegalArgumentException("Missing ')'")
      case ")" +: rest => (aggrCount + 1, aggrExprs.reverse)
      case "(" +: rest => {
        val (numTokens, parsedList) = parseList(rest, 1, List())
        parseList(tokens drop numTokens, aggrCount + numTokens, parsedList :: aggrExprs)
      }
      case token +: rest => parseList(rest, aggrCount + 1, atom(token) :: aggrExprs)
    }
    parseList(tokens :+ ")", 0, List())._2
  }

  // Split input up into a list of tokens.
  def tokenize(str: String) = str.replaceAll("[()]", " $0 ").split("\\s").filter(!_.isEmpty)

  // Convert a token to an atom, i.e. boolean, int, float or symbol.
  def atom(token: String): Expr =
    Try(token.toBoolean).getOrElse(
      Try(token.toInt).getOrElse(
        Try(token.toFloat).getOrElse(token)))

  // Get a stream of input strings.
  def inputs: Stream[String] = {
    print("scalisp> ")
    readLine() match {
      case null => Stream()
      case str => str #:: inputs
    }
  }

  // Turn an expression into a valid Lisp string.
  def toStr(expr: Expr): String = expr match {
    case List(args @ _*) => args.map(toStr(_)).mkString("(", " ", ")")
    case _ => expr.toString
  }

  // The main read-eval-print loop (REPL).
  def repl(globalEnv: Env): Unit = {
    for (input <- inputs)
      for (expr <- parse(tokenize(input)))
        println(toStr(Try(eval(expr, globalEnv)) match {
          case Success(v) => v
          case Failure(e) => e.getMessage
        }))
  }

  // Evaluate expressions from a file.
  def load(filename: String, env: Env) = {
    for (input <- Source.fromFile(filename).getLines())
      eval(parse(tokenize(input)), env)
  }

  val globalEnv = createGlobalEnv()

  for (arg <- args)
    load(args(0), globalEnv)

  repl(globalEnv)

}
