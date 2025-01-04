package lab2_2

import scala.util.parsing.combinator._
import scala.collection.mutable.{Map, Stack}
import lab2_2.Expr

class ExprParser extends JavaTokenParsers {
  val variables = Map[String, Expr]()
  val stack = new Stack[Expr]()
  val newLine = sys.props("line.separator")

  def calculator = rep(rawExprLine) ^^ { lines => (lines, variables) }

  def rawExprLine = s""".*$newLine""".r ^^ { x =>
    parseAll(exprLine, x.replace(newLine, "")) match {
      case Success(result, _) => result
      case failure : NoSuccess => throw new Exception(failure.msg)
    }
  }

  def exprLine: Parser[Expr] = rep(expr) ^^ { _.last }
  def expr: Parser[Expr] = declaration | evaluation | value
  
  def declaration: Parser[Expr] = ident <~ "=" <~ exprLine ^^ { x  => {
      variables(x) = stack.top
      stack.top
    }
  }

  def evaluation: Parser[Expr] = opDouble | opSingle | variable
  def opDouble: Parser[Expr] = add | mult | rem | div | pow
  def add: Parser[Expr] = "+" ~> expr ^^ { _ => stack.push(stack.pop + stack.pop).top }
  def mult: Parser[Expr] = "*" ~> expr ^^ { _ => stack.push(stack.pop * stack.pop).top }
  def rem: Parser[Expr] = "-" ~> expr ^^ { _ => stack.push(stack.pop - stack.pop).top }
  def div: Parser[Expr] = "/" ~> expr ^^ { _ => stack.push(stack.pop / stack.pop).top }
  def pow: Parser[Expr] = "^" ~> expr ^^ { _ => stack.push(stack.pop ^ stack.pop).top }
  def opSingle: Parser[Expr] = sqrt | sin | cos | tan
  def sqrt: Parser[Expr] = "sqrt" ~> expr ^^ { _ => stack.push(stack.pop.sqrt).top }
  def sin: Parser[Expr] = "sin" ~> expr ^^ { _ => stack.push(stack.pop.sin).top }
  def cos: Parser[Expr] = "cos" ~> expr ^^ { _ => stack.push(stack.pop.cos).top }
  def tan: Parser[Expr] = "tan" ~> expr ^^ { _ => stack.push(stack.pop.tan).top }
  
  def variable: Parser[Expr] = ident <~ not("=") ^^ { x => stack.push(variables(x)).top }
 
  def value: Parser[Expr] = int | float
  def int: Parser[Expr] = wholeNumber ^^ { x => stack.push(Expr(x.toInt)).top }
  def float: Parser[Expr] = floatingPointNumber ^^ { x => stack.push(Expr(x.toDouble)).top }
}