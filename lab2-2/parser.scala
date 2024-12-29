package lab2_2

import scala.util.parsing.combinator._
import scala.collection.mutable.Map
import lab2_2.Expr

class ExprParser extends JavaTokenParsers {
  var variables: Map[String, Expr] = Map()

  def calculator: Parser[List[Expr]] = repsep(expr, ",") ^^ { items => {
    println("Variables: " + variables)
    println("Items: " + items)
    items
  } }
  def expr: Parser[Expr] = declaration | evaluation | value
  
  def declaration: Parser[Expr] = var_declaration ~ expr ^^ {
    case x ~ v => {
      variables(x) = v
      v
    }
  }
  def var_declaration: Parser[String] = stringLiteral <~ "="

  def evaluation: Parser[Expr] = opDouble | opSingle | variable
  def opDouble: Parser[Expr] = add | mult | rem | div | pow
  def add: Parser[Expr] = expr ~ "+" ~ expr ^^ { case a ~ op ~ b => a + b }
  def mult: Parser[Expr] = expr ~ "*" ~ expr ^^ { case a ~ op ~ b => a * b }
  def rem: Parser[Expr] = expr ~ "-" ~ expr ^^ { case a ~ op ~ b => a - b }
  def div: Parser[Expr] = expr ~ "/" ~ expr ^^ { case a ~ op ~ b => a / b }
  def pow: Parser[Expr] = expr ~ "^" ~ expr ^^ { case a ~ op ~ b => a ^ b }
  def opSingle: Parser[Expr] = sqrt | sin | cos | tan
  def sqrt: Parser[Expr] = "sqrt" ~> expr ^^ { _.sqrt }
  def sin: Parser[Expr] = "sin" ~> expr ^^ { _.sin }
  def cos: Parser[Expr] = "cos" ~> expr ^^ { _.cos }
  def tan: Parser[Expr] = "tan" ~> expr ^^ { _.tan }
  def variable: Parser[Expr] = stringLiteral ^^ { variables(_) }
 
  def value: Parser[Expr] = dec | float
  def dec: Parser[Expr] = wholeNumber ^^ { x => Expr(x.toInt) }
  def float: Parser[Expr] = floatingPointNumber ^^ { x => Expr(x.toDouble) }
}