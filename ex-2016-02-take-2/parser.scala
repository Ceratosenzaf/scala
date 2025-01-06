package stepByStep

import scala.util.parsing.combinator._
import stepByStep._

class StepByStepParser extends JavaTokenParsers {
  def expr: Parser[Int] = e ^^ { StepByStepInterpreter.printAndEvaluate }
  def e: Parser[Expr] = sum | sub | mul | div | num

  def sum: Parser[Sum] = "(" ~> e ~ "+" ~ e <~ ")" ^^ { case a~_~b => Sum(a,b) }
  def sub: Parser[Sub] = "(" ~> e ~ "-" ~ e <~ ")" ^^ { case a~_~b => Sub(a,b) }
  def mul: Parser[Mul] = "(" ~> e ~ "*" ~ e <~ ")" ^^ { case a~_~b => Mul(a,b) }
  def div: Parser[Div] = "(" ~> e ~ "/" ~ e <~ ")" ^^ { case a~_~b => Div(a,b) }

  def num: Parser[Num] = wholeNumber ^^ { v => Num(v.toInt) }
}