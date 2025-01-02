package expStepByStep

import scala.util.parsing.combinator._
import expStepByStep._

class ExpStepByStepParser extends JavaTokenParsers {
  def exp: Parser[Exp] = ("(" ~> expBody <~ ")") | int
  def expBody: Parser[Exp] = add | sub | mul | div 
  
  def add: Parser[Add] = exp ~ "+" ~ exp ^^ { case a ~ _ ~ b => Add(a,b) }
  def sub: Parser[Sub] = exp ~ "-" ~ exp ^^ { case a ~ _ ~ b => Sub(a,b) }
  def mul: Parser[Mul] = exp ~ "*" ~ exp ^^ { case a ~ _ ~ b => Mul(a,b) }
  def div: Parser[Div] = exp ~ "/" ~ exp ^^ { case a ~ _ ~ b => Div(a,b) }

  def int: Parser[Num] = wholeNumber ^^ { v => Num(v.toInt) }
}