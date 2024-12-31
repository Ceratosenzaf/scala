package yourDesk

import scala.util.parsing.combinator._
import scala.collection.mutable.Map

class YourDeskParser extends JavaTokenParsers {
  val m = Map[String,Int]()

  def program = printSection ~ whereSection ^^ {
    case p ~ w => {
      val replaced_values: List[Int] = p.map {
        case x: String => m(x)
        case v: Int => v
        case _ => throw new IllegalArgumentException
      }

      replaced_values.foldLeft(0)(_+_)
    }
  }

  def printSection = "print" ~> printExp
  def printExp = rep1sep(var_or_num, "+")

  def whereSection = "where" ~> whereExp
  def whereExp = rep1sep(var_declaration, ",") 

  def var_declaration = variable ~ "=" ~ number ^^ {
    case x ~ "=" ~ v => m(x) = v
  }

  def var_or_num = variable | number
  def variable = "[a-z]".r
  def number = wholeNumber ^^ { n => n.toInt } 
}