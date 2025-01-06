package onYourDesk

import scala.util.parsing.combinator._
import scala.collection.mutable._

class OnYourDeskParser extends JavaTokenParsers {
  val m = Map[String, Int]()

  def program = printSection <~ whereSection ^^ { l => {
    val values = l.map { 
      case (v: Int) => v
      case (x: String) => m(x)
      case v => throw new IllegalArgumentException(s"unsupported value: $v")
    }

    val res = values.fold(0)(_ + _)
    (res, m)
  }}

  def printSection = "print" ~> repsep(varOrInt, "+")
  def varOrInt = variable | int

  def whereSection = "where" ~> repsep(varInit, ",")
  def varInit = variable ~ "=" ~ int ^^ { case x~_~v => m(x) = v }

  def variable = "[a-z]".r
  def int = wholeNumber ^^ { _.toInt }
}