package wtf

import scala.util.parsing.combinator._
import scala.collection.mutable._

class WTFParser(funs: Map[String, (Int, String)], params: List[Int]) extends JavaTokenParsers {
  val stack = new Stack[Int]()
  val newLine = sys.props("line.separator")

  def this() = this(new HashMap[String, (Int, String)](), List())
  
  def program: Parser[Option[Int]] = fnSection ~ dataSection ^^ { _ => if (!stack.isEmpty) Some(stack.pop()) else None }

  def fnSection = rep(fnDefinition)
  def fnDefinition = ("def" ~> fnName) ~ int ~ ("=" ~> s".*$newLine".r) ^^ {
    case name~args~body => funs(name) = (args, body.replace(newLine, ""))
  }

  def dataSection = rep(expr)
  
  def expr = zero | sum | sub | print | ifThenElse | fnParam | fnCall
  
  def zero = "0" ^^ { _ => stack.push(0) }
  def sum = "+" ^^ { _ => stack.push(stack.pop() + 1) }
  def sub = "-" ^^ { _ => stack.push(stack.pop() - 1) }
  def print = "!" ^^ { _ => println(stack.pop()); stack }
  
  def block = """\[.*?\]""".r ^^ { _.tail.init }
  def ifThenElse = ("?" ~> block) ~ (":" ~> block) ^^ { 
    case t~f => parseAll(program, if (stack.pop() == 0) t else f) match {
      case Success(Some(x: Int), _) => stack.push(x)
      case Success(None, _) => stack
    }
  }

  def fnParam = "$" ~> int ^^ { i => stack.push(params(i-1)) }
  def fnCall = fnName ^^ { name => {
    val fn = funs(name)
    val fnParams = (for (i <- 0 until fn._1) yield stack.pop()).reverse.toList
    val fnParser = new WTFParser(funs, fnParams)

    fnParser.parseAll(fnParser.program, fn._2) match {
      case fnParser.Success(Some(x: Int), _) => stack.push(x)
      case fnParser.Success(None, _) => stack
    }
  }}

  def fnName = "[A-Z]".r ^^ { _.toString }
  def int = wholeNumber ^^ { _.toInt }
}
