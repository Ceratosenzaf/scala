package prettyPrint

import scala.util.parsing.combinator._

class PrettyPrintParser extends JavaTokenParsers {
  val lineBreak = sys.props("line.separator")
  
  override def skipWhitespace = false

  def csv = rawLine ~ rep(rawLine) ^^ { case h~b => new PrettyPrintTable(h,b) }

  def rawLine = s""".*$lineBreak""".r ^^ { x => 
    parseAll(line, x.replace(lineBreak, "")) match {
      case Success(res, _) => res
    }
  }

  def line = repsep(col, ",")

  def col = string | text | empty
  def string = stringLiteral ^^ { s => s.substring(1, s.length - 1) }
  def text = """[^,]*""".r
  def empty = ""
}

class PrettyPrintTable(header: List[String], body: List[List[String]]) {
  val newLine = sys.props("line.separator")
  val table = header :: body

  def getColMaxWidth(i: Int): Int = table.map(_(i)).map(_.length).max
  def pad2Len(s: String, l: Int) = f"%%-${l}s" format s
  def formatRow(r: List[String]): String = r.zipWithIndex.map { case (c,i) => pad2Len(c, getColMaxWidth(i)) } mkString("| ", " | ", " |")

  override def toString: String = {
    val h = formatRow(header)
    val b = body.map(formatRow).mkString(newLine)
    val div = "-" * h.length

    val t = if (body.length > 0) List(div,h,div,b,div) else List(div,h,div)
    t.mkString(newLine)
  } 
}