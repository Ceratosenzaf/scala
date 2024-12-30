package prettyPrint

import scala.util.parsing.combinator._
import prettyPrint.PrettyPrintTable

class PrettyPrintParser extends JavaTokenParsers {
  override def skipWhitespace = false
  val newline = sys.props("line.separator")

  def csv = rep1sep(row, newline) ^^ { rows => {
      val (header :: rest) = rows

      if(header.isEmpty)
        throw new IllegalArgumentException("Empty header")

      rest.foreach { row => 
        if (row.length != header.length)
          throw new IllegalArgumentException("Invalid row length")
      }

      new PrettyPrintTable(header, rest)
    }
  }

  def row: Parser[List[String]] = rep1sep(col, ",")

  def col: Parser[String] = opt(stringLiteral) ^^ {
    case Some(s) => s
    case None => ""
  }
}