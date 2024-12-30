package prettyPrint

import prettyPrint.PrettyPrintParser

object PrettyPrint {
  def main(args: Array[String]): Unit = {
    val p = new PrettyPrintParser

    val filename = if (args.length > 0) args(0) else "test.csv"
    val csv = scala.io.Source.fromFile(filename).mkString

    p.parseAll(p.csv, csv) match {
      case p.Success(res, _) => println(res)
      case e => println(e)
    }
  }
}