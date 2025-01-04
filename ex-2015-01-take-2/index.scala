package prettyPrint

object PrettyPrintEvaluator {
  def main(args: Array[String]): Unit =
    args.foreach { filename => {
      val src = scala.io.Source.fromFile(filename)
      val csv = src.mkString
      val p = new PrettyPrintParser

      p.parseAll(p.csv, csv) match {
        case p.Success(res, _) => println(res)
        case e => println(e)
      }

      src.close()
    }
  }
}