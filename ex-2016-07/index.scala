package wtf

object Wtf {
  def main(args: Array[String]): Unit = {
    args.foreach { fileName => {
      val src = scala.io.Source.fromFile(fileName)
      val program = src.mkString
      val p = new WtfParser
      
      p.parseAll(p.program, program) match {
        case p.Success(res, _) => println(res)
        case e => println(e)
      }

      src.close()
    }}
  }
}