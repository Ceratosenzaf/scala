package wtf

import wtf._

object WTFEvaluator {
  def main(args: Array[String]): Unit = args.foreach { fileName => {
    val src = scala.io.Source.fromFile(fileName)
    val program = src.mkString

    val p = new WTFParser()

    p.parseAll(p.program, program) match {
      case p.Success(res, _) => println(res)
      case e => println(e)
    }

    src.close()
  }}
}