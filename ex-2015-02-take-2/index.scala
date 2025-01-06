package onYourDesk

import onYourDesk._

object OnYourDeskEvaluator {
  def main(args: Array[String]): Unit = 
    args.foreach { fileName => {
      val src = scala.io.Source.fromFile(fileName)
      val program = src.mkString
      val p = new OnYourDeskParser

      p.parseAll(p.program, program) match {
        case p.Success((res, map), _) => { println(res); println(map) }
        case e => println(e)
      }

      src.close()
    }}
}