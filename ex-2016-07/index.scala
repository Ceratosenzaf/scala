package wtf

object Wtf {
  def main(args: Array[String]): Unit = {
    val program = 
    """
    0--!
    0 ? [0++!] : [0--!]
    0++!
    """
    val p = new WtfParser

    p.parseAll(p.program, program) match {
      case p.Success(res, _) => println(res)
      case e => println(e)
    }

  }
}