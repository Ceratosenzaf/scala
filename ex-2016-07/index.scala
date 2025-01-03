package wtf

object Wtf {
  def main(args: Array[String]): Unit = {
    val program = 
    """
    def H 1 = [ $1 ! ]
    def S 2 = [ $2 ? [$1] : [$1+$2-S] ] // TODO: handle recursion

    0--!
    0 ? [0++!] : [0--!]
    0++ H
    0++++++ 0+++ S!
    """
    val p = new WtfParser

    p.parseAll(p.program, program) match {
      case p.Success(res, _) => println(res)
      case e => println(e)
    }

  }
}