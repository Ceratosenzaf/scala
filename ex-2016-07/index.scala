package wtf

object Wtf {
  def main(args: Array[String]): Unit = {
    val program = 
    """
    def H 1 = [$1 !]
    def S 2 = [$2 ? [$1] : [$1+$2-S]]
    def M 2 = [$2 ? [$1] : [$1-$2-M]]
    def A 3 = [$2 ? [$1] : [$1 $3 S $2- $3 A]]
    def P 2 = [0 $2 $1 A]
    def X 2 = [$2 ? [$1] : [$1 $2 P $2- X]]
    def F 1 = [$1 $1- X]
    0+++++++!
    0 H
    0+++ H
    0- ? [0+!] : [0-!]
    0++++++ 0+++ S!
    0++++++ 0+++ M!
    0+++ 0++++++ M!
    0+ 0++++++++++ M 0++++S!
    0++ 0++++ S 0++++ S!
    0+++++ 0+++++++ P!
    0++++ 0++ M 0+++++++ P!
    0++ 0++++ P 0+++++ P!
    0+++++F!
    """
    val p = new WtfParser

    p.parseAll(p.program, program) match {
      case p.Success(res, _) => println(res)
      case e => println(e)
    }

  }
}