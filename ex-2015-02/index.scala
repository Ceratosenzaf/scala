package yourDesk

import yourDesk._

object YourDesk {
  def main(args: Array[String]): Unit = {
    val program = "print x+y+z+1+x+-3 where x = 25, y = 1, z=-7"
    val p = new YourDeskParser

    p.parseAll(p.program, program) match {
      case p.Success(res, _) => println(res)
      case e => println(e)
    }
  }
}