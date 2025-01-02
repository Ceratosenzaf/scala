package arnoldC

import arnoldC._

object ArnoldC {
  def main(args: Array[String]): Unit = {
    val program =
    """
    IT'S SHOWTIME
    HEY CHRISTMAS TREE isLessThan10
    YOU SET US UP 1
    HEY CHRISTMAS TREE n
    YOU SET US UP 0
    STICK AROUND isLessThan10 [
    GET TO THE CHOPPER n
    HERE IS MY INVITATION n
    GET UP 1
    ENOUGH TALK
    TALK TO THE HAND n
    GET TO THE CHOPPER isLessThan10
    HERE IS MY INVITATION 10
    LET OFF SOME STEAM BENNET n
    ENOUGH TALK
    ] CHILL
    YOU HAVE BEEN TERMINATED
    """
    val p = new ArnoldCParser

    p.parseAll(p.program, program) match {
      case p.Success(res, _) => println(res)
      case e => println(e)
    }
  }
}