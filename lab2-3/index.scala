package brainFuck

import brainFuck.BrainFuckParser

object BrainFuck {
  def main(args: Array[String]) = {
    val parser = new BrainFuckParser
    val program = 
      """
      ++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
      """
    
    parser.parseAll(parser.program, program) match {
      case parser.Success(res, _) => println("completed!")
      case e => println(s"halted: $e")
    }
  }
}