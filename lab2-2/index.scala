package lab2_2

import lab2_2._

object Lab2_2 {
  def main(args: Array[String]): Unit = {
   
    val parser = new ExprParser

    val exprCalculator = 
      """
      B = 1 + 2 * 3,
      A = B,
      C = D = 2 * A,
      E = A * 2,
      F = B + C + D + E,
      G = sqrt A + 1,
      G ^ 2
      """

    parser.parseAll(parser.calculator, exprCalculator) match {
      case parser.Success((l,v), _) => println(s"Results line by line: $l\nVariables: $v")
      case e => println(e)
    }
  }
}