package lab2_2

import lab2_2.ExprParser

object Lab2_2 {
  def main(args: Array[String]): Unit = {
   
    val parser = new ExprParser

    val exprCalculator = 
      """
      "B" = 1 + 2 * 3,
      "A" = "B",
      "A" + "B"
      """

    parser.parseAll(parser.calculator, exprCalculator) match {
      case parser.Success(result, _) => println(result)
      case parser.Failure(msg, _) => println("Failure: " + msg)
      case parser.Error(msg, _) => println("Error: " + msg)
    }
  }
}