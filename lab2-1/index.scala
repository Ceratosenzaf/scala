package lab2_1

import lab2_1.calculator
import lab2_1.Expr._

object Lab2_1 {
  def main(args: Array[String]): Unit = {
    val result = calculator { x =>
      x("B") = 1 + 2 * 3
      x("A") = x("B")
      x("A") + x("B")
    }
    println(result)
  }
}