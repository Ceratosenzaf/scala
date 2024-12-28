package lab2_1

import lab2_1.Expr

class calculator {
  private val variables = scala.collection.mutable.Map[String, Expr]()

  def apply(expressions: calculator => Expr): Expr = expressions(this)

  def apply(variable: String): Expr = {
    variables.getOrElse(variable, throw new RuntimeException(s"Variable $variable is not initialized"))
  }

  def update(variable: String, value: Expr): Unit = {
    variables(variable) = value
  }

  def eval(expression: => Expr): Expr = expression
}

object calculator {
  def apply(expressions: calculator => Expr): Double = {
    expressions(new calculator).value
  }
}