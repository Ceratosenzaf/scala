package stepByStep

sealed trait Expr
case class Num(v: Int) extends Expr {
  override def toString = s"$v"
}
case class Sum(a: Expr, b: Expr) extends Expr {
  override def toString = s"($a + $b)"
}
case class Sub(a: Expr, b: Expr) extends Expr {
  override def toString = s"($a - $b)"
}
case class Mul(a: Expr, b: Expr) extends Expr {
  override def toString = s"($a * $b)"
}
case class Div(a: Expr, b: Expr) extends Expr {
  override def toString = s"($a / $b)"
}

object StepByStepInterpreter {
  def reduceByOne(e: Expr): Expr = e match {
    case Num(v) => Num(v)
    case Sum(Num(a), Num(b)) => Num(a + b)
    case Sub(Num(a), Num(b)) => Num(a - b)
    case Mul(Num(a), Num(b)) => Num(a * b)
    case Div(Num(a), Num(b)) => Num(a / b)
    case Sum(a, b) => Sum(reduceByOne(a), reduceByOne(b))
    case Sub(a, b) => Sub(reduceByOne(a), reduceByOne(b))
    case Mul(a, b) => Mul(reduceByOne(a), reduceByOne(b))
    case Div(a, b) => Div(reduceByOne(a), reduceByOne(b))
  }

  def printAndEvaluate(e: Expr): Int = {
    println(e)
    reduceByOne(e) match {
      case Num(v) => v
      case reduced => printAndEvaluate(reduced)
    }
  }
}