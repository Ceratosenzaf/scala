package expStepByStep

sealed trait Exp {
  def eval: Int
}
case class Add(a: Exp, b: Exp) extends Exp {
  def eval = a.eval + b.eval
  override def toString: String = s"($a + $b)"
}
case class Sub(a: Exp, b: Exp) extends Exp {
  def eval = a.eval - b.eval
  override def toString: String = s"($a - $b)"
}
case class Mul(a: Exp, b: Exp) extends Exp {
  def eval = a.eval * b.eval
  override def toString: String = s"($a * $b)"
}
case class Div(a: Exp, b: Exp) extends Exp {
  def eval = a.eval / b.eval
  override def toString: String = s"($a / $b)"
}
case class Num(v: Int) extends Exp {
  def eval = v
  override def toString: String = s"$v"
}

object ExpStepByStepParserInterpreter {
  def reduce(exp: Exp): Exp = exp match {
    case Num(v) => Num(v)
    case Add(Num(a), Num(b)) => Num(a + b)
    case Add(a, b) => Add(reduce(a), reduce(b))
    case Sub(Num(a), Num(b)) => Num(a - b)
    case Sub(a, b) => Sub(reduce(a), reduce(b))
    case Mul(Num(a), Num(b)) => Num(a * b)
    case Mul(a, b) => Mul(reduce(a), reduce(b))
    case Div(Num(a), Num(b)) => Num((a / b))
    case Div(a, b) => Div(reduce(a), reduce(b))
    case _ => throw new IllegalArgumentException
  }

  def printEval(exp: Exp): Int = {
    println(exp)
    exp match {
      case Num(v) => v
      case _ => printEval(reduce(exp))
    }
  }
}