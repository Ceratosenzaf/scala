package lab2_1

class Expr(val value: Double) {
  def +(n: Expr): Expr = new Expr(value + n.value)
  def -(n: Expr): Expr = new Expr(value - n.value)
  def *(n: Expr): Expr = new Expr(value * n.value)
  def /(n: Expr): Expr = new Expr(value / n.value)
  def ^(n: Expr): Expr = new Expr(Math.pow(value, n.value))
  def sqrt: Expr = new Expr(Math.sqrt(value))
  def sin: Expr = new Expr(Math.sin(value))
  def cos: Expr = new Expr(Math.cos(value))
  def tan: Expr = new Expr(Math.tan(value))
}

object Expr {
  def apply(v: Double) = new Expr(v)
  def unapply(n: Expr) = Some(n.value)
  
  implicit def intToExpr(v: Int): Expr = Expr(v.toDouble)
  implicit def doubleToExpr(v: Double): Expr = Expr(v)
}
