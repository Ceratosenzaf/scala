package arnoldC

sealed trait Exp {
  implicit def int2Value(i: Int): Value = Value(i)
}
sealed trait ExpWithValue extends Exp {
  def value: Int
}
sealed trait Op extends Exp {
  val b: ExpWithValue
  def exec(a: ExpWithValue): Value
  def copy(b: ExpWithValue): Op
}

case class Block(body: List[Exp]) extends Exp {
  override def toString = s"{\n${body.mkString("\n")}\n}"
}
case class Print(exp: ExpWithValue) extends Exp {
  override def toString = s"println($exp)"
}
case class Declare(name: Variable, value: ExpWithValue) extends Exp {
  override def toString = s"declare $name = $value"
}
case class Set(name: Variable, init: ExpWithValue, rest: Op) extends Exp {
  override def toString = s"set $name = $init $rest"
}
case class Add(b: ExpWithValue) extends Op {
  def exec(a: ExpWithValue) = a.value + b.value
  def copy(b: ExpWithValue) = Add(b)
  override def toString = s"+ $b"
}
case class Sub(b: ExpWithValue) extends Op {
  def exec(a: ExpWithValue) = a.value - b.value
  def copy(b: ExpWithValue) = Sub(b)
  override def toString = s"- $b"
}
case class Mul(b: ExpWithValue) extends Op {
  def exec(a: ExpWithValue) = a.value * b.value
  def copy(b: ExpWithValue) = Mul(b)
  override def toString = s"* $b"
}
case class Div(b: ExpWithValue) extends Op {
  def exec(a: ExpWithValue) = a.value / b.value
  def copy(b: ExpWithValue) = Div(b)
  override def toString = s"/ $b"
}
case class Eq(b: ExpWithValue) extends Op {
  def exec(a: ExpWithValue) = if (a.value == b.value) 1 else 0
  def copy(b: ExpWithValue) = Eq(b)
  override def toString = s"== $b"
}
case class Gr8(b: ExpWithValue) extends Op {
  def exec(a: ExpWithValue) = if (a.value > b.value) 1 else 0
  def copy(b: ExpWithValue) = Gr8(b)
  override def toString = s"> $b"
}
case class And(b: ExpWithValue) extends Op {
  def exec(a: ExpWithValue) = if (a.value != 0 && b.value != 0) 1 else 0
  def copy(b: ExpWithValue) = And(b)
  override def toString = s"&& $b"
}
case class Or(b: ExpWithValue) extends Op {
  def exec(a: ExpWithValue) = if (a.value != 0 || b.value != 0) 1 else 0
  def copy(b: ExpWithValue) = Or(b)
  override def toString = s"|| $b"
}
case class IfThenElse(cond: ExpWithValue, t: Block, f: Block) extends Exp {
  override def toString = s"if $cond then $t else $f"
}
case class Loop(cond: ExpWithValue, body: Block) extends Exp {
  override def toString = s"while $cond $body"
}
case class Variable(name: String) extends ExpWithValue {
  var value = 0
  def setValue(value: Int) = { this.value = value; this }
  override def toString = s"$name = $value"
}
case class Value(value: Int) extends ExpWithValue {
  override def toString = s"$value"
}

class ArnoldCInterpreter {
  val m = scala.collection.mutable.Map[String, Variable]()

  def getValue(exp: ExpWithValue): Int = exp match {
    case v: Value => v.value
    case v: Variable => m(v.name).value
  }

  def exec(exp: Exp): Unit = exp match {
    case Print(exp) => println(getValue(exp))
    case Declare(name, value) => m(name.name) = name.setValue(getValue(value))
    case Set(name, init, rest) => {
      val updatedInit = Value(getValue(init))
      val updatedOp = rest.copy(Value(getValue(rest.b)))
      val newValue = updatedOp.exec(updatedInit)
      m(name.name).setValue(newValue.value)
    }
    case IfThenElse(cond, t, f) => if (getValue(cond) != 0) exec(t) else exec(f)
    case Loop(cond, body) => while (getValue(cond) != 0) exec(body)
    case Block(body) => body.foreach(exec)
    case _ => throw new UnsupportedOperationException(s"exec($exp)")
  }
}