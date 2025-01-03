package wtf

import scala.collection.mutable._

sealed trait Exp
case class Zero() extends Exp
case class Add() extends Exp
case class Sub() extends Exp
case class Print() extends Exp
case class IfThenElse(t: Block, f: Block) extends Exp
case class Block(body: List[Exp]) extends Exp

class WtfInterpreter {
  val stack = Stack[Int]()

  def exec(exp: Exp): Unit = exp match {
    case Block(l) => l.foreach(exec)
    case Zero() => stack.push(0)
    case Add() => stack.push(stack.pop + 1)
    case Sub() => stack.push(stack.pop - 1)
    case Print() => println(stack.pop)
    case IfThenElse(t,f) => if (stack.pop == 0) exec(t) else exec(f)
  }
}