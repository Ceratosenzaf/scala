package wtf

import scala.collection.mutable.{Stack, Map}

sealed trait Exp
case class Zero() extends Exp
case class Add() extends Exp
case class Sub() extends Exp
case class Print() extends Exp
case class IfThenElse(t: Block, f: Block) extends Exp
case class Block(body: List[Exp]) extends Exp
case class FnDef(name: String, params: Int, body: Block) extends Exp
case class FnCall(name: String) extends Exp
case class FnParam(number: Int) extends Exp

class WtfInterpreter(params: Seq[Int]) {
  val stack = Stack[Int]()
  val functions = Map[String, FnDef]()

  def exec(exp: Exp): Unit = exp match {
    case Block(l) => l.foreach(exec)
    case Zero() => stack.push(0)
    case Add() => stack.push(stack.pop + 1)
    case Sub() => stack.push(stack.pop - 1)
    case Print() => println(stack.pop)
    case IfThenElse(t,f) => if (stack.pop == 0) exec(t) else exec(f)
    case FnDef(n,p,b) => functions(n) = FnDef(n,p,b)
    case FnParam(n) => stack.push(params(params.length - n))
    case FnCall(n) => {
      functions(n) match {
        case FnDef(n, p, b) => {
          val fnParams = for (x <- 0 until p) yield stack.pop
          val int = new WtfInterpreter(fnParams)
          int.exec(b)
        }
      }
    }
  }
}