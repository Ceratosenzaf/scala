package wtf

import scala.util.parsing.combinator._
import wtf._

class WtfParser extends JavaTokenParsers {
  val interpreter = new WtfInterpreter(Seq[Int]())

  def program = fnBlock ~ codeBlock ^^ { case f~c => interpreter.exec(Block(List(f,c))) }

  def fnBlock: Parser[Block] = rep(fnDef) ^^ { Block }

  def fnDef: Parser[FnDef] = ("def" ~> fnName) ~ (int <~ "=") ~ codeBlock ^^ {
    case n~p~b => FnDef(n,p,b)
  }

  def codeBlock: Parser[Block] = (("[" ~> rep(exp) <~ "]") | rep(exp)) ^^ { Block }

  def exp: Parser[Exp] = op | value | print | ifThenElse | fnCall | fnParam

  def fnParam: Parser[FnParam] = "$" ~> int ^^ { FnParam }
  def fnCall: Parser[FnCall] = fnName ^^ { FnCall }

  def print: Parser[Print] = "!" ^^ { _ => Print() }

  def ifThenElse: Parser[IfThenElse] = ("?" ~> codeBlock) ~ (":" ~> codeBlock) ^^ {
    case t~f => IfThenElse(t,f)
  }

  def op: Parser[Exp] = plus | minus
  def plus: Parser[Add] = "+" ^^ { _ => Add() }
  def minus: Parser[Sub] = "-" ^^ { _ => Sub() }

  def value: Parser[Zero] = "0" ^^ { _ => Zero() }
  def int: Parser[Int] = wholeNumber ^^ { _.toInt }
  def fnName: Parser[String] = "[A-Z]".r
}