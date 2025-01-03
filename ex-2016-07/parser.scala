package wtf

import scala.util.parsing.combinator._
import wtf._

class WtfParser extends JavaTokenParsers {
  val interpreter = new WtfInterpreter

  def program = fnBlock ~ codeBlockWithoutBrackets ^^ { case f~c => interpreter.exec(f,c) }

  def fnBlock: Parser[Block] = rep(fnDef) ^^ { Block }

  def fnDef: Parser[FnDef] = ("def" ~> fnName) ~ (int <~ "=") ~ codeBlockWithBrackets ^^ {
    case n~p~b => FnDef(n,p,b)
  }

  def codeBlock: Parser[Block] = codeBlockWithBrackets | codeBlockWithoutBrackets
  def codeBlockWithBrackets: Parser[Block] = "[" ~> rep(exp) <~ "]" ^^ { Block }
  def codeBlockWithoutBrackets: Parser[Block] = rep(exp) ^^ { Block }

  def exp: Parser[Exp] = op | value | print | ifThenElse | fnCall | fnParam

  def fnParam: Parser[FnParam] = "$" ~> int ^^ { FnParam }
  def fnCall: Parser[FnCall] = fnName ^^ { FnCall }

  def print: Parser[Print] = "!" ^^ { _ => Print() }

  def ifThenElse: Parser[IfThenElse] = ("?" ~> codeBlockWithBrackets) ~ (":" ~> codeBlockWithBrackets) ^^ {
    case t~f => IfThenElse(t,f)
  }

  def op: Parser[Exp] = plus | minus
  def plus: Parser[Add] = "+" ^^ { _ => Add() }
  def minus: Parser[Sub] = "-" ^^ { _ => Sub() }

  def value: Parser[Zero] = "0" ^^ { _ => Zero() }
  def int: Parser[Int] = wholeNumber ^^ { _.toInt }
  def fnName: Parser[String] = "[A-Z]".r
}