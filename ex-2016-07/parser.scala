package wtf

import scala.util.parsing.combinator._
import wtf._

class WtfParser extends JavaTokenParsers {
  val interpreter = new WtfInterpreter

  def program = block ^^ { interpreter.exec }

  def block: Parser[Block] = rep(exp) ^^ { Block }

  def exp: Parser[Exp] = op | value | print | ifThenElse

  def print: Parser[Print] = "!" ^^ { _ => Print() }

  def ifThenElse: Parser[IfThenElse] = ("?" ~> "[" ~> block <~ "]") ~ (":" ~> "[" ~> block <~ "]") ^^ {
    case t~f => IfThenElse(t,f)
  }

  def op: Parser[Exp] = plus | minus
  def plus: Parser[Add] = "+" ^^ { _ => Add() }
  def minus: Parser[Sub] = "-" ^^ { _ => Sub() }

  def value: Parser[Zero] = "0" ^^ { _ => Zero() }
}