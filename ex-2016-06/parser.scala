package arnoldC

import scala.util.parsing.combinator._
import arnoldC._

class ArnoldCParser extends JavaTokenParsers {
  val interpreter = new ArnoldCInterpreter

  def program: Parser[Unit] = "IT'S" ~> "SHOWTIME" ~> programBody <~ "YOU" <~ "HAVE" <~ "BEEN" <~ "TERMINATED" ^^ { interpreter.exec }
  def programBody: Parser[Block] = rep(exp) ^^ { Block }

  def exp: Parser[Exp] = printing | declaration | assignment | operation | ifThenElse | loop
  def operation: Parser[Op] = arithmeticOperation | logicalOperation
  def intOrVar: Parser[ExpWithValue] = int | variable

  def printing: Parser[Print] = "TALK" ~> "TO" ~> "THE" ~> "HAND" ~> intOrVar ^^ { Print }

  def declaration: Parser[Declare] = ("HEY" ~> "CHRISTMAS" ~> "TREE" ~> variable) ~ ("YOU" ~> "SET" ~> "US" ~> "UP" ~> intOrVar) ^^ {
    case x~v => Declare(x, v)
  }
  
  def assignment: Parser[Set] = ("GET" ~> "TO" ~> "THE" ~> "CHOPPER" ~> variable) ~ ("HERE" ~> "IS" ~> "MY" ~> "INVITATION" ~> intOrVar) ~ (opt(operation) <~ "ENOUGH" <~ "TALK") ^^ {
    case x~init~rest => Set(x, init, rest.getOrElse(Add(Value(0))))
  }
  
  def arithmeticOperation: Parser[Op] = add | sub | mul | div
  def add: Parser[Add] = "GET" ~> "UP" ~> intOrVar ^^ { Add }
  def sub: Parser[Sub] = "GET" ~> "DOWN" ~> intOrVar ^^ { Sub }
  def mul: Parser[Mul] = "YOU'RE" ~> "FIRED" ~> intOrVar ^^ { Mul }
  def div: Parser[Div] = "HE" ~> "HAD" ~> "TO" ~> "SPLIT" ~> intOrVar ^^ { Div }
  
  def logicalOperation: Parser[Op] = eq | gr8 | and | or
  def eq: Parser[Eq] = "YOU" ~> "ARE" ~> "NOT" ~> "YOU" ~> "YOU" ~> "ARE" ~> "ME" ~> intOrVar ^^ { Eq }
  def gr8: Parser[Gr8] = "LET" ~> "OFF" ~> "SOME" ~> "STEAM" ~> "BENNET" ~> intOrVar ^^ { Gr8 }
  def and: Parser[And] = "KNOCK" ~> "KNOCK" ~> intOrVar ^^ { And }
  def or: Parser[Or] = "CONSIDER" ~> "THAT" ~> "A" ~> "DIVORCE" ~> intOrVar ^^ { Or }
  
  def ifThenElse: Parser[IfThenElse] = ("BECAUSE" ~> "I'M" ~> "GOING" ~> "TO" ~> "SAY" ~> "PLEASE" ~> variable) ~ programBody ~ ("BULLSHIT" ~> programBody <~ "YOU" <~ "HAVE" <~ "NO" <~ "RESPECT" <~ "FOR" <~ "LOGIC") ^^ {
    case x~t~f => IfThenElse(x, t, f)
  }
  
  def loop: Parser[Loop] = ("STICK" ~> "AROUND" ~> variable <~ "[") ~ (programBody <~ "]" <~ "CHILL") ^^ {
    case x ~ body => Loop(x, body)
  }

  def variable: Parser[Variable] = ident ^^ { Variable }
  def int: Parser[Value] = wholeNumber ^^ { x => Value(x.toInt) }
}