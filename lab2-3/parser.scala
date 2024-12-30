package brainFuck

import scala.util.parsing.combinator._
import brainFuck._

class BrainFuckParser extends JavaTokenParsers {
  private val interpreter = new BrainFuckInterpreter

  def program: Parser[Unit] = rep(command) ^^ { interpreter.run }

  def command: Parser[Command] = (">" | "<" | "+" | "-" | "." | "," | loop ) ^^ {
    case ">" => IncrementPointer()
    case "<" => DecrementPointer()
    case "+" => IncrementData()
    case "-" => DecrementData()
    case "." => OutputData()
    case "," => InputData()
    case Loop(commands) => Loop(commands)
    case _ => throw new IllegalArgumentException
  }
  
  def loop: Parser[Loop] = "[" ~> rep(command) <~ "]" ^^ { Loop }
}