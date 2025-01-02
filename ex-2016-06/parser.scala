package arnoldC

import scala.util.parsing.combinator._

class ArnoldCParser extends JavaTokenParsers {
  def program = "IT'S" ~> "SHOWTIME" ~> programBody <~ "YOU" <~ "HAVE" <~ "BEEN" <~ "TERMINATED" ^^ { res => println(s"program $res"); res }
  def programBody = rep(exp) ^^ { x => val res = s"body { ${x.mkString("; ")} }"; println(res); res }

  def exp: Parser[Any] = printing | declaration | assignment | operation | ifThenElse | loop ^^ { res => println(res); res }
  def operation = arithmeticOperation | logicalOperation
  def opOrIntOrVar = operation | int | variable

  def printing = "TALK" ~> "TO" ~> "THE" ~> "HAND" ~> opOrIntOrVar ^^ {
    case x => val res = s"print $x"; println(res); res
  }

  def declaration = ("HEY" ~> "CHRISTMAS" ~> "TREE" ~> variable) ~ ("YOU" ~> "SET" ~> "US" ~> "UP" ~> int) ^^ {
    case x~v => val res = s"declare $x = $v"; println(res); res
  }
  
  def assignment = ("GET" ~> "TO" ~> "THE" ~> "CHOPPER" ~> variable) ~ ("HERE" ~> "IS" ~> "MY" ~> "INVITATION" ~> opOrIntOrVar) ~ ( opOrIntOrVar <~ "ENOUGH" <~ "TALK") ^^ {
    case x~init~rest => val res = s"set $x = $init $rest"; println(res); res
  }
  
  def arithmeticOperation = add | sub | mul | div
  def add = "GET" ~> "UP" ~> opOrIntOrVar ^^ { b => val res = s" + $b"; println(res); res }
  def sub = "GET" ~> "DOWN" ~> opOrIntOrVar ^^ { b => val res = s" - $b"; println(res); res }
  def mul = "YOU'RE" ~> "FIRED" ~> opOrIntOrVar ^^ { b => val res = s" * $b"; println(res); res }
  def div = "HE" ~> "HAD" ~> "TO" ~> "SPLIT" ~> opOrIntOrVar ^^ { b => val res = s" / $b"; println(res); res }
  
  def logicalOperation = eq | gr8 | and | or
  def eq = "YOU" ~> "ARE" ~> "NOT" ~> "YOU" ~> "YOU" ~> "ARE" ~> "ME" ~> opOrIntOrVar ^^ { b => val res = s" == $b"; println(res); res }
  def gr8 = "LET" ~> "OFF" ~> "SOME" ~> "STEAM" ~> "BENNET" ~> opOrIntOrVar ^^ { b => val res = s" > $b"; println(res); res }
  def and = "KNOCK" ~> "KNOCK" ~> opOrIntOrVar ^^ { b => val res = s" && $b"; println(res); res }
  def or = "CONSIDER" ~> "THAT" ~> "A" ~> "DIVORCE" ~> opOrIntOrVar ^^ { b => val res = s" || $b"; println(res); res }
  
  def ifThenElse = ("BECAUSE" ~> "I'M" ~> "GOING" ~> "TO" ~> "SAY" ~> "PLEASE" ~> variable) ~ exp ~ ("BULLSHIT" ~> exp <~ "YOU" <~ "HAVE" <~ "NO" <~ "RESPECT" <~ "FOR" <~ "LOGIC") ^^ {
    case x~t~f => val res = s"if $x is true(0) run $t else run $f"; println(res); res
  }
  
  def loop = ("STICK" ~> "AROUND" ~> variable <~ "[") ~ (programBody <~ "]" <~ "CHILL") ^^ {
    case x ~ body => val res = s"as long as $x is true (0) run $body"; println(res); res
  }

  def variable = ident ^^ { x => val res = s"variable $x"; println(res); res }
  def int = wholeNumber ^^ { x => val res = s"int $x"; println(res); res }
}