package longLang

import scala.util.parsing.combinator._
import longLang.LongLangInterpreter

class LongLangParser extends JavaTokenParsers {
  def program = rep1(task)
  def task = "task" ~> (ident ~ taskDef)
  def taskDef = "{" ~> taskBody <~ "}"
  def taskBody = rep1(taskOp)
  def taskOp = (remove | rename | merge | backup) ^^ { op =>
    try { op(); true }
    catch { case (_: Throwable) => false }
  }

  def remove = "remove" ~> unquotedStringLiteral ^^ { a =>
    () => LongLangInterpreter.remove(a)
  }
  def rename = "rename" ~> (unquotedStringLiteral ~ unquotedStringLiteral) ^^ { case a~b =>
    () => LongLangInterpreter.rename(a,b)
  }
  def merge = "merge" ~> (unquotedStringLiteral ~ unquotedStringLiteral ~ unquotedStringLiteral) ^^ { case a~b~c =>
    () => LongLangInterpreter.merge(a,b,c)
  }
  def backup = "backup" ~> (unquotedStringLiteral ~ unquotedStringLiteral) ^^ { case a~b =>
    () => LongLangInterpreter.backup(a,b)
  }

  def unquotedStringLiteral = stringLiteral ^^ { s => s.substring(1, s.length-1) }
}