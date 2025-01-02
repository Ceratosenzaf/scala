package longLang

import longLang._

object LongLang {
  def main(args: Array[String]): Unit = {
    val program =
    """
      task TaskOne {
        rename "test" "test.old"
      }
      task TaskTwo {
        backup "test.old" "test.new"
        backup "test.new" "test"
      }
      task TaskThree {
        merge "test.old" "test.new" "test"
      }
    """
    val p = new LongLangParser

    p.parseAll(p.program, program) match {
      case p.Success((res), _) => res.foreach { case p.~(task, ops) => {
        println(s"Task $task")
        ops.zipWithIndex.foreach { case (op, i) => println(s" [op${i+1}] $op") }
      }}
      case e => println(e)
    }
  }
}