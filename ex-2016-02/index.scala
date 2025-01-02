package expStepByStep

import expStepByStep._

object ExpStepByStep {
  def main(args: Array[String]): Unit = {
    val p = new ExpStepByStepParser
    args.foreach { exp => {
      p.parseAll(p.exp, exp) match {
        case p.Success(res, _) => ExpStepByStepParserInterpreter.printEval(res)
        case e => println(e)
      }
    }}
  }
}