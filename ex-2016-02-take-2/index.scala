package stepByStep

import stepByStep._

object StepByStepEvaluator {
  def main(args: Array[String]): Unit = args.foreach { expr => {
    val p = new StepByStepParser
    p.parseAll(p.expr, expr) match {
      case p.Success(res, _) => println(res)
      case e => println(e)
    }
  }}
}