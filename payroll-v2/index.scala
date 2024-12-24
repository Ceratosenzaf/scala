package payrollv2

import payrollv2._
import payrollv2.Type2Money._

object Payroll {
  def main(args: Array[String]) = {
    val john = Employee(Name("John", "Doe"), 30000)
    val mike = Employee(Name("Mike", "Tyson"), 50000)
    val louis = Employee(Name("Louis", "Smith"), 80000)

    val employees = List(john, mike, louis)

    val parser = new PayrollParserCombinator(employees)

    val payrollCalculator = 
      """
      paycheck for employee "John Doe"
      is salary for 2 weeks minus deductions for {
        federal income tax is 25. percent of gross,
        state income tax is 5. percent of gross,
        insurance premiums are 500. in gross currency,
        retirement fund contributions are 10. percent of gross
      }
      """

    parser.parseAll(parser.paycheck, payrollCalculator) match {
      case parser.Success(Pair(e: Employee, paycheck: Paycheck), _) => 
        println(s"${e.name}: $paycheck")
      case parser.Failure(msg, _) => println("Failure: " + msg)
      case parser.Error(msg, _) => println("Error: " + msg)
    }
  }
}