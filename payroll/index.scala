package payroll

import payroll._
import payroll.rules._
import payroll.Type2Money._

object Payroll {
  def main(args: Array[String]) = {
    val john = Employee(Name("John", "Doe"), 30_000)
    val mike = Employee(Name("Mike", "Tyson"), 50_000)
    val louis = Employee(Name("Louis", "Smith"), 80_000)

    val payrollCalculator = rules { employee =>
      employee salary_for 2.weeks minus_deductions_for { gross =>
        federalIncomeTax is (25.0 percent_of gross)
        stateIncomeTax is (5.0 percent_of gross)
        insurancePremiums are (500.0 in gross)
        retirementFundContributions are (10.0 percent_of gross)
      }
    }

    List(john, mike, louis).foreach(e => 
      println(s"${e.name}: ${payrollCalculator(e)}")
    )
  }
}