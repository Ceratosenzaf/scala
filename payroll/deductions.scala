package payroll

import payroll.Employee
import payroll.Money
import payroll.Paycheck
import payroll.Type2Money._
import payroll.DeductionBuilder

abstract class Deduction {
  def is(builder: DeductionBuilder) = apply(builder)
  def are(builder: DeductionBuilder) = apply(builder)
  def apply(builder: DeductionBuilder): Money
}

object federalIncomeTax extends Deduction {
  def apply(builder: DeductionBuilder): Money = 
    builder.employee.grossAnnualSalary * .25
}

object stateIncomeTax extends Deduction {
  def apply(builder: DeductionBuilder): Money =
    builder.employee.grossAnnualSalary * 0.05
}

object insurancePremiums extends Deduction {
  def apply(builder: DeductionBuilder): Money = 500
}

object retirementFundContributions extends Deduction {
  def apply(builder: DeductionBuilder): Money =
    builder.employee.grossAnnualSalary * .10
}
