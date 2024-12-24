package payrollv2

import payrollv2.Employee
import payrollv2.Money
import payrollv2.Paycheck
import payrollv2.Type2Money._
import payrollv2.DeductionBuilder

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
