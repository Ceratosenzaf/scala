package payrollv2

import payrollv2.Money
import payrollv2.Type2Money._

case class Duration(n: Int) {
  def days = n
  def weeks = n * 5
  def years = weeks * 52
}

case class DeductionAmount(v: Double) {
  def in(builder: DeductionBuilder) = {
    builder.addDeductions(v)
    builder
  }
  def percent_of(builder: DeductionBuilder) = {
    builder.addDeductionsPercentageOfGross(v)
    builder
  }
}

class GrossPayBuilder(val employee: Employee) {
  var gross: Money = 0

  def salary_for(days: Int) = {
    gross = dailyGrossSalary(employee.grossAnnualSalary) * days
    this
  }

  def weeklyGrossSalary(annual: Money) = annual / 52
  def dailyGrossSalary(annual: Money) = weeklyGrossSalary(annual) / 5
}

class DeductionBuilder(gpb: GrossPayBuilder) {
  val employee = gpb.employee
  var paycheck = new Paycheck(gpb.gross, 0)

  def minus_deductions_for(rules: DeductionBuilder => Unit) = {
    rules(this)
    paycheck
  }

  def addDeductions(amount: Money) =
    paycheck = paycheck.plusDeductions(amount)

  def addDeductionsPercentageOfGross(percentage: Double) = {
    val amount = paycheck.gross * (percentage/100.0)
    addDeductions(amount)
  }
}
