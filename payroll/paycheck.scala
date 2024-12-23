package payroll

import payroll.Money

case class Paycheck(gross: Money, deduction: Money) {
  def plusGross(amount: Money) = Paycheck(gross + amount, deduction)
  def plusDeductions(amount: Money) = Paycheck(gross, deduction + amount)

  override def toString = s"$gross - $deduction = ${gross - deduction}"
}