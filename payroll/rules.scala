package payroll

import scala.language.implicitConversions
import payroll.Employee
import payroll.Paycheck
import payroll.GrossPayBuilder
import payroll.DeductionBuilder
import payroll.DeductionAmount

object rules {
  def apply(rules: Employee => Paycheck) = new PayrollBuilderRules(rules)

  implicit def employee2PayBuilder(e: Employee): GrossPayBuilder = new GrossPayBuilder(e)
  implicit def grossPayBuilder2DeductionBuilder(gpb: GrossPayBuilder): DeductionBuilder = new DeductionBuilder(gpb)
  implicit def int2Duration(v: Int): Duration = new Duration(v)
  implicit def double2DeductionAmount(v: Double): DeductionAmount = new DeductionAmount(v)
}

class PayrollBuilderRules(rules: Employee => Paycheck) {
  def apply(e: Employee) = {
    try { rules(e) }
    catch {
      case (th: Throwable) => new PayrollException(
        s"Failed to process payroll for employee ${e}. ${th}"
      )
    }
  }
}

class PayrollException(msg: String) extends Exception(msg)
