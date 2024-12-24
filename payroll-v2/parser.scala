package payrollv2

import scala.util.parsing.combinator._
import payrollv2.Employee
import payrollv2.Money
import payrollv2.GrossPayBuilder
import payrollv2.Type2Money._

class PayrollParserCombinator(employees: List[Employee]) extends JavaTokenParsers {
  var currentEmployee: Employee = null
  var grossSalary: Money = Money(0)

  def paycheck = empl ~ gross ~ deductions ^^ {
    case empl ~ gross ~ deductions => {
      try { (empl, Paycheck(gross, deductions)) }
      catch {
        case (err: PayrollException) => err
        case (th: Throwable) => new PayrollException(
          s"Failed to process payroll for employee ${empl}. ${th}"
        )
      }
    }
  }

  def empl = "paycheck" ~> "for" ~> "employee" ~> name ^^ { name => {
      val fullName = name.substring(1, name.length-1).split(" ")
      val inputName = Name(fullName(0), fullName(1))
      employees.find(_.name == inputName) match {
        case Some(e) => currentEmployee = e
        case None => throw new EmployeeNotFoundException(s"Employee '$name' not found")
      }
      currentEmployee
    }
  }
  def name = stringLiteral

  def gross = "is" ~> "salary" ~> "for" ~> duration ^^ { duration => 
    grossSalary = new GrossPayBuilder(currentEmployee).salary_for(duration).gross
    grossSalary
  }
  def duration = intNumber ~ period ^^ { case n ~ p => n * p }
  def intNumber = decimalNumber ^^ { _.toInt }
  def period = days | weeks | years
  def days = "days?".r ^^ { _ => Duration(1).days }
  def weeks = "weeks?".r ^^ { _ => Duration(1).weeks }
  def years = "years?".r ^^ { _ => Duration(1).years }

  def deductions = "minus" ~> "deductions" ~> "for" ~> "{" ~> deductItems <~ "}"
  def deductItems = repsep(deductItem, ",") ^^ { items => items.foldLeft(Money(0))(_ + _) }
  def deductItem = deductKind ~> deductAmount
  def deductKind = tax | insurance | retirement
  def tax = fedState <~ "income" <~ "tax"
  def fedState = "federal" | "state"
  def insurance = "insurance" ~> "premiums"
  def retirement = "retirement" ~> "fund" ~> "contributions"
  def deductAmount = percentage | amount
  def percentage = toBe ~> doubleNumber <~ "percent" <~ "of" <~ "gross" ^^ { percentage =>
    grossSalary * (percentage / 100.0)
  }
  def amount = toBe ~> doubleNumber <~ "in" <~ "gross" <~ "currency" ^^ { Money(_) }
  def toBe = "is" | "are"
  def doubleNumber = floatingPointNumber ^^ { _.toDouble }
}

class PayrollException(msg: String) extends Exception(msg)
class EmployeeNotFoundException(msg: String) extends PayrollException(msg)
