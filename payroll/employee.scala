package payroll

import payroll.Money

case class Name(firstName: String, lastName: String) {
  override def toString(): String = s"$firstName $lastName"
}
case class Employee(name: Name, grossAnnualSalary: Money)