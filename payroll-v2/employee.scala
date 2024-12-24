package payrollv2

import payrollv2.Money

case class Name(firstName: String, lastName: String) {
  override def toString(): String = s"$firstName $lastName"
}
case class Employee(name: Name, grossAnnualSalary: Money)