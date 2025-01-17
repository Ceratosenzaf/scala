package payrollv2

import scala.language.implicitConversions
import java.math.{BigDecimal}
import payrollv2.Money

object Type2Money {
  implicit def bigDecimal2Money(amount: BigDecimal): Money = Money(amount)
  implicit def double2Money(amount: Double): Money = Money(amount)
  implicit def int2Money(amount: Int): Money = Money(amount)
}