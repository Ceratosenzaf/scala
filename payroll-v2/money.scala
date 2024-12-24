package payrollv2

import java.math.{BigDecimal,MathContext,RoundingMode}

class Money (val amount: BigDecimal) {
  def + (m: Money) = Money(amount.add(m.amount))
  def - (m: Money) = Money(amount.subtract(m.amount))
  def * (m: Money) = Money(amount.multiply(m.amount))
  def / (m: Money) = Money(amount.divide(m.amount, Money.scale, Money.roundingMode))

  def < (m: Money) = amount.compareTo(m.amount) < 0
  def <= (m: Money) = amount.compareTo(m.amount) <= 0
  def > (m: Money) = amount.compareTo(m.amount) > 0
  def >= (m: Money) = amount.compareTo(m.amount) >= 0

  override def toString(): String = f"$$$amount%.2f"
  override def equals(that: Any): Boolean =
    that match {
      case m: Money => amount.equals(m.amount)
      case _ => false
    }
}

object Money {
  def apply(amount: BigDecimal) = new Money(amount)
  def apply(amount: Double) = new Money(scaled(new BigDecimal(amount)))
  def apply(amount: Int) = new Money(scaled(new BigDecimal(amount)))
  def unapply(m: Money) = Some(m.amount)

  val scale = 4; val roundingMode = RoundingMode.HALF_UP
  val context = new MathContext(scale, roundingMode)
  protected def scaled(d: BigDecimal) = d.setScale(scale, roundingMode)
}