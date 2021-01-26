package co.com.training.optics.entities

trait Discount
case class NoDiscount() extends Discount
case class PercentageOff(value: Double) extends Discount
case class FixPriceOff(value: Double) extends Discount