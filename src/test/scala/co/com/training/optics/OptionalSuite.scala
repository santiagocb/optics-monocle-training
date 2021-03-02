package co.com.training.optics

import co.com.training.optics.entities.{Discount, FixPriceOff, NoDiscount, PercentageOff}
import monocle.Optional
import org.scalatest.FunSuite

class OptionalSuite extends FunSuite {

  def getDiscountValue(discount: Discount): Option[Double] = {
    val maybeDiscountValue = Optional[Discount, Double] {
      case pctOff: PercentageOff => Some(pctOff.value)
      case fixOff: FixPriceOff => Some(fixOff.value)
      case _ => None
    } { discountValue => discount =>
      discount match {
        case pctOff: PercentageOff => pctOff.copy(value = discountValue)
        case fixOff: FixPriceOff => fixOff.copy(value = discountValue)
        case _ => discount
      }
    }

    maybeDiscountValue.getOption(discount)
  }

  val head: Optional[List[Int], Int] = Optional[List[Int], Int] {
    case Nil => None
    case x :: xs => Some(x)
  } { a =>
  {
    case Nil => Nil
    case x :: xs => a :: xs
  }
  }

  val xs = List(1, 2, 3)

  test("Optional optic version for list headOption") {

    assert(head.getOption(xs).contains(1))
    assert(head.nonEmpty(xs))
  }

  test("Optional getOrModify method") {

    val r = head.getOrModify(xs)
    val r0 = head.getOrModify(List.empty)

    assert(r.isRight)
    assert(r.contains(1))
    assert(r0.isLeft)
    assert(r0 == Left(List.empty))
  }

  test("Optional setters") {

    val r = head.set(5)(xs)
    assert(r == List(5, 2, 3))
    assert(head.set(10)(List.empty) == List())
  }

  test("Optional modify & modifyOption method for modifying the target of Optional") {

    val optionalModified = head.modify(a => a + 1)

    assert(optionalModified(xs) == List(2, 2, 3))
    assert(optionalModified(List.empty) == List.empty)

    assert(head.modifyOption(a => a + 1)(xs).contains(List(2, 2, 3)))
    assert(head.modifyOption(a => a + 1)(List.empty).isEmpty)

    assert(head.setOption(5)(xs).contains(List(5, 2, 3)))
  }

  test("Optional Discount example") {

    val value = 3L
    assert(getDiscountValue(FixPriceOff(value)).contains(value))
    assert(getDiscountValue(NoDiscount()).isEmpty)
  }
}
