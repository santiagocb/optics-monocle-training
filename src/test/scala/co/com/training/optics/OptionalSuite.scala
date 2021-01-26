package co.com.training.optics

import co.com.training.optics.entities.{Discount, FixPriceOff, PercentageOff}
import monocle.Optional
import org.scalatest.FunSuite

class OptionalSuite extends FunSuite {

  test("") {

    def getDiscountValue(discount: Discount): Option[Double] = {
      val maybeDiscountValue = Optional[Discount, Double] {
        case pctOff: PercentageOff => Some(pctOff.value)
        case fixOff: FixPriceOff => Some(fixOff.value)
        case _ => None
      } { discountValue => {
          case pctOff: PercentageOff => pctOff.copy(value = discountValue)
          case fixOff: FixPriceOff => fixOff.copy(value = discountValue)
          case _ => discount
        }
      }

      maybeDiscountValue.getOption(discount)
    }
  }
}
