package co.com.training.optics

import co.com.training.optics.entities.{Discount, PercentageOff}
import monocle.Monocle.doubleToInt
import monocle.{Lens, Prism}
import monocle.macros.{GenIso, GenLens, GenPrism}
import org.scalatest.FunSuite

class PrismSuite extends FunSuite {

  sealed trait Json
  case object JNull extends Json
  case class JStr(v: String) extends Json
  case class JNum(v: Double) extends Json
  case class JObj(v: Map[String, Json]) extends Json

  case class User(name: String, cart: Cart)
  case class Cart(id: String, item: Item, quantity: Int)
  case class Item(sku: String, price: Double, leftInStock: Int, discount: Discount)

  private val jStr: Prism[Json, String] = Prism[Json, String] {
    case JStr(v) => Some(v)
    case _ => None
  }(JStr)

  test("Prism optic basics getOption") {
    val jStr2 = Prism.partial[Json, String] { case JStr(v) => v }(JStr)

    assert(jStr("santiago") == JStr("santiago"))
    assert(jStr2("santiago") == JStr("santiago"))

    assert(jStr.getOption(JStr("santiago")).contains("santiago"))
    assert(jStr.getOption(JNum(3.2)).isEmpty)
  }

  test("Prism set and modify methods") {

    assert(jStr.set("Bar")(JStr("Hello")) == JStr("Bar"))
    assert(jStr.modify(_.reverse)(JStr("Hello")) == JStr("olleH"))

    // Optic has not been applied
    assert(jStr.set("Bar")(JNum(10)) == JNum(10))
    assert(jStr.modify(_.reverse)(JNum(10)) == JNum(10))
  }

  test("Prism setOption and modifyOption method for verifying optic has been applied") {

    assert(jStr.modifyOption(_.reverse)(JStr("Hello")).contains(JStr("olleH")))
    assert(jStr.modifyOption(_.reverse)(JNum(10)).isEmpty)
  }

  test("Prism composition") {
    val jNum: Prism[Json, Double] = Prism.partial[Json, Double] { case JNum(v) => v }(JNum)
    val jInt: Prism[Json, Int] = jNum composePrism doubleToInt

    assert(jInt(5) == JNum(5))
    assert(jInt.getOption(JNum(5.0)).contains(5))
    assert(jInt.getOption(JNum(5.2)).isEmpty)
    assert(jInt.getOption(JStr("Hello")).isEmpty)
  }

  test("Prism generation") {

    val rawJNum: Prism[Json, JNum] = GenPrism[Json, JNum]

    assert(rawJNum.getOption(JNum(4.5)).contains(JNum(4.5)))
    assert(rawJNum.getOption(JStr("Hello")).isEmpty)
  }

  test("Prism optic generation mixed with Iso optic") {

    /* val jNum: Prism[Json, Double] = GenPrism[Json, JNum] composeIso GenIso[JNum, Double]
    val jNull: Prism[Json, Unit] = GenPrism[Json, JNull.type] composeIso GenIso.unit[JNull.type]

    assert(jNum.getOption(JNum(4.5)).contains(4.5))
    assert(jNull.getOption(JNum(4.5)).isEmpty) */
  }

  test("Prism optic zooms only in an specific attribute of case class") {

    def updateDiscountedItemsPrice(cart: Cart, newDiscount: Double): Cart = {
      val discountLens: Lens[Item, Discount] = GenLens[Item](_.discount)
      val onlyPctDiscount = Prism.partial[Discount, Double] {
        case PercentageOff(p) => p
      }(PercentageOff)

      val newItem =
        (discountLens composePrism onlyPctDiscount set newDiscount)(cart.item)

      cart.copy(item = newItem)
    }

    val originalDiscount = 10L
    val newDiscount = 5L
    val updatedCart = updateDiscountedItemsPrice(
      Cart("abc", Item("item123", 23L, 1, PercentageOff(originalDiscount)), 1),
      newDiscount
    )
    assert(updatedCart.item.discount == PercentageOff(newDiscount))
  }
}
