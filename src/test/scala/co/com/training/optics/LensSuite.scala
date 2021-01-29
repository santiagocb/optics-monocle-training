package co.com.training.optics

import co.com.training.optics.entities.{Cart, Item, NoDiscount, User}
import monocle.Lens
import monocle.macros.GenLens
import org.scalatest.FunSuite

class LensSuite extends FunSuite {

  case class Person(name: String, age: Int, address: Address)
  case class Address(streetNumber: Int, streetName: String)

  test("Lens can focus in a specific field of a case class and set/get it") {

    val strNumberLense: Lens[Address, Int] = Lens[Address, Int](_.streetNumber)(n => a => a.copy(streetNumber = n))
    val strNumberLense2: Lens[Address, Int] = GenLens[Address](_.streetNumber)  // Using macros

    val address = Address(5, "La Ceiba")

    assert(strNumberLense2.set(10)(address) == strNumberLense.set(10)(address))
    assert(strNumberLense2.get(address) == 5)
  }

  test("Lens can modify (get and then set) an attribute") {

    val strNumberLense: Lens[Address, Int] = GenLens[Address](_.streetNumber)

    val address = Address(5, "La Ceiba")
    assert(strNumberLense.modify(_ + 1)(address).streetNumber == 6)

  }

  test("Lens modifyF - can modify the context of the Product depending the Functor passed (List case)") {

    import cats.implicits._ // to get Functor[List] instance

    def neighbors(n: Int): List[Int] =
      if (n > 0) List(n - 1, n + 1) else List(n + 1)

    val strNumberLense: Lens[Address, Int] = GenLens[Address](_.streetNumber)

    val address = Address(5, "La Ceiba")
    val address2 = Address(0, "Dabeiba")

    val listAddress = strNumberLense.modifyF(neighbors)(address)

    assert(listAddress == List(Address(streetNumber = 4, streetName = "La Ceiba"), Address(streetNumber = 6, streetName = "La Ceiba")))
    assert(strNumberLense.modifyF(n => neighbors(n))(address2) == List(Address(streetNumber = 1, streetName = "Dabeiba")))
  }

  test("Lens can modify a nested attribute inside a case class in a simply way") {
    def updateLeftInStockWithoutLens(user: User) = {
      user.copy(
        cart = user.cart.copy(
          item = user.cart.item.copy(leftInStock = user.cart.item.leftInStock - user.cart.quantity)
        )
      )
    }

    def updateLeftInStockWithLens(user: User) = {
      val cart: Lens[User, Cart] = GenLens[User](_.cart)
      val item: Lens[Cart, Item] = GenLens[Cart](_.item)
      val leftInStock: Lens[Item, Int] = GenLens[Item](_.leftInStock)

      (cart composeLens item composeLens leftInStock).modify(_ - user.cart.quantity)(user)
    }

    val user = User(
      name = "santiagocb", cart = Cart(
        id = "001", item = Item(
          sku = "909", price = 7600, leftInStock = 2, discount = NoDiscount()), quantity = 1))

    assert(updateLeftInStockWithoutLens(user) == updateLeftInStockWithLens(user))
  }

  test("Lens modify nested case class") {

    val addressLens = GenLens[Person](_.address)
    val streetNumberLens = GenLens[Address](_.streetNumber)

    val address = Address(5, "La Ceiba")
    val john = Person("John", 20, address)

    val johnSix = (addressLens composeLens streetNumberLens).set(6)(john)

    assert((addressLens composeLens streetNumberLens).get(john) == 5)
    assert(johnSix.address.streetNumber == 6)
    assert((addressLens composeLens streetNumberLens).modify(_ + 2)(john) ==
      Person(address = Address(7, "La Ceiba"), name = "John", age = 20))
  }

  test("Lens can allows us to compose few lenses at the same time to modify different attributes") {
    val compose = GenLens[Person](_.name).set("Mike") compose GenLens[Person](_.age).modify(_ + 1)

    val address = Address(5, "La Ceiba")
    val john = Person("John", 20, address)

    assert(compose(john) == Person("Mike", 21, Address(5, "La Ceiba")))
  }

  /* test("Lens annotation for creating lenses to all the attributes of a case class") {

    import monocle.macros.Lenses

    @Lenses case class Point(x: Int, y: Int)
    val p = Point(5, 3)

    assert(Point.x.modify(_ + 10)(p) == Point(15, 3))
  }

  test("Lens annotation can be specified through an specific prefix") {

    import monocle.macros.Lenses

    @Lenses("_") case class OtherPoint(x: Int, y: Int)
    val op = OtherPoint(5, 3)

    assert(OtherPoint._y.modify(_ + 20)(op) == OtherPoint(5, 23))
  } */
}
