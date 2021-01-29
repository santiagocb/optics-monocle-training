package co.com.training.optics

import org.scalatest.FunSuite
import monocle.Iso

class IsoSuite extends FunSuite {

  case class Person(name: String, age: Int)

  case class Meter(whole: Int, fraction: Int)
  case class Centimeter(whole: Int)


  test("Iso optic basic test about get and reverse") {

    val personToTuple = Iso[Person, (String, Int)](p => (p.name, p.age)) { case (name, age) => Person(name, age) }

    assert(personToTuple.get(Person("santiagocb", 24)) == ("santiagocb", 24))
    assert(personToTuple(("santiagocb", 24)) == Person("santiagocb", 24))
    assert(personToTuple.reverseGet(("santiagocb", 24))== Person("santiagocb", 24))
  }

  test("Iso allows us to change different kind of collections") {
    def listToVector[A] = Iso[List[A], Vector[A]](_.toVector)(_.toList)
    def vectorToList[A] = listToVector[A].reverse

    assert(listToVector.apply(Vector(1, 2, 3)) == List(1, 2, 3))
    assert(vectorToList.get(Vector(1, 2, 3)) == List(1, 2, 3))

  }

  test("Iso express a value in different ways (setting value with modify)") {
    val centimeterToMeterIso: Iso[Centimeter, Meter] = Iso[Centimeter, Meter] { cm =>
      Meter(cm.whole / 100, cm.whole % 100)
    }{ m =>
      Centimeter(m.whole * 100 + m.fraction)
    }

    val modifiedLense = centimeterToMeterIso.modify(m => m.copy(m.whole + 3))

    assert(centimeterToMeterIso.apply(Meter(1, 0)) == Centimeter(100))
    assert(centimeterToMeterIso.get(Centimeter(120)) == Meter(1, 20))
    assert(modifiedLense(Centimeter(155)) == Centimeter(455))
  }

  test("Iso other example with string and char") {

    val stringToList = Iso[String, List[Char]](_.toList)(_.mkString(""))

    val lense = stringToList.modify(_.tail)

    assert(lense("Hello") == "ello")
  }
}
