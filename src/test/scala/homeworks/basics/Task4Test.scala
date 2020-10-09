package homeworks.basics

import homeworks.basics.Task4._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Task4Test extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "Basic Map - List Test" should "be correct" in {
    val inputMap = Map(
      "a" -> 1,
      "b" -> 2,
      "c" -> 4,
      "d" -> 1,
      "e" -> 0,
      "f" -> 2,
      "g" -> 2
    )

    val outputList = List(
      Set("e") -> 0,
      Set("a", "d") -> 1,
      Set("b", "f", "g") -> 2,
      Set("c") -> 4
    )

    sortConsideringEqualValues(inputMap) shouldEqual outputList
  }

  "BasicTwo Map - List Test" should "be correct" in {
    val inputMap = Map(
      "a" -> 1,
      "2" -> 2,
      "c" -> 4,
      "d" -> 1,
      "4" -> 0,
      "f" -> -1,
      "g" -> 2
    )

    val outputList = List(
      Set("f") -> -1,
      Set("4") -> 0,
      Set("a", "d") -> 1,
      Set("2", "g") -> 2,
      Set("c") -> 4
    )

    sortConsideringEqualValues(inputMap) shouldEqual outputList
  }

}
