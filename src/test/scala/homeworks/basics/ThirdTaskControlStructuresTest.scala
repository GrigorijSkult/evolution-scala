package homeworks.basics

import homeworks.basics.ThirdTaskControlStructures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ThirdTaskControlStructuresTest  extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks  {

  /*
  *Several tests to cover the main calculation and input aspects
  * */

  "Division" should "be correct" in {
    process("divide 4 5")  shouldEqual "4.0 divided by 5.0 is 0.8"
    process("divide -6 2")  shouldEqual "-6.0 divided by 2.0 is -3.0"
  }

  "ErrorMessages" should "messages" in {
    process("divide 4 0")  shouldEqual "ErrorMessage(Error: division by zero is prohibited)"
    process("divide 4")  shouldEqual "ErrorMessage(Error: Division requires two numbers)"
    process("torra 4224")  shouldEqual "ErrorMessage(Error: Invalid operation command)"
    process("max a")  shouldEqual "ErrorMessage(Error: Incorrect number input: a)"
    process("")  shouldEqual "ErrorMessage(Error: Not enough data for calculation)"
    process("min")  shouldEqual "ErrorMessage(Error: Not enough data for calculation)"
  }

  "Sum" should "be correct" in {
    process("sum 5 5 6 8.5")  shouldEqual "the sum of 5.0 5.0 6.0 8.5 is 24.5"
    process("sum 4 -5")  shouldEqual "the sum of 4.0 -5.0 is -1.0"
    process("sum 0 0")  shouldEqual "the sum of 0.0 0.0 is 0.0"
  }

  "Average" should "be correct" in {
    process("average 4 3 8.5 4")  shouldEqual "the average of 4.0 3.0 8.5 4.0 is 4.875"
  }

  "Min" should "be correct" in {
    process("min 4 -3 -17")  shouldEqual "the min of 4.0 -3.0 -17.0 is -17.0"
  }

  "Max" should "be correct" in {
    process("max 4 -3 -17")  shouldEqual "the max of 4.0 -3.0 -17.0 is 4.0"
  }
}
