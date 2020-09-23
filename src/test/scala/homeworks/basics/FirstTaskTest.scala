package homeworks.basics

import homeworks.basics.FirstTask._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class FirstTaskTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "Positive greatest common divisor" should "be correct" in {
    gcd(1, 10) shouldEqual 1
    gcd(5, 10) shouldEqual 5
    gcd(24, 24) shouldEqual 24
  }

  "Zero greatest common divisor" should "be correct" in {
    gcd(0, 0) shouldEqual 0 //or better to throw exception ?
    gcd(5, 0) shouldEqual 5
    gcd(0, 15) shouldEqual 15
  }

  "Negative greatest common divisor" should "be correct" in {
    gcd(-5, 10) shouldEqual 5
  }

  "Positive lowest common denominator" should "be correct" in {
    lcm(24, 12) shouldEqual 24
    lcm(36, 48) shouldEqual 144
    lcm(50, 180) shouldEqual 900
  }

  "Zero lowest common denominator" should "be correct" in {
    lcm(5, 0) shouldEqual 0
  }

  "Negative lowest common denominator" should "be correct" in {
    lcm(24, -12) shouldEqual 24
  }

}
