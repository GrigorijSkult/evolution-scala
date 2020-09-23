package homeworks.basics

object FirstTask {

  /*
  * The calculation algorithms were taken from:
  * first for Greatest common divisor calculation (gcd) https://habr.com/ru/post/205106/
  * third for Lowest common denominator calculation (lcm) http://spacemath.xyz/nod_i_nok/
   */

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) {
      Math.abs(a)
    } else {
      gcd(b, a % b)
    }
  }

  def lcm(a: Int, b: Int): Int = {
    Math.abs((a * b) / gcd(a, b))
  }

}
