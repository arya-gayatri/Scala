object datafunctions {

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x + y
  x - y - z
  x < y
  val testzero = new Rational(2, 0)
  //testzero+testzero
  new Rational(2) // calls second constructor


  // primary constructor implicitly included in class def
  class Rational(x: Int, y: Int) {

    //require(y != 0, "Denominator must be non-zero")

    // second constructor
    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else
      gcd(b, a % b)

    val numer = x / gcd(x, y)
    val denom = y / gcd(x, y)

    def +(that: Rational) = {

      new Rational(
        numer * that.denom + that.numer * denom, denom * that.denom)

    }

    override def toString = {
      numer + "/" + denom
    }

    def mul(that: Rational) = {
      new Rational(numer * that.numer, denom * that.denom)
    }

    def -(that: Rational) = this + -that

    def unary_- = new Rational(-numer, denom)

    def <(that: Rational) = numer * that.denom < denom * that.numer

    def max(that: Rational) = if (that < (this)) this else that

  }

}