object classes {


  // class for rational arithmetic
  class Rational(a: Int, b: Int) {

    def numer = a

    def denom = b

  }

  def addRational(a: Rational, b: Rational): Rational = {

    new Rational(a.numer * b.denom + b.numer * a.denom, a.denom * b.denom)

  }

  def makeString(x: Rational): String = {

    x.numer + "/" + x.denom

  }

  // add two fractions
  makeString(addRational(new Rational(1,2),new Rational(3,4)))
}