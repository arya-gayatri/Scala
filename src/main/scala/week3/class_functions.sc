object datafunctions {

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x.add(y)
  x.sub(y).sub(z)
  y.add(y)
  x.less(y)
  x.max(y)
  val testzero = new Rational(2, 0)
  testzero.add(testzero)
  new Rational(2) // calls second constructor
}

// primary constructor implicitly included in class def
class Rational(x:Int, y:Int){

    require(y!=0, "Denominator must be non-zero")

    // second constructor
    def this(x:Int) = this(x,1)

    private def gcd(a:Int, b:Int):Int = if(b==0) a else
      gcd(b,a%b)

    val numer = x/gcd(x,y)
    val denom = y/gcd(x,y)

    def add(that:Rational) = {

      new Rational(
      numer*that.denom+that.numer*denom, denom*that.denom)

    }

    override def toString = {

        numer+"/"+denom

    }

    def mul(that:Rational) = {

      new Rational( numer*that.numer, denom*that.denom)

    }

    def sub(that:Rational) = add(that.neg)

    def neg = new Rational(-numer, denom)

    def less(that:Rational) = numer*that.denom < denom*that.numer

    def max(that:Rational) = if(that.less(this)) this else that

}
