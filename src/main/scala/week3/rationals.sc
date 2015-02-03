object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  val a = new Rational(4, 7)
  x.add(y)
  x.sub(y)
  x.sub(y).sub(z)
  x.neg
  y.add(y)
  val k = new Rational(1, -2)
  k.add(new Rational(-1))

}

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator is zero!")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def numer = x

  def denom = y

  def add(some: Rational) =
    new Rational(
      numer * some.denom + some.numer * denom,
      denom * some.denom)

  def neg: Rational = new Rational(-numer, denom)

  def sub(some: Rational) = add(some.neg)

  def less(some: Rational) =
    numer * some.denom < denom * some.numer

  def max(some: Rational) =
    if (this.less(some)) some else this

  override def toString = {
    val g = gcd(numer, denom)

    if (denom / g < 0) -numer / g + "/" + -denom / g
    else numer / g + "/" + denom / g
  }
}