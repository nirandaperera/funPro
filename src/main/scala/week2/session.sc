object session {

  def gcd(x: Int, y: Int): Int =
    if (y == 0) {
      x
    } else {
      gcd(y, x % y)
    }

  gcd(14, 21)

  def factorial(x: Int): Int =
    if (x == 0) {
      1
    } else {
      x * factorial(x - 1)
    }

  factorial(3)

  //tail recursion
  def factorialTR(x: Int): Int = {
    def loop(acc: Int, x: Int): Int =
      if (x == 0) {
        acc
      }
      else {
        loop(acc * x, x - 1)
      }
    loop(1, x)
  }

  factorialTR(3)

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) {
      0
    } else {
      f(a) + sum(f, a + 1, b)
    }
  }

  sum(x => x, 1, 3)

  def sumTR(f: Int => Int, a: Int, b: Int) = {
    def loop(acc: Int, x: Int): Int =
      if (x > b) {
        acc
      }
      else {
        loop(f(x) + acc, x + 1)
      }
    loop(0, a)
  }

  sum(x => x, 1, 4)
  sumTR(x => x * x, 3, 5)
}