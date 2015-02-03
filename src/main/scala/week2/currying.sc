object currying {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) {
      0
    } else {
      f(a) + sum(f)(a + 1, b)
    }
  }

  sum(x => x)(1, 5)

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) {
      1
    } else {
      f(a) * product(f)(a + 1, b)
    }
  }

  product(x => x)(1, 4)

  def fact(x: Int) = product(x => x)(1, x)

  fact(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) {
      zero
    }
    else {
      combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
    }
  }

  def productMR(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)

  productMR(x => x)(1, 4)

  mapReduce(x => x, (x, y) => x * y, 1)(1, 4)
  mapReduce(x => x * x, (x, y) => x + y, 0)(1, 3)


}