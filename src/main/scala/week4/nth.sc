import week4._
object nth {
  def nth[T](n: Int, xs: List[T]): T =
    if (xs.isEmpty) {
      throw new IndexOutOfBoundsException
    }
    else if (n == 0) {
      xs.head
    }
    else {
      nth(n - 1, xs.tail)
    }
  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  nth(2, list)
  //nth(4, list)
  //nth(-1, list)
  val list12 = List (1,2)
  val list0 = List ()
  val list3 = List(3)
  val compList = List(list3, list12)

  val ll = List (List(1), List(List(2),List(3)))

}