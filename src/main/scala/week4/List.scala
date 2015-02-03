package week4

import java.util.NoSuchElementException

/**
 * Created by niranda on 1/5/15.
 */
trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.head")
}

object List {

  def apply [T] (x1: T, x2: T): List [T] = new Cons (x1, new Cons(x2, new Nil))

}