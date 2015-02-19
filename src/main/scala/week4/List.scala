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

  override def toString = "["+head.toString+","+tail.toString+"]"
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

 override def toString = "_"
}

object List {

  //def apply [T] (x1: T, x2: T): List [T] = new Cons (x1, new Cons(x2, new Nil))
  def apply [T] (): List [T] = new Nil
  def apply [T] (x: T): List [T] = new Cons (x, List())
  def apply [T] (x1: T, x2: T): List [T] = new Cons (x1, List (x2))

}