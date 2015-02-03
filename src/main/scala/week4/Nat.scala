package week4

/**
 * Created by niranda on 1/12/15.
 */
// Peano numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}


object Zero extends Nat{
  def isZero: Boolean = true

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) this else throw new Error ("negative number")

  def predecessor: Nat = throw new Error ("0.predecessor")
}

class Succ(n: Nat) extends Nat{
  def isZero: Boolean = false

  def predecessor: Nat = n

  def +(that: Nat): Nat = new Succ(n+that)

  def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}

