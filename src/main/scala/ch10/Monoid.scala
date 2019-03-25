package main.ch10

/**
 * A Monoid is a type of algebra which adheres to the following constructs:
 * - It has some type, A
 * - It has an associative binary operator
 * - It has a value zero, an identity value
 */
trait Monoid[A] {
  def op(a: A, b: A): A
  val zero: A
}
object Monoid {
  // One example is string 
  val stringMonoid = new Monoid[String] {
    def op(a: String, b: String) = a + b
    val zero = ""
  }
  
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a: List[A], b: List[A]) = a ++ b
    val zero = List()
  }
  
  /**
   * Exercise 10.1
   * Implement moinoids for addition, multiplication, and booleans
   */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int) = a + b
    val zero = 0
  }
  
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int) = a * b
    val zero = 1
  }
  
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a | b
    val zero = false
  }
  
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a & b
    val zero = false
  }
  
  /**
   * Exercise 10.2
   * Give a monoid instance for combining options
   */
  // The following is just one option for "combining" the Option type
  // The dual is also valid
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]) = a orElse b
    val zero = None
  }
  
  // Something interesting discussed in the solutions is the dual of any monoid
  // so I thought it should be added.
  // The dual can be obtained by simply flipping the direction of op
  def dual[A](m: Monoid[A]) = new Monoid[A] {
    def op(a: A, b: A) = m.op(b,a)
    val zero = m.zero
  }
  
  /**
   * Exercise 10.3
   * A function having the same argument and return type is called an <b>endofunction</b>
   * Write a monoid for such a type.
   */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a: A=>A, b: A=>A) = a.compose(b)
    val zero: A => A = (z) => z
  }
}