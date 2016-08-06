package ch1

object chapter1_1 {
  /* Exercise 2.3
   * Currying. Implement the notion of currying
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    //Want to apply f as g(a)(b):C
    (a: A) => (b: B) => f(a, b)
  }                                               //> curry: [A, B, C](f: (A, B) => C)A => (B => C)

  def sum = curry((x: Int, y: Int) => x + y)      //> sum: => Int => (Int => Int)
  sum(1)(2)                                       //> res0: Int = 3

  /* Exercise 2.4
   * Uncurry. Implement the inverse of the previous exercise
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }                                               //> uncurry: [A, B, C](f: A => (B => C))(A, B) => C

  /* Exercise 2.5
	 * Implement a higher order functoin which composes two functions
	 */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
		(a: A) => f(g(a))
  }                                               //> compose: [A, B, C](f: B => C, g: A => B)A => C
}