package ch1

object chapter1_1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(232); 
  /* Exercise 2.3
   * Currying. Implement the notion of currying
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    //Want to apply f as g(a)(b):C
    (a: A) => (b: B) => f(a, b)
  };System.out.println("""curry: [A, B, C](f: (A, B) => C)A => (B => C)""");$skip(47); 

  def sum = curry((x: Int, y: Int) => x + y);System.out.println("""sum: => Int => (Int => Int)""");$skip(12); val res$0 = 
  sum(1)(2);System.out.println("""res0: Int = """ + $show(res$0));$skip(175); 

  /* Exercise 2.4
   * Uncurry. Implement the inverse of the previous exercise
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  };System.out.println("""uncurry: [A, B, C](f: A => (B => C))(A, B) => C""");$skip(173); 

  /* Exercise 2.5
	 * Implement a higher order functoin which composes two functions
	 */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
		(a: A) => f(g(a))
  };System.out.println("""compose: [A, B, C](f: B => C, g: A => B)A => C""")}
}
