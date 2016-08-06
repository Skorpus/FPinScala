package ch1

object chapter1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(275); 
  /* Exercise 2.1
   * Write a recursive function to find the nth Fibbonacci number
   */
   def fib(n: Int): Int = {
    def loop(a: Int, b: Int, n: Int): Int = {
      if (n == 0) b
      else loop(b, a + b, n - 1)
    }
    loop(1, 0, n)
  };System.out.println("""fib: (n: Int)Int""");$skip(329); 
  
  /* Exercise 2.2
   * Implement isSorted which checks whether a given array is sorted
   */
   def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {
   	def loop(n: Int): Boolean = {
   		if (n >= as.length - 1) true
   		else if (!(ordered(as(n), as(n+1)))) false
   		else loop(n+1)
   	}
   	loop(0)
   };System.out.println("""isSorted: [A](as: Array[A], ordered: (A, A) => Boolean)Boolean""");$skip(55); val res$0 = 
   isSorted(Array(1,2,3,4), (a: Int ,b: Int) => a < b);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(56); val res$1 = 
   isSorted(Array(1,10,3,4), (a: Int, b: Int) => a < b);System.out.println("""res1: Boolean = """ + $show(res$1))}
  
}
