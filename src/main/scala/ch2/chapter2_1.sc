package ch1

object chapter1 {
  /* Exercise 2.1
   * Write a recursive function to find the nth Fibbonacci number
   */
   def fib(n: Int): Int = {
    def loop(a: Int, b: Int, n: Int): Int = {
      if (n == 0) b
      else loop(b, a + b, n - 1)
    }
    loop(1, 0, n)
  }                                               //> fib: (n: Int)Int
  
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
   }                                              //> isSorted: [A](as: Array[A], ordered: (A, A) => Boolean)Boolean
   isSorted(Array(1,2,3,4), (a: Int ,b: Int) => a < b)
                                                  //> res0: Boolean = true
   isSorted(Array(1,10,3,4), (a: Int, b: Int) => a < b)
                                                  //> res1: Boolean = false
  
}