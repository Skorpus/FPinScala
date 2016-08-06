package main.scala.ch3

// sealed means that all must be defined in this file
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// This is known as the companion object. It allows us to create functions 
// that work on lists that don't need to be called in the typical OO dot notation.
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // * means many parameters
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /* Exercise 3.1
	 * What will be the result of the following expression?
	 */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    //    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /* Exercise 3.2
   * Define the tail expression which obtains the tail of the list
   */
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => sys.error("Accessing empty List")
    case Cons(a, as) => as
  }

  /*
   * Extra function to assist returning the head of the List
   */
  def head[A](xs: List[A]): A = xs match {
    case Nil => sys.error("Accessing empty List")
    case Cons(a, as) => a
  }

  /* Exercise 3.3
   * Implement the setHead function for replacing the first element of the list
   */
  def setHead[A](a: A, as: List[A]): List[A] = {
    Cons(a, tail(as))
  }

  /* Exercise 3.4
   * Implement drop which removes the first n elements
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  /* Exercise 3.5
   * Implement the dropWhile, which removes elements from the List prefix
   * as long as they match the predicate
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    if (f(head(l))) dropWhile(tail(l), f)
    else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /* Exercise 3.6
   * Implement a function, init, that returns a list consisting of all but 
   * the last element of a List. This function cannot be implemented in 
   * constant time as the List is singly linked
   */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("Init on empty list")
      case Cons(h, t) => Cons(h, init(t))
      case Cons(_, Nil) => Nil
    }
  }

  /*
   * Fold right is a higher order function to use to perform similar functions
   * as those defined previously. Sum and product can be implemented using this.
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  def sum2(as: List[Int]) = {
    foldRight(as, 0)(_ + _)
  }

  def product2(as: List[Double]) = {
    foldRight(as, 1.0)(_ * _)
  }

  /* Exercise 3.9
   * Compute the length of a list
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc: Int) => acc + 1)
  }

  /* Exercise 3.10
   * Create a tail recursive function foldLeft
   */
  // The annotation is used when expect the function to use tail
  // recursion. Will through compile time error if it doesn't
  @annotation.tailrec
  final def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  /* Exercise 3.11
   * Write sum, product, and length in terms of foldLeft
   */
  def sum3(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }
  def product3(as: List[Double]): Double = {
    foldLeft(as, 1.0)(_ * _)
  }
  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  /* Exercise 3.12
   * Write a function that reverses a list
   */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((t, h) => Cons(h, reverse(t)))
  }

  /* Exercise 3.14
   * Implement append in either terms of foldLeft or foldRight. Where append
   * it the appending of second list.
   */
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(a1, a2)((b, a) => Cons(a, b))
  }

  /* Exercise 3.15
   * Hard - Write a function that concatenates a list of lists into
   * a single list. Runtime should be linear in size of all the lists. Try
   * only use functions we have defined.
   */
  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, List[A]())(appendViaFoldRight)
  }

  /* Exercise 3.16
   * Write a function that transforms a list of integers by adding 1 to each element
   */
  def mapPlusOne(as: List[Int]): List[Int] = {
    foldRight(as, List[Int]())((h, t) => Cons(h + 1, t))
  }

  /* Exercise 3.17
   * Write a function that turns each value in a List[Double] into a String
   */
  def mapToString(as: List[Double]): List[String] = {
    foldRight(as, List[String]())((h, t) => Cons(h.toString(), t))
  }

  // Need this function as foldRight is not stack safe so this should be used
  // instead of foldRight to avoid stack overflow on large lists
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  /* Exercise 3.18
   * Write a function map that generalizes modifying each element in a list
   * while maintaining the structure of the list.
   */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    // Need to define Nil type for type checker
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  /* Exercise 3.19
   * Write a function filter that removes elements from a list unles they
   * satisfy a given predicate.
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  /* Exercise 3.20
   * Write a function flatMap that works like map except that the function given
   * will return a list instead of a single result, and that list should be inserted
   * into the final resulting list.
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  /* Exercise 3.21
   * Use flatMap to implement filter.
   */
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a) => if (f(a)) List(a) else Nil)
  }

  /* Exercise 3.22
   * Write a function that accepts two lists and constructs a new list
   * by adding corresponding elements.
   */
  def addPairwise(as: List[Int], bs: List[Int]): List[Int] = {
    (as, bs) match {
      case (_, Nil) => List[Int]()
      case (Nil, _) => List[Int]()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }
  }

  /* Exercise 3.23
   * Generalize the function you have just wrote and call it zipWith
   */
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  /* Exercise 3.24
   * Hard - implement hasSubsequence for checking whether a List contains 
   * another List as a subsequence
   */
  
}
/**
 * @author Dylan Maccora
 */
object Main {
  def main(args: Array[String]) = {
    println(List.zipWith(List(1,1,1),List(1,2,3))(_ + _))
    println(List.length2(List(1,1,1,1,1)))
    println(List.length(List(1,1,1,1,1)))
  }
}