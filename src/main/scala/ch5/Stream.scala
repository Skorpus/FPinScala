package ch5

import Stream._

trait Stream[+A] {
  /* Exercise 5.1
   * Write a function to convert a Stream to a List 
   */
  // NOTE this is not tail recursive so can cause overflow.
  def toList: List[A] = {
    this match {
      case Empty => List()
      case Cons(h, t) => h() :: t().toList
    }
  }
  // Trying to do this one with tail recursion - just took solution

  def toListTail: List[A] = {
    @annotation.tailrec
    def loop(rest: Stream[A], list: List[A]): List[A] = {
      rest match {
        case Cons(h, t) => loop(t(), h() :: list)
        case _ => list
      }
    }
    loop(this, List())
  }

  /* Exercise 5.2
   * Write the function take(n) for returning the first n elements of a Stream,
   * and, drop(n) for skipping the first n elements of a Stream.
   */
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
      case Empty => empty
    }
  }

  /* Exercise 5.3
   * Write the function takeWhile for returning all starting elements of a
   * Stream that match the given predicate.
   */
  def takeWhile(f: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) => if (f(h())) Stream.cons(h(), t().takeWhile(f)) else t().takeWhile(f)
      case Empty => empty
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // Implementing some general recursion with foldRight
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z

    }

  /* Exercise 5.4
   * Implement forAll function that checks whether all elements of a Stream
   * match a given predicate. It should terminate as soon as an element does
   * not match the predicate.
   */
  def forAll(f: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(h, t) => f(h()) && t().forAll(f)
    }
  }
  // If we use the above definition of foldRight can rewrite forAll
  def forAll2(f: A => Boolean): Boolean = {
    this.foldRight(true)((a, b) => f(a) && b)
  }

  /* Exercise 5.5
   * Use foldRight to implement takeWhile
   */
  def takeWhile2(p: A => Boolean): Stream[A] = {
    this.foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons(a, b) else empty
    }
  }

  /* Exercise 5.7
   * Implement map, filter, append, and flatMap using foldRight.The append
   * method should be non-strict in its argument.
   */
  def map[B](f: A => B): Stream[B] = {
    this.foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(p: A => Boolean): Stream[A] = {
    this.foldRight(empty[A]) {
      (h, t) => if (p(h)) cons(h, t) else t
    }
  }
  // Recall that this append an entire stream not a single element
  def append[B >: A](that: => Stream[B]): Stream[B] = {
    this.foldRight(that)((h, t) => cons(h, t))
  }
  
  /* Exercise 5.13
   * Use unfold to implement map, take, takeWhile, zipWith, and zipAll.
   */
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    // NOTE This doesn't feel right
    this match {
      case Empty => empty
      case Cons(h,t) => unfold(this)((stream) => Some((f(h()),t())))
    }
  }

}
case object Empty extends Stream[Nothing]
// () => states the parameter is call by name. It is called a thunk
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // The following are known as smart constructors. I believe this
  // is because they are not evaluated until required.
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // lazy caches the value of head and tail to avoid repeated evaluation
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  // This is an infinite Stream, created by recursion. Since it is
  // not evaluated until required we can do this.
  val ones: Stream[Int] = Stream.cons(1, ones)

  /* Exercise 5.8
   * Generalize ones slightly to the function constant, which returns an
   * infinite Stream of a given value.
   */
  def constant[A](a: A): Stream[A] = {
    // Required to be a lazy evaluation
    lazy val result: Stream[A] = Stream.cons(a, result)
    result
  }

  /* Exercise 5.9
   * Write a function that generates an infinite stream of integers,
   * starting from n, then n+1, etc.
   */
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  /* Exercise 5.10
   * Write a function that generates an infinite stream of Fibonacci numbers
   */
  def fibs {
    def loop(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, loop(b, a + b))
    }
    loop(0, 1)
  }
  // 0, 1, 1, 2, 3
  // loop(3,5)
  // loop(2,3)
  // loop(1,2)
  // loop(1,1)
  // loop(0,1)

  /* Exercise 5.11
   * Write a more general Stream building function called unfold which 
   * takes an initial state and a function for producing both the next state
   * and next value for the Stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    // z is the initial state not value
    // stream should terminate when f returns None
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => empty
    }
  }

  /* Exercise 5.12
   * Write fibs, from, constant, and ones in terms of unfold
   */
  def fibsWithUnfold {
    type FibState = (Int, Int)
    unfold((0, 1))(
      (s: FibState) => Some((s._1, (s._2, s._1 + s._2))))
  }
  
  def fromWithUnfold(n: Int): Stream[Int] = {
    // State is defined by value of n
    unfold(n)((s) => Some((s, s + 1)))
  }
  
  def constantWithUnfold(n: Int) = {
    // State is irrelevant, it remains in the same state always
    unfold(n)( _ => Some((n,n)))
  }
  
  def onesWithUnfold {
    constantWithUnfold(1)
  }
  
  
}