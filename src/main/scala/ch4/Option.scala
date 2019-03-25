package main.ch4
//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{ Option => _, Either => _, _ }

sealed trait Option[+A] {
  /* Exercise 4.1
   * Implement the following functions to act on the option type
   */
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(a) => f(a)
    }
  }
  def flatMap2[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }
  /**
   * Returns the result inside the <code>Some</code> case of the <code>Option</code>,
   *  or if the Option is <code>None</code>, returns the given default value.
   */
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }
  /**
   * Returns the first <code>Option</code> if it's defined; otherwise, it
   * returns the second <code>Option</code>
   */
  def orElse[B >: A](ob: Option[B]): Option[B] = {
    this match {
      case None => ob
      case Some(a) => this
    }
  }
  def filter(f: A => Boolean): Option[A] = {
    this match {
      case None => None
      case Some(a) => if (f(a)) this else None
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    xs match {
      case Nil => None
      case _ => Some(xs.sum / xs.length)
    }
  }
  
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap ( m => mean( xs map (x => math.pow(x - m, 2))))
  }
  
  /* Exercise 4.3
   * Write a generic function map2 that combines two Option values using
   * a binary function. If either Option value is None, then the return 
   * value is too.
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a flatMap2 ( x => b map (y => f(x,y)))
  }
  
  /* Exercise 4.4
   * Write a function sequence that combines a list of Options into one Option
   * containing a list of all the Some values in the original list. If the 
   * original list contains None even once, the result of the function should be
   * None; otherwise the result should be Some with a list of all the values.
   */
  //NOTE Really struggled with this one. Try to better understand.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    // Type annotation is required otherwise Scala wrongly infers the result type
    // of the fold as 'Some[Nil.type]'
    a.foldRight[Option[List[A]]](Some(Nil))((c, r) => map2(c,r)(_ :: _))
  }
  
  /* Exercise 4.5
   * Implement the traverse function which maps over a list using a function
   * that may fail. It should only traverse the list once.
   */
  def traverseTwice[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(a map f)
  }
  
  def traverse[A,B](a: List[A])(f:A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }
}

sealed trait Either[+E, +A] {
  /* Exercise 4.6
   * Implement the following functions on the Either class
   */
  def map[B](f: A => B): Either[E,B] = {
    this match {
      case Left(a) => Left(a)
      case Right(a) => Right(f(a))
    }
  }
  def flatMap[EE >: E, B] (f:A => Either[EE,B]):Either[EE,B] = {
    this match {
      case Left(a) => Left(a)
      case Right(a) => f(a)
    }
  }
  def orElse[EE >: E, B >: A] (b: => Either[EE, B]): Either[EE,B] = {
    this match {
      case Left(_) => b
      case Right(_) => this
    }
  }
  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
  }
  
  /* Exercise 4.7
   * Implement sequence and traverse for Either. These should return the 
   * first error that's encountered.
   */
  /**
   * Applies a function that may fail over a <code>List</code>.
   */
  def traverse[E, A, B](as:List[A])(f: A => Either[E,B]): Either[E,List[B]] = {
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }
  }
  def sequence[E, A](es: List[Either[E,A]]): Either[E, List[A]] = {
    // This was just taken from answers. Need to be smart about some as
    // 
    traverse(es)(x => x)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]