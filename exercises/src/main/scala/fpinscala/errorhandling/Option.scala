package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  // apply f, if the Option is not None
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // extractor - return the value inside the Some, or the default if None
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // apply f (which may fail) to the Option, if the Option is not None
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  // return the first Option (if not None), otherwise the second Option
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  //convert Some to None if the value doesn't satisfy f
  def filter(f: A => Boolean): Option[A] =
    this flatMap (x => if (f(x)) Some(x) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // if the mean of a sequence is m, the variance is the mean of
  // math.pow(x - m, 2), for each element x in the sequence
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (mm => mean(xs.map(x => math.pow(x - mm, 2))))
  }

  // combine two Option values using a binary function; if either Option value
  // is None, then the return value is too
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  // combine a list of Options into one Option containing a list of all the Some values in
  // the original list. If the original list contains None even once, the result of the
  // function should be None; otherwise the result should be Some with a a list of all
  // the values.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = sys.error("todo")
  // ==> recursion?
  // ==> foldRight? (List)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}
