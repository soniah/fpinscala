package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means
  // that the function `f` takes its second argument by name
  // and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      // If `f` doesn't evaluate its second argument, the
      // recursion never occurs.
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // Here `b` is the unevaluated recursive step that folds the
  // tail of the stream. If `p(a)` returns `true`, `b` will
  // never be evaluated and the computation terminates early.
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  @annotation.tailrec // soln
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case s => s // or soln: case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (p(h)) cons(h, t.takeWhile(p))
      else empty)

  // Since `&&` is non-strict in its second argument, this
  // terminates the traversal as soon as a nonmatching element
  // is found.
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  // hint: not None, but 'None: Option[A]'
  def headOption: Option[A] =
    foldRight(None: Option[A])((x,_) => Some(x))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  // solutions: use tailrec + List.reverse
  def toList: List[A] = foldRight(List[A]())(_ :: _)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter[A](p: (A) => Boolean): Stream[A] = sys.error("todo")

  // append should be non-strict in it's arguments
  def append[B >: A](rest: Stream[B]): Stream[B] = sys.error("todo")

  def flatMap[A,B](f: (A) => Stream[B]): Stream[B] = sys.error("todo")

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
