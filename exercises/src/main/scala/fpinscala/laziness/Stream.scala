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

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(p: (A) => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else t)

  // append should be non-strict in it's arguments
  def append[B >: A](a2: => Stream[B]): Stream[B] =
    foldRight(a2)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t )

  // solutions: use tailrec + List.reverse
  def toList: List[A] = foldRight(List[A]())(_ :: _)
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

  val ones_old: Stream[Int] = Stream.cons(1, ones_old)

  // solutions: more efficient version
  def constant_old[A](a: A): Stream[A] =
    Stream.cons(a, constant_old(a))

  // solution: cons(n, from(n+1))
  // is my solution lazier than using cons?
  def from_old(n: Int): Stream[Int] = {
    lazy val tail: Stream[Int] = Cons(() => n, () => tail.map(_ + 1))
    tail
  }

  // soln uses a go(0, 1), but same thing
  def fibs_old(n0: Int, n1:Int): Stream[Int] = {
    cons(n0, fibs_old(n1, n0 + n1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((a,s)) => cons(a, unfold(s)(f))
    }

  // 5.12 use unfold to implement fibs, from, constant and ones

  val ones: Stream[Int] =
    unfold(1)(x => Some((1,1)))

  def constant[A](a: A): Stream[A] =
    unfold(a)(x => Some((x,x)))

  def from(n: Int): Stream[Int] =
    unfold(n)(x => Some((x,x+1)))

  def fibs: Stream[Int] =
    unfold((0,1)){ case (x,y) =>
      Some( (x, (y, x+y)) ) }

}
