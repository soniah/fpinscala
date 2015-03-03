package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /*
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  */

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    fpinscala.datastructures.List.foldLeft(a1, a2)((acc, h) => Cons(h, acc))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of an empty list")
      case Cons(_, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setHead of an empty list")
      case Cons(_, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => sys.error("drop of an empty list")
      case Cons(x, xs) =>
        if (n == 0) Cons(x, xs)
        else drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x)) dropWhile(xs, f)
        else Cons(x, xs)
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  def sum3(ns: List[Int]) = fpinscala.datastructures.List.foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    fpinscala.datastructures.List.foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    fpinscala.datastructures.List.foldLeft(l, 0)((x, _) => x + 1)

  // def init[A](l: List[A]): List[A] =
  def reverse[A](l: List[A]): List[A] = {
    fpinscala.datastructures.List.foldLeft(l, List[A]())((acc, h)
    => Cons(h, acc))

  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def plus1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def tostr(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((hd, acc) => Cons(hd.toString(), acc))

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((hd, acc) => Cons(f(hd), acc))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((hd, acc) => if (f(hd)) Cons(hd, acc) else acc)

  def concat[A](as: List[List[A]]): List[A] = foldRight(as, Nil: List[A])(append)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  // foldRight(as, List[B]())((hd: A, acc: List[B]) => append(acc, f(hd))  )
    concat(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def zipint(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(a + b, zipint(as, bs))
  }

  def zipwith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipwith(as, bs)(f))
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(a, as), Cons(b, bs)) if (a == b) => startsWith(as, bs)
    case _ => false
  }

  def hasSubs[A](major: List[A], minor: List[A]): Boolean = major match {
    case Nil => false
    case Cons(a, as) if (startsWith(major, minor)) => true
    case Cons(a, as) => startsWith(as, minor)
  }

}
