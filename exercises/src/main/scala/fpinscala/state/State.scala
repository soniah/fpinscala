package fpinscala.state


trait RNG {
  // Should generate a random `Int`. We'll later define other
  // functions in terms of `nextInt`.
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text
  case class Simple(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {

      // `&` is bitwise AND. We use the current seed to generate
      // a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) &
        0xFFFFFFFFFFFFL

      // The next state, which is an `RNG` instance created from
      // the new seed.
      val nextRNG = Simple(newSeed)

      // `>>>` is right binary shift with zero fill. The value
      // `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt

      // The return value is a tuple containing both a
      // pseudo-random integer and the next `RNG` state.
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // soln: +1 for n1<0
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng1) = rng.nextInt
    if (n1 < 0) nonNegativeInt(rng1)
    else (n1, rng1)
  }

  // soln: / (Int.MaxValue.toDouble + 1)
  def double(rng: RNG): (Double, RNG) = {
    val (n1, rng1) = nonNegativeInt(rng)
    if (n1 == Int.MaxValue) ((n1 - 1).toDouble / Int.MaxValue, rng1)
    else (n1.toDouble / Int.MaxValue, rng1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
