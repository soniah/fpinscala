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
    rngb => {
      val (a, rnga) = s(rngb)
      (f(a), rnga)
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

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1,r1) = rng.nextInt
    val (d2,r2) = double(r1)
    ((i1,d2),r2)
  }

  // soln: use intDouble, reverse output
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d1,r1) = double(rng)
    val (i2,r2) = r1.nextInt
    ((d1,i2),r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  // scalaz has an unfold, but...
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int)(rng: RNG)(acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) (acc,rng)
      else {
        val (i1, r1) = rng.nextInt
        go(count-1)(r1)(i1 :: acc)
      }
    }
    go(count)(rng)(List[Int]())
  }

  def foo: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // soln: used _ not i; but whatever...
  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rngc => {
      val (a, rnga) = ra(rngc)
      val (b, rngb) = rb(rnga)
      (f(a,b),rngb)
    }

  // soln: my soln works, but inelegantly. When I see a list
  // input, I should think "foldRight"! z is then
  // 'unit(List[A]())' and join results using map2 (which I
  // just wrote) operating on ::
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rngla => {
      fs match {
        case h :: t => {
          val (ha, hrng) = h(rngla)
          val randt = sequence(t)
          val (ta, trng) = randt(hrng)
          ((ha :: ta), trng)
        }
        case _ => (List[A](), rngla)
      }
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (x, rngx) = f(rng)
      g(x)(rngx)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s){ i => unit(f(i)) }

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ a => map(rb) { b => f(a,b) } }

}

// ===============================================

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    sys.error("todo")

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {

    })

  // def unit -> in object State

  // def sequence -> in object State

}

// ===============================================

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
