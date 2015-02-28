package fpinscala.state

object StateMain {
  import fpinscala.state._
  def main(args: Array[String]): Unit = {
    val rng = RNG.Simple(42)

    val (n1, _) = RNG.nonNegativeInt(rng)
    println("nonNegativeInt: " + n1)

    val (n2, _) = RNG.double(rng)
    println("double: " + n2)
  }
}
