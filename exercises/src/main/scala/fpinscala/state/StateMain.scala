package fpinscala.state

object StateMain {
  import fpinscala.state._
  def main(args: Array[String]): Unit = {
    val rng = RNG.Simple(42)

    val (n1, _) = RNG.nonNegativeInt(rng)
    println("nonNegativeInt: " + n1)

    val (n2, _) = RNG.double(rng)
    println("double: " + n2)

    val ((n3,n4), _) = RNG.intDouble(rng)
    println("intDouble: " + n3 + " " + n4)

    val ((n5,n6), _) = RNG.doubleInt(rng)
    println(s"doubleInt: $n5 $n6")

    val ((n7,n8,n9), _) = RNG.double3(rng)
    println(s"double3: $n7 $n8 $n9")
  }
}
