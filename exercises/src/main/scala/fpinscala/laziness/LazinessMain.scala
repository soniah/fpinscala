package fpinscala.laziness

object LazinessMain {
  import fpinscala.laziness._
  def main(args: Array[String]): Unit = {

    val l = Stream(10,12,13,14,15)
    println("toList: " + l.toList)
    println("take(2): " + l.take(2).toList)
    println("drop(2): " + l.drop(2).toList)

    def even(x: Int) = {(x % 2) == 0}
    println("takeWhile(even): " + l.takeWhile(even).toList)

    val evens = Stream(10,12,14,16)
    val odds = Stream(11,13,15,17)
    println("forAll - evens: " + evens.forAll(even))
    println("forAll - odds: " + odds.forAll(even))
    println("forAll - mixed: " + l.forAll(even))
  }
}
