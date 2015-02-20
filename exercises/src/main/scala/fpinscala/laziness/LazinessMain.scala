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
  }
}
