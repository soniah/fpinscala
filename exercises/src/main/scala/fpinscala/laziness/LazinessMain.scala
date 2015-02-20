package fpinscala.laziness

object LazinessMain {
  import fpinscala.laziness._
  def main(args: Array[String]): Unit = {

    val l = Stream(1,2,3,4,5)
    println("toList: " + l.toList)
    println("take: " + l.take(2).toList)

  }
}
