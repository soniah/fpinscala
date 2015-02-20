package fpinscala.laziness

object LazinessMain {
  import fpinscala.laziness._
  def main(args: Array[String]): Unit = {

    val l = Stream(1,2,3)
    println("toList: " + (l.toList))

  }
}
