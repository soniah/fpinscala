package fpinscala.laziness

object LazinessMain {
  import fpinscala.laziness._
  def main(args: Array[String]): Unit = {

    val l = Stream(10,12,13,14,15)
    val e = Empty
    println("toList: " + l.toList)
    println("drop(2): " + l.drop(2).toList)

    def even(x: Int) = {(x % 2) == 0}
    println("takeWhile(even): " + l.takeWhile(even).toList)

    val evens = Stream(10,12,14,16)
    val odds = Stream(11,13,15,17)
    println("forAll - evens: " + evens.forAll(even))
    println("forAll - odds: " + odds.forAll(even))
    println("forAll - mixed: " + l.forAll(even))

    println("headOption - l: " + l.headOption)
    println("headOption - empty: " + Stream.empty.headOption)

    def plus1(x: Int) = {x+1}
    println("filter: " + l.filter(even).toList)
    println("append: " + evens.append(odds).toList)
    def plus2(x: Int) = {Stream(x+2)}
    println("flatMap: " + evens.flatMap(plus2).toList)

    println("ones: " + Stream.ones.take(5).toList)
    println("constant_old: " + Stream.constant_old(42).take(3).toList)
    println("constant: " + Stream.constant(42).take(3).toList)
    println("from_old: " + Stream.from_old(5).take(3).toList)
    println("from: " + Stream.from(5).take(3).toList)
    println("fibs_old: " + Stream.fibs_old(0,1).take(10).toList)
    println("fibs: " + Stream.fibs.take(10).toList)

    println("map: " + evens.map(plus1).toList)
    println("mapU: " + evens.mapU(plus1).toList)

    println("take(2): " + l.take(2).toList)
    println("takeU(2): " + l.take(2).toList)
    println("take(2): " + e.take(2).toList)
    println("takeU(2): " + e.take(2).toList)

    println("takeWhile(even): " + l.takeWhile(even).toList)
    println("takeWhileU(even): " + l.takeWhile(even).toList)
  }
}
