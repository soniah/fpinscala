package fpinscala.errorhandling

object ErrorHandlingMain {
  import fpinscala.errorhandling._
  def main(args: Array[String]): Unit = {
    println("Hello World!")

    /*
    val a = List(1.0,2.0,3.0)
    val m = Option.mean(a)
    println("mean:")
    println(m)
    */

   /*
   val b = List(Some(1), Some(2), Some(3))
   val b_res = Option.sequence(b)
   println("b_res: ")
   println(b_res)

   val c = List(Some(1), None, Some(3))
   val c_res = Option.sequence(c)
   println("c_res: ")
   println(c_res)

   def foo(x: Int): Option[Double] = {
     if (x == 42) None
     else Some(x+1)
   }

   val d = List(1,2,3)
   val d_res = Option.traverse(d)(foo)
   println("d_res: ")
   println(d_res)

   val e = List(1,42,3)
   val e_res = Option.traverse(e)(foo)
   println("e_res: ")
   println(e_res)
   */

   def plus1(x: Int): Int = {
     x + 1
   }

   val e = Left("error")
   println("e map plus1: " + (e map plus1))

   val a = Right(2)
   println("a map plus1: " + (a map plus1))

   val b = Right(42)
   println("a orElse b: " + (a orElse b))
   println("e orElse b: " + (e orElse b))

   def plus2(x: Int): Either[String, Int] = {
     if (x == 42) Left("Not the meaning of..")
     else Right(x + 2)
   }
   println("Right(3) flatMap plus2: " + (Right(3) flatMap plus2))
   println("Right(42) flatMap plus2: " + (Right(42) flatMap plus2))

   val esr = List(Right(1), Right(2), Right(3), Right(4))
   val esl = List(Right(1), Left("error1"), Right(3),
     Left("error2") )
   println("Either.sequence(esr)): " + (Either.sequence(esr)))
   println("Either.sequence(esl)): " + (Either.sequence(esl)))

   val tsr = List(1,2,3)
   println("traverse tsr +2: " + (Either.traverse(tsr)(plus2)))
   val tsl = List(1,42,3)
   println("traverse tsl +2: " + (Either.traverse(tsl)(plus2)))

  }
}
