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
   */

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
  }
}
