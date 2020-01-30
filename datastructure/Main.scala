//
//import scala.{ Option => _, Either => _, _ }
//



object Main {
  def main(args: Array[String]): Unit = {
    
    val s = Stream(1, 2, 3).map(a => a + 1).toList
    val r = SimpleRNG(12)
    val a = r.unit(44)
    println(r.double(r))
    println(a(r))
  }
}