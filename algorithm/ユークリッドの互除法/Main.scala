import scala.io.StdIn

object Main {
  def main(args: Array[String]) {
    var a = StdIn.readLine().split(" ").map(_.toInt)

    println(yukuriddo(a(0), a(1)))
  }

  def yukuriddo(a: Int, b: Int): Int = {

    if (b != 0) {
      yukuriddo(b, a % b)
    } else {
      a
    }

  }
}