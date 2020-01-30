import scala.io.StdIn

object Main {
  def main(args: Array[String]) = {
    val n = StdIn.readInt
    val array = StdIn.readLine.split(" ").map(_.toInt)
    val t = selectionSort(n, array)
    println(t._1.mkString(" "))
    println(t._2)
  }

  def selectionSort(n: Int, array: Array[Int]) = {
    var count = 0
    for (i <- 0 to n - 1) {
      var min = i
      for (k <- i to n - 1) {
        if (array(min) > array(k)) {
          min = k
        }
      }
      if (array(min) != array(i)) {
        val tmp = array(i)
        array(i) = array(min)
        array(min) = tmp
        count = count + 1
      }
    }
    (array, count)
  }
}