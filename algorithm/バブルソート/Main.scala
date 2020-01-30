import scala.io.StdIn

object Main{
    def main(args: Array[String]) = {
        val n = StdIn.readInt
        val array = StdIn.readLine.split(" ").map(_.toInt)
        val (arr, count) = bubblesort(n, array)
        println(arr.mkString(" "))
        println(count)
    }
    
    def bubblesort(n: Int, array: Array[Int]) = {
        var count = 0
        for(i <- 0 to n-1){
            for(k <- n-1 to i+1 by -1){
                if(array(k) < array(k-1)){
                    var tmp = array(k-1)
                    array(k-1) = array(k)
                    array(k) = tmp
                    count = count + 1
                }
            }
        }
        (array, count)
    }
}