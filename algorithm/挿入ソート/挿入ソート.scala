import scala.io.StdIn

object Main {
  def main(args: Array[String]){
    val num = StdIn.readInt()
    val input = StdIn.readLine().split(" ").map(_.toInt)
    insertionSort(input,num)
  }
  
  def insertionSort(input: Array[Int], num: Int) = {
      println(input.mkString(" "))
      for(i <- 1 to num -1){
          val v = input(i)
          var j = i -1
          while(j >= 0 && input(j) > v){
              input(j+1) = input(j)
              j = j -1
          }
          input(j+1) = v
          println(input.mkString(" "))
      }
      
  }
}