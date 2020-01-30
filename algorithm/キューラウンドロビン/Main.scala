import scala.io.StdIn


object Main{
    def main(args: Array[String]) = {
        val tmp = StdIn.readLine.split(" ").map(_.toInt)
        val n = tmp(0)
        val q = tmp(1)
        var queue = new scala.collection.mutable.Queue[(String, Int)]
        for(i <- 0 to n-1){
            val a = StdIn.readLine.split(" ")
            queue += ((a(0), a(1).toInt))
        }
        
        calc(q, queue)
        ddd.foreach(f => println(f._1 + " " + f._2))
        
    }
    
    var count = 0
    var ddd = new scala.collection.mutable.Queue[(String, Int)]
    
    
    def calc(c: Int, q: scala.collection.mutable.Queue[(String, Int)]): Unit = {
        if(q.isEmpty) return
        val a = q.dequeue
        val v = a._2 - c
        if(v > 0){
          q += ((a._1, v));
          count = count + c
        } else{
          count = count + a._2;
          ddd += ((a._1, count));
        }
        
        
        calc(c, q)
        
    }
}