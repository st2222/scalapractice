import scala.io.StdIn
import scala.math.sqrt

object Main{
    def main(args: Array[String]){
        val sc = new java.util.Scanner(System.in)
        val n = sc.nextInt // 最初の数字を読み取る
        val list = List.fill(n)(sc.nextInt) // nextIntがn回呼ばれる
        var count = 0;
        for(j <- list)if(isPrime(j)) count = count + 1
        println(count)
    }
    
    def isPrime(x: Int): Boolean = {
        if(x == 2){
            return true
        }
        
        if(x < 2 || x % 2 == 0){
            return false
        }
        
        var i = 3;
        while(i <= sqrt(x)){
            if(x % i == 0){
                return false
            }
            i += 2
        }
        return true
    }
}