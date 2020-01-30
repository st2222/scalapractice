object Main{
    def main(args: Array[String]) = {
        val sc = new java.util.Scanner(System.in)
        val n = sc.nextInt // 最初の数字を読み取る
        val list = Array.fill(n)(sc.nextInt) // nextIntがn回呼ばれる
        
        var max = list(1) - list(0)
        var min = list(0)
        for(i <- 1 to n -1){
            if(max < (list(i)-min)){
                max = (list(i)-min)
            }
            if(min > list(i)){
                min = list(i)
            }
        }
        println(max)
    }
}