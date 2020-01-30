import scala.io.StdIn
import scala.collection.mutable.Stack


object Main{
    def main(args: Array[String]) = {
        val array = StdIn.readLine.split(" ")
        val stack = new Stack[String]
        for(value <- array){
            value match {
                case "+" => stack.push((stack.pop().toInt + stack.pop().toInt).toString)
                case "-" => {
                  var a = stack.pop().toInt;var b = stack.pop().toInt
                  stack.push((b - a).toString)
                }
                case "/" => {
                  var a = stack.pop().toInt;var b = stack.pop().toInt
                  stack.push((b / a).toString)
                }
                case "*" => stack.push((stack.pop().toInt * stack.pop().toInt).toString)
                case _ => stack.push(value)
            }
        }
        println(stack.pop())
        
    }
}
