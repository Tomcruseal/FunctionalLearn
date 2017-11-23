package fpinScala.Fibonacci
object Fibonacci{
    def fib(n:Int):Int={
        @annotation.tailrec
        def fibCal(n:Int,num1:Int,num2:Int):Int={
            if (n==1) num1
            else fibCal(n-1,num2,num1+num2)
        }
        fibCal(n,0,1)
    }

    def main(args:Array[String]):Unit={
        println(fib(6))
    }
}