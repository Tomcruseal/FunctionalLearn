object firstModule{
    def abs(n:Int)={
        if (n<0) -n
        else n
    }

    def factorial(n:Int):Int={
        @annotation.tailrec
        def products(n:Int,acc:Int):Int={
            if (n<=0) acc
            else products(n-1,n*acc)
        }
        products(n,1)
    }

    private def formatAbs(x:Int)={
        val msg="The absolute value of %d is %d"
        msg.format(x,abs(x))
    }

    private def formatFactorial(n:Int)={
        val msg="The factorial of %d is %d"
        msg.format(n,factorial(n))
    }

    def main(args:Array[String]):Unit={
        println(formatAbs(-233))
        println(formatFactorial(7))
    }
}