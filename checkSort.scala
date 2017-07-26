object checkSort{
    def isSorted[A](as:Array[A],ordered:(A,A)=>Boolean):Boolean={
        @annotation.tailrec
        def dualCompare(n:Int):Int={
            if (!=ordered(as(n),as(n+1))) -1
            else if (n>=as.length) 1
            else dualCompare(n+1)
        }
        dualCompare(0)
    }

    
}