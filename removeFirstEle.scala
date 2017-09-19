object removeFirstEle{
    def tail(lists:List[Int]):List[Int]=lists match{
        case Cons(_,t) => t
    }

    def setHead(lists:List[Int],altern:Int):List[Int]=lists match{
        case Nil => sys.error("empty list")
        case Cons(_,t) => Cons(altern,t) 
    }

    def main(args:Array[String]):Unit={
        val x=List(1,2,3,4,5)
        println(tail(x))
    }
}