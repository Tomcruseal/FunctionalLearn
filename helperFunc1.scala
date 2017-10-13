/*
def toList[A](as:A*): List[A]= match{
    case empty => List[Nothing]
    case cons(as.head,as.tail) => Cons(as.head,as.tail)
}
these above may be false
*/

def toList:List[A]=this match{
    case Cons(h,t) => h()::t().toList  //:: assocaite to the right,which is equal to Cons(h(),t().toList)  explicit forcing of the h thunk using h()
    case _ => List()
}

//上述函数未作尾递归优化

def toListTailrec:List[A]={
    @annotation.tailrec
    def helper(st:Stream[A],as:List[A]):List[A]=st match{
        case Cons(h,t) => helper(t(),h()::as)
        case _ => as
    }
    helper(this,List()).reverse  //此处列表是逆序，需要反转
}