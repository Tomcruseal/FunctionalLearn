object typeInferIs{
    def foldRightCus[A,B](list1:List[A],accum:B)(f:(A,B)=>B):B=list1 match{
        case Nil => accum
        case Cons(x,xs) => f(x,foldRightCus(xs,accum)(f))
    }

    def sumFold(list1:List[Int])={
        foldRightCus(list1,0)((x,y) => x+y)
    }

    def product(list1:List[Double])={
        foldRightCus(list1,1)((x,y) => x*y)
    }

    def length[A](as:List[A]):Int={
        foldRightCus(as,0)((x,y) => y+1)  //x should be replaced by _
    }

    def foldLeftCus[A,B](list1:List[A],accum:B)(f:(B,A)=>B):B=list1 match{
        case Nil => accum
        case Cons(h,t)=>f(t,foldLeftCus(h,accum)(f))    //TODO
    }
} 