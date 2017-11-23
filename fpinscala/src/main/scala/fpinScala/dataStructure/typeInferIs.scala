object typeInferIs{
    def foldRightCus[A,B](list1:List[A],accum:B)(f:(A,B)=>B):B=list1 match{
        case Nil => accum
        case Cons(x,xs) => f(x,foldRightCus(xs,accum)(f))
    }

    def sumFoldRight(list1:List[Int])={
        foldRightCus(list1,0)((x,y) => x+y)
    }

    def productFoldRight(list1:List[Double])={
        foldRightCus(list1,1)((x,y) => x*y)
    }

    def length[A](as:List[A]):Int={
        foldRightCus(as,0)((x,y) => y+1)  //x should be replaced by _
    }

    /*def foldLeftCus[A,B](list1:List[A],accum:B)(f:(B,A)=>B):B=list1 match{
        case Nil => accum
        case Cons(h,t)=>f(t,foldLeftCus(h,accum)(f))    //TODO
    }*/

    def foldLeftCus[A,B](list1:List[A],accum:B)(f:(B,A)=>B):B=list1 match{
        case Nil => accum
        case Cons(h,t) => foldLeftCus(t,f(accum,h))
    }

    def sumFoldLeft(list1:List[Int])={
        foldLeftCus(list1,0)((y,x) => y+x)
    }

    def productFoldLeft(list1:List[Int])={
        foldLeftCus(list1,1)((y,x) => y*x )
    }

    def reverseList(list1:List[A]):{
        foldLeftCus(list1,Nil)((y,x) => (Cons(x,y)))    //Nil should be replaced by List[A]()
    }

    def foldRightCus2[A,B](list1:List[A],accum:B)(f:(A,B) => B):B= list1 match{
        
    }    //very very difficult,may some of the students can solve it

    def appendCus[A](list1:List[A],list2:List[A]):List[A]={
        foldRightCus(list1,list2)((x,y)=>Cons(x,y))    //将Nil替换成list2
    }

    def listConcatenates(list1:List[A]):List[A]={
        foldRightCus(list1,Nil:List[A])(appendCus)    //上一个函数，线性        
    }

}