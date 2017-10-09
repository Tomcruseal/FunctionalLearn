object dataStream{
    def addOne[A](list1:List[A]):List[A]={
         foldRight(list1,Nil:List[A])((x,y) => Cons(x+1,y))
    }

    def doubleToString(list1:List[Double]):String={
        foldRight(list1,Nil:List[String])((x,y) => Cons(x.toString,y))
    }

    def mapCus[A,B](as:List[A])(f: A => B):List[B]={
        foldRight(as,Nil:List[B])((x,y)=>Cons(f(x),y))
    }

    def filterCus[A](as:List[A])(f:A=>Boolean):List[A]={
        foldRight(as,Nil:List[B])
    }

    def flatMapCus[A,B](as:List[A])(f:A=>List[B]):List[B]={    //may be false
        foldRight(as,Nil:List[B])((x,y)=>Cons(f(x),y))
    }

    def filterCus1[A](as:List[A])(f:A=>Boolean):List[A]={
        flatMapCus(as)(a => if(f(a)) List(a) else Nil)
    }
}