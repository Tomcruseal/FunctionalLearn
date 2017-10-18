/*
def take(n:Int,origion:Stream[A]):Stream[A]={
    @annotation.tailrec
    def go(n:Int,acc:Stream[A]):Stream[A]=acc match{
        if (n>=0){
            go(n-1,h()::acc)
        }
        else acc
    }
    go(n,origion)
}
what about the empty condition?
*/

def take2(n:Int):Stream=this match{
    case Cons(h,t) if n>1 => cons(h(),t.take2(n-1))  //t should be replaced by t() !!
    case Cons(h,t) if n==1 => cons(h(),empty)
    case _ => empty
}

def drop(n:Int):Stream = this match{
    case Cons(_,t) if n>0 => t().drop(n-1)
    case _ => this
}

def takeWhile(p: A => Boolean): Stream[A] = this match{
    case Cons(h,t) if p(h()) => cons(h(),t().takeWhile(p))
    case _ => empty
}