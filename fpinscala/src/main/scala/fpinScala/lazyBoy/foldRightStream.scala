def foldRightStream[B](z: => B)(f(A, => B)): B= this match{
    case Cons(h,t) => f(h(),t().foldRightStream(z)(f))
    case _ => z
}

def checkExist[A](p: A => Boolean): Boolean={
    foldRight(false)((a,b) => p(a) || b)    //这个方法太巧妙了有木有
}

def forAll(p: A => Boolean): Boolean={
    foldRight(true)((a,b) => p(a) && b)    //
}

def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h,t) => 
        if p(h) cons(h,t)
        else empty
    )
}

def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
}

def find(p: A => Boolean): Option[A] = 
    filter(p).headOption