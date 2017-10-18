def foldRightStream[B](z: => B)(f(A, => B)): B= this match{
    case Cons(h,t) => f(h(),t().foldRightStream(z)(f))
    case _ => z
}

def checkExist[A](p: A => Boolean): Boolean={
    foldRight(false)((a,b) => p(a) || b)    //这个方法太巧妙了有木有
}

def forAll(p: A => Boolean): Boolean={
    foldRight(true)(a,,b) => p(a) && b)    //
}
