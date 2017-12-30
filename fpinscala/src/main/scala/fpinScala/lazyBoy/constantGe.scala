def constant[A](a: A): Stream[A] = {
    Stream.cons(a,constant(a))
}

//below is a more efficient version

def constant1[A](a: A): Stream[A] ={
    lazy val tail: Stream[A] = Cons(() => a,() => tail)
    tail
}

def from(n: Int): Stream[Int] = {
    Sream.cons(n,from(n+1))
}
//any trick?


/*def fibsStream(): Stream[A] = {
    
}*/

val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = {
        Stream.cons(f0,go(f1,f0+f1))
    }
    go(0,1)
}

//below is difficult since I haven't understand the meaning of unfold
def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = z match{
    case Nil => None
    case Some(_) => Stream.cons(z,unfold(f(z)))
}

def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = 
    f(z) match {
        case Some((h,s)) => Stream.cons(h,unfold(s)(f))
        case None => empty
    }
//每一次产生一个新的值并更新状态

val fibs = {
    unfold((0,1)) {case (f0,f1) => Some((f0,(f1,f0+f1))) }  //here is equivalent to p => p match {case (f0,f1) => Some(f0,(f1,f0+f1))}
}
//每次解开一个括号并向后。可否unfold(0)作为initial?

def from_unfold(n: Int): Stream[Int]={
    unfold(n)(n => Some(n,n+1))
} 

/*
def constant_unfold[A](a: A): Stream[A]={
    unfold(a)(a => Some(a,a))    //a replaced by _?
}
*/

def constant_unfold[A](a: A): Stream[A]={
    unfold(a)(_ => Some(a,a))   
}

//val ones: Stream[Int] = unfold(1)(1 => Some(1,1))  //可以不要前面的声明  1 replaced by _

val ones: Stream[Int] = unfold(1)(_ => Some(1,1))

def map_unfold[B](f: A => B): Stream[B]={
    unfold(this)(p => p match{
        case Cons(h,t) => Some((f(h),t()))
        case _ => None
    })
}


//注意unfold函数，下面不需要再自己实现递归
def take_unfold(n: Int): Stream[A] = {
    unfold(this,n)(p => p match{
        case (Cons(h,t),n) => if n>1 Some((h(),(t(),n-1)))
        case (Cons(h,t),1) => if n==1 Some((h(),(empty,0)))
        case _ => None 
    })
}

def takeWhile_unfold(f: A => Boolean): Stream[A] = {
    unfold(this)
        case Cons(h,t) => if f(h) Some((h(),t()))
        case _ => None
}


//{}代表block，上述写法是否合法？

def zipWith_unfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold((this,s2)){
        case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1,h2),(t1(),t2())))
        case _ => None
    }

def zip[B](s2: Stream[B]): Stream[(A,B)] = 
    zipWith(s2)((_,_))

def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = 
    zipWIthAll(s2)((_,_))

def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = 
    Stream.unfoled(this,s2){
        case (Empty, Empty) => None
        case ((Cons(h,t), Empty)) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
        case (Empty, Cons(h,t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
        case (Cons(h1,t1), Cons(h2,t2)) => Some(f(Some(h1()),Some(h2())) -> (t1() -> t2()))
    }
//TODO
//zipWithAll seems hard to understand where -> indicates key -> value in a map.