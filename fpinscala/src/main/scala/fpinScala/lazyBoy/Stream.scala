sealed trait Stream[+A]
case object Empty extends Strean[Nothing]
case class Cons[+A] (h: => A, t: => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]):Stream[A]={
        lazy val head = hd
        lazy val tail = tl
        Cons(=> head, => tail)
    }
    def empty[A]: Stream[A]=Empty

    def apply[A](as: A*):Stream[A]=
        if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*)
}

//force h explicitly via h()
def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
}