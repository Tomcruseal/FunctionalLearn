package example.cats

trait Monoid[A] {
    def op(a: A, aa: A): A 
    def id(a: A): A
}

object Monoid {
    implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
        def op(a: Int, aa: Int): Int = a + b
        def id(a: Int): Int = 0
    }

    implicit val StringMonoid: Monoid[String] = new Monoid[String] {
        def op(a: String, aa: String): Stirng = a + b
        def id(a: String): String = ""
    }

}

def sum[A: Monoid](xs: List[A): A = {
    val m = implicitly[Monoid[A]]
    xs.foldLeft(m.id)(m.op)
}

object foldLeftList {
    def foldLeft[A, B](xs: List[A], z: B, f: (B, A) => B): B = xs.foldLeft(z)(f)
}

//below is FoldLeft typeclass
trait FoldLeft [F[_]]{
     def foldLeft[A, B](xs: F[_], z: B, f: (B, A) => B): B
}

object FoldLeft extends FoldLeft{
    implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List]
    def foldLeftList[A, B](xs: List[A], z: B, f: (B, A) => B)
}

def sum[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
    val m = implicitly[M[A]]
    val fl = implicitly[Monoid[A]]
    fl.foldLeft(xs, m.id, m.op)
}