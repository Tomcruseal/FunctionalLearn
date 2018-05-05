package example.cats

def head[A](xs: List[A]): A = xs(0)
//A can be Int, or even a case class!parametric polymorphism

//below is subtype, since different types have different interpretation of plus
trait PlusIntType[A] {
    def plus(a: A): A
}
def plusBySubtype[A <: PlusIntType[A]](a1: A, a2: A): A = a2.plus(a1)

//below is ad-hoc polymorphism, difficult
trait PlusImpType[A] {
    def plus(a1: A, a2: A): A
}
def plusByAdhoc[A: PlusIntType](a1: A, a2: A): A = implicit[PlusImpType[A]].plus(a1, a2)

def sum(xs: List[Int]): Int = xs.foldLeft(0)(_ + _)
//if define an Int momoid for plus:
trait Monoid[A] {
    def op(a: A, b: A): A
    def id: A
}
object IntMonoid extends Monoid[Int] {
    def op(a: Int, b: Int): Int = a + b
    def id: Int = 0
}

//By introducing monoid into sum
def sumMonoid(xs: List[Int]): Int = xs.foldLeft(IntMonoid.id)(IntMonoid.op)
def sumMonoidA[A](xs: List[A], m: Monoid[A]): A = xs.foldLeft(m.id)(m.op)

//if make Monoid umplicit:
def sumImpMonoid[A](xs: List[A])(implicit m: Monoid): A = xs.foldLeft(m.id)(m.op)
