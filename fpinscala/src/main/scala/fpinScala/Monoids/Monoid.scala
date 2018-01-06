//below is a monoid
trait Monoid[A] {
    def op(a1: A, a2: A): A    //binary operation, associative
    def id: A                  //identity
}

//below is a string monoid
val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val id = ""
}

//below is a List monoid
def listMonoid[A] = new Monoid[List[A]] {            //note the type parameter
    def op(a1: List[A], a2: List[A]) = a1 ++ a2    //++ equals to append???
    val id = Nil
}

//integer addition monoid
val intAdditionMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val id = 0
}

//integer multiplication monoid
val intMultiplicationMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val id = 1
}

//bool or monoid
val booleanOrMonoid = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val id = false
}

//bool and monoid
val booleanAndMonoid = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val id = true
}

//Option monoid
//error!Do you really think there is a "++" method in Option?
//recall getOrElse
/*
def optonMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 ++ a2
    val id = None
}
*/
def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val id = None
}
//more introduction to the dual in the answer keys

//a function having the same argument and return type is called an endofunction
def endoMonoid[A]: Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2
    val id = (x: A) => x    //Good job! f(x)=x
}

def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop