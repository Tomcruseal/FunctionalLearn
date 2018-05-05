//def map[A, B](oa: Option[A])(f: A => B): Option[A]

//Functor: apply a function to a wrapped value
trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
}

val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
}


trait Mon[F[_]] {
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = 
        fa flatMap (a => fb map (b => f(a, b)))
}


trait MonX[F[_]] {

    def map[A, B](fa: F[A])(f: A => B): F[B]

    def faltMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    
    //below infix expression is not allowed
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = 
        flatMap(fa)(a => map(fb)(b => f(a, b)))
}

//use unit and faltMap as the minimal set
trait MonY[F[_]] extends Functor[F] {
    def unit[A](fa: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = 
        faltMap(fa)(a => unit(f(a)))
    
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) = C): F[C] = 
        faltMap(fa)(a => map(fb)(b => f(a, b)))
}

//Monad is a proper subset of Functor

 //Monad for Gen
 object Monad {
     val genMonad = new Monad[Gen]{
         def unit[A](fa: => A): Gen[A] = Gen.unit(fa)
         def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = 
             fa flatMap f
     }
 }

 //Monad for Option
 object Monad {
     val optionMonad = new Monad[Option]{
         def unit[A](fa: => A): Option[A] = Option.unit(fa)
         def faltMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B]  =
             fa flatMap f
     }
 }
 