sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
//where Left stands for false case,Right stands fot true case

def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty)
        Left("mean of empty list")
    else
        Right(xs.sum/xs.length)

def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch {case e: Exception => Left(e)}
}

//Similarly,the Either version of Try
def Try(a: => A): Either[Exception, A]  = {
    try Right(a)
    catch {case e: Exception => Left(e)}
}

//下面代码直接复制于答案，因为懒得写了
def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
  es match {
    case Nil => Right(Nil)
    case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
  }

def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
  es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 
  traverse(es)(x => x)