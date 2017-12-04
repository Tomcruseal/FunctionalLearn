package fpinScala.fpErrors.Option

sealed trait Option[+A]
case class Some[+A] (get: A) extends Option[+A]
case object None extends Option[Nothing]

def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}

def map[B](f: A => B): Option[B] = this match{
    case None => None
    case Some(a) => Some(f(a))
}
/*
def flatMap[B](f: A => Option[B]): Option[B] ={
    map(f(a)) getOrElse None
}
the parameters of the function map should be a fucntion!
*/

def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
}
// and anthoer solution using explicit pattern matching

def flatMap2[B](f: A => Option[B]): Option[B] = this  {
    case None => None
    case Some(a) => f(a)
}

def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
}
def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map(Some(_)) getOrElse ob
}

def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match{
    case None => ob
    case _ => this
}

def filter(f: A=> Boolean): Option[A] = this match{
    case Some(a) if f(a) => this
    case _ => None
}

/*
def variance(xs: Seq[Double]): Option[Double] = {
    val m = this.Mean()
    xs.flatMap(math.pow(xs-m,2))    //flatMap()参数应为函数
}
//is "this" really solid here
*/

def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x-m,2))))
}

def variance_right(xs: Seq[Double]): Option[Double] = 
    mean(xs) flatmap (m => mean(xs.map(x => math.pow(x-m,2))))
//  Important

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

//try to lift funciton abs

val absO: Option[Double] => Option[Double] = lift(math.abs)

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double

def parseInsuranceRateQuote{
    age: String,
    numberOfSpeedingTickets: String
    val optAge: Option[Int] = Try{age.toInt}
    val optTickets: Option[Int] = Try{numberOfSpeedingTickets.toInt}
    map2(optAge, optTickets)(insuranceRateQuote)
}

def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e : Exception => None}
}

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa =>
    b map (bb => 
    f(aa, bb)))
}
//the answer on github is not user friendly.
//map has higher priority than => since => begins with
//= ???

def sequence[A](a: List[Option[A]]): Option[List[A]] = a match{
    case Nil => Some(Nil)
    case h::t => h flatMap (hh => sequence(t) map (hh::_))
}