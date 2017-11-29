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
