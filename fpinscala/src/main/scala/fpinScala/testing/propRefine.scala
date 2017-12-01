package fpScala.testing.propRefine

trait Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

type TestCase = Int
//type Result = Either[(FailedCase, SuccessCount), SuccessCount]

case class Prop(run: TestCase => Result)

//since there is no need to know the SuccessCount if the test pass
//so it is better to remove the rightmost argument

type Result = Either[(FailedCase, SuccessCount)]

//now make a new data type

sealed trait Result {
    def isFalsified: Boolean
}

case object Passed extends Result {
    def isFalsified = false
}

case class Falsified(failure: FailedCase,
                     success: SuccessCount) extends Result {
    def isFalsified = true  
}

//to implement function forAll, RNG is needed
case class Prop(run: (TestCase, RNG) => Result)
//then the following functions can be implemented

def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a,i) => try {
            if (f(a)) Passed else Falsified(a.toString,i)
        } catch {case e: Exception => Falsified(buildMsg(a,e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
}

def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = 
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

def buildMsg[A](s: A, e: Exception): String = 
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"