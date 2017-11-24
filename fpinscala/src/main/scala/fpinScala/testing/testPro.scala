package fpScala.testing.testPro
/*
1. slice a list of integers into 2,the sum of the
2 lists should be equal to the sum of the original
list...
*/
/*
any other number except the max should be smaller than
the max
*/

trait Prop {
    def check: Boolean
    //def && (p: Boolean): Boolean
    def &&(p: Prop): Prop = new Prop{
        def check = Prop.this.check && p.check
    }
}

object Prop {
    type FailedCase = String
    type SuccessCount = Int
} 

trait PropE {
    def check: Either[(FailedCase, SuccessCount), SuccessCount]

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}

case class Gen[A](sample: State[RNG,A])

def choose (start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(_ => _%(stopExclusive-start) ))
}

def unit[A](a: =>A): Gen[A] = {
    Gen(State.unit(a))
}

def booleanGen: Gen[Boolean] = {
    Gen(State(RNG.boolean))    //?印象里并没有这个额
}

def listofN[A](n: Int, g: Gen[A]): Gen[List[A]]