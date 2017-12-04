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

def listofN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State(RNG.ints(n)))
}
//答案是调用的sequence

def listofN2[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(list.fill(n)(g.sample)))
}
//emmmmm

def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a =>f(a).sample))
}//是否有更简洁的写法？

def listofN3[A](size: Int): Gen[List[A]] = {
    Gen.listofN(size,this)
}// it is just a method alias(I think it seems like a trick)

def listofN4[A](size: Gen[Int]): Gen[List[A]] = {
    size flatMap(a => listofN3(a))
} //Here exists a tuny bug, think about it

def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    if isOdd g1.sample.nextInt
        Gen(g1.sample)
    else Gen(g2.sample)
}

def listOf1[A](g: Gen[A]): Gen[List[A]] = {
    val a = g.choose.sample.nextInt
    if a != 0
        listofN(n)
    else
        
        
}