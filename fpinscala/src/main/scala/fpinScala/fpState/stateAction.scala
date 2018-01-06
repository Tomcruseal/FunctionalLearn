type Rand[+A] = RNG => (A, RNG)
// important here!!!
val int: Rand[Int] = _.nextInt

//maybe for Double:
val double: Rand[Double] = _.nextDouble    //我之前有写过nextDouble吗？

def unit[A](a: A): Rand[A] = 
    rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
    rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
    }
//改变某状态的输出值而不更改状态本身

def nonNegativeEven: Rand[Int] = 
    map(nonNegativeInt)(i => i-i % 2)


//more elegant way to implement doulbe(exercise 6.2)
def doubleRandom3: Rand[Double] = 
    map(nonNegativeInt)(i => (i/(Int.MaxValue.toDouble+1)))

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
        val (a, r1) = ra(rng)
        val (b, r2) = rb(r1)
        (f(a,b),r2)
    }
// since the results are random anyway

def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = 
    map2(ra,rb)((_, _))

val randIntDouble: Rand[(Int, Double)] = 
    both(int, double)

val randDoubleInt: Rand[(Double,Int)] = 
    both(double, int)

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f,acc)(_ :: _))
//maybe it is better to implement this above via foldLeft

def ints(count: Int): Rand[List[A]] = 
    sequence(List.fill(count)(int))

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
        val (a, r1) = f(rng)
        g(a)(r1)
    }

//书本86页示例有错？

def nonNegativeLessThan(n: Int): Rand[Int] = {    // generate an integer n \in [0,n)
    faltMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
}
//unit:passes the RNG state without using it ,always returning a constant value rather than a random value

def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
}
//a

def map2ViaflatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))  
}
//先用flatMap映射（传递）一次状态