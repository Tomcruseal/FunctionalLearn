type Rand[+A] = RNG => (A, RNG)
// important here!!!
val int: Rand[Int] = _.nextInt

//maybe for Double:
val double: Rand[Double] = _.nextDouble    //我之前有些过nextDouble吗？

def unit[A](a: A): Rand[A] = 
    rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
    rng => {
        val (a, rng2) = a(rng)
        (f(a), rng)
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