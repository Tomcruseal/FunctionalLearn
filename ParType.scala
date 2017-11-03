def unit[A](a: => A): Par[A]

def get[A](a: Par[A]): A

def sum(ints: IndexedSeq[Int]): Int = 
    if (ints.size <= 1)
        ints.headOption getOrElse 0
    else {
        val (l, r) = ints.splitAt(ints.length/2)
        val sumL: Par[Int] = Par.unit(sum(l))
        val sumR: Par[Int] = Par.unit(sum(r))
        Par.get(sumL) + Par.get(sumR)
    }

//if don't call get
def sum(ints: IndexedSeq[Int]): Par[Int] = 
    if (ints.size <= 1)
        Par.unit(ints.headOption getOrElse 0)
    else {
        val (l, r) = ints.splitAt(ints.length/2)
        Par.map2(sum(l) + sum(r))(_ + _)
    }

//the signature of map2
map2[A](l: Par[A], r: Par[A])(f: (A, A) => A ): Par[A]