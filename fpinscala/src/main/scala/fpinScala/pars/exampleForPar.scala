adef sumOfList(a: IndexedSeq[Int]): Int = {
    if (a.size<=1)
        a headOption getOrElse 0
    else{
        val (l, r) = a.splitAt(a.length/2)
        sum(l) + sum(r)
    }
}

def unit[A](a: =>): Par[A]

def get[A](Par[Int]): A

def parsumOfList(a: IndexedSeq[Int]): Int = {
    if (a.size<=1)
        a headOption getOrElse 0
    else{
        val (l, r) = a.splitAt(a.length/2)
        val lp: Par[Int] = Par.unit(sum(l))
        val rp:L Par[Int] = Par.unit(sum(r))
        Par.get(lp) + par.get(rp)

    }
}
