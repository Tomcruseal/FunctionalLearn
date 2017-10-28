def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng2) = rng.nextInt
    if n1 >=0
        (n1, rng2)
    else
        nonNegativeInt(rng2)
}
//val应该替换成var?

def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (i,r) = rng.nextInt
    (if (i < 0) -(i+1) else i,r)
}
//Int.MinValue is 1 smaller than -(Int.MaxValue)

def doubleRandom(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    val d : Double = i.toDouble/(Int.MaxValue.toDouble)
    (d, r)
}

//考虑到reuse

def doubleRandom2(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (1/(Int.MaxValue.toDouble + 1),r)
}

def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i ,r) = rng.nextInt
    val (d, r2) = doubleRandom(r)
    ((i, d), r2)
}

def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = doubleRandom(rng)
    val (i, r2) = rng.nextInt
    ((d, i), r2) 
}

// issues for reuse

def doubleInt2(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
}

def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = doubleRandom(rng)
    val (d2, r2) = doubleRandom(r1)
    val (d3, r3) = doubleRandom(r2)
    ((d1, d2, d3), r3)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count<=0)
        (List(),RNG)
    else{
        val (x, r) = rng.nextInt
        val (xs, r2) = ints(count - 1)(r)
        ((x::xs), r2) 
    }
}

//How to apply tail recursion?

