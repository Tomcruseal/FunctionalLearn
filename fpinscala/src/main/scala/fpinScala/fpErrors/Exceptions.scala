package fpinScala.fpErrors.Exceptions

def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double = {
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length
}

def mean_2(xs.Seq[Double]): Double = {
    if (xs.isEmpty)
        throw new ArithmeticException("mean of empty lise!")
    else xs.sum / xs.length
}