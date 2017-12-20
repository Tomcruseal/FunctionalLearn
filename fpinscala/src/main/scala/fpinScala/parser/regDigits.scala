implicit def regex(r: Regex): Parser[String]

//return the number of 'a' characters read
for {
    digit <- "[0-9]+".r
    val n = digit.toInt
    _ <- listOfN(n, char('a'))
} yield n

//the above answer is not the optimal