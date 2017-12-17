package fpinScala.parser

def product[A,B](p: Parser[A],p2: Parser[B]): Parser[(A,B)]  //tuple

def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = {
    f(product(p,p2))
}
//here is misleading, notice that f operated on type A,B
// not Parser[A] nor Parser[B]
//above is not correct!!!

def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = {
    map(product(p,p2))(f.tupled)
}

def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_::_)
}

//the parser for zero or more 'a' followed by one or more 'b':
char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
//** is equal to product

def many[A](p: Parser[A]): Parser[List[A]] = 
    map2(p, many(p))(_::_) or succeed(List())

//below is difficult
def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <=0 ) succeed(List())
    else map2(p,listOfN(n-1, p))(_ :: _)
}

//since the 2nd argument is strict, map can never terminate!!
//Because a call to map2 always evaluates its second argument

def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]

def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    map(product(p,p2))(f.tupled)
}