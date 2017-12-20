def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = {
    faltMap(p)(a => map(p2)(b => (a,b)))
}

//hint: function map is allowed

def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = {
    for {a <- p; b<- p2} yield f(a,b)
}
//Notice the comma 
//多个statements放在一行需要;

//below express map in term of flatMap
def map[A,B](p: Parser[A])(f: A => B): Parser[B] = {
    flatMap(p)(a => succeed(f(a)))    
}