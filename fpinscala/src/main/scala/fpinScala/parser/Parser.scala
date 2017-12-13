def char(c: Char): Parser[Char]

def run[A](p: Parser[A])(input: String): Either[ParseError, A]

//to make it with a trait
trait Parsers[ParseError, Parser[+_]] {        //Parser is a type that itself is a covariant type constructor
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]
    def char(c: Char): Parser[Char]
}

def many[A](p: Parser[A]): Parser[List[A]]

def map[A,B](a: Parser[A])(f: A=> B): Parser[B]

//here is the parser
map(many(char('a')))(_.size)
//add many and map to ParserOps:
val numA: Parser[Int] = char('a').many.map(_.size)

//structure preserving of map:
map(p)(a => a) == p

def slice[A](p: Parser[A]): Parser[String]
