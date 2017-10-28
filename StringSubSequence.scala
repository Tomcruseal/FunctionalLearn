def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forall {
        case (h,h2) => h==h2
    }
//目前没有forAll这个关键词了

/*第一个case就跑错了……
def tails: Stream[Stream[A]] = 
    unfold(this){
        case Cons(h, t) => Some((h(), t()))
        case Cons(h, empty) => Some((h(), empty))
        case _ => None
    }
*/

def tail: Stream[Stream[A]] = 
    unfold(this){
        case Empty => None
        case s => Some((s,s drop 1))
    } append Stream(empty)


def hasSubsequence[A](s: Stream[A]): Boolean = 
    tails exists (_ startsWith s)

/*def hasSubsequence[A](s: Stream[A]): Boolean = 
    this.tails exists match p => p
        case (_.startsWith(s)) 
*/