sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

def size(tree:Tree[A]):Int = t match{
    case Leaf(_) => 1
    case Branch(l,f) => 1 + size(l) + size(r)
}

/*def treeMaximum(tree:Tree[Int]):Int = t match{
    case Branch(l,f) =>{
        treeMaximum(l)
        treeMaximum(r)
    }
    case Leaf
}*/
//此处的递归思想

def treeMaximum(tree:Tree[Int]):Int = t match{
    case Leaf(n) => n
    case Branch(l,r) => treeMaximum(l) max treeMaximum(r)
}

def treeDepth(tree:Tree[Int]):Int = t match{
    case Leaf(_) => 0
    case Branch(l,r) =>treeDepth(l) max treeDepth(r)
}

def mapTree(tree:Tree[Int])(f:Int=>Int):Tree[Int]={
    case Leaf(t)=>Leaf(f(t))
    //case Branch(l,r)=>mapTree(l)+mapTree(r)
    case Branch(l,r)=>Branch(mapTree(l)(f),mapTree(r)(f))
}
