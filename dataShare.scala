object dataShare{
    def drop[A](l:List[A],n:Int):List[A]= {
        /*@annotation.tailrec
        def dropHelp[A](l:List[A],n:int):List[A]=l match{
            case Cons(_,t)=>t
            case Nil=> sys.error("length not enough")
        }
        dropHelp(l,n)

        }
    }*/
        if (n<=0) l
        else l match{
            case Nil => sys.error("length not enough")
            case Cons(_,t) => drop(t,n-1)
        }
    }

    def dropWhile[A](l:List[A],f:A=>Boolean):List[A]=l match{
        case Nil => Nil
        case Cons(h,t) if f(h) => dropWhile(t,f)
    }

    def dropWhile2[A](as:List[A])(f:A=>Boolean):List[A]=as match{   //type inference
        case Cons(h,t) if f(h) => dropWhile2(t)(f)
        case _  => as
    }
    val listInput: List[Int] = List(1,2,3,4,5)
    val resList = dropWhile2(listInput)(x=>x<3)  //匿名函数类型推断不需声明x的类型
}