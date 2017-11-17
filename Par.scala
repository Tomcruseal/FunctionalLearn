object Par {
    def unit[A] (a:A): Par[A] = (es: ExecutionService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A]{
        def isDone = true
        def get(timeout: Long, units: TimeUnit) = get 
        def isCancelled = false
        def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
        (es: ExecutionService) => {
            val af = a(es)
            val bf = b(es)
            UnitFuture((f(af.get, bf.get)))
        }

    def fork[A](a: => Par[A]): Par[A] = 
        es => es.submit(new Callable[A] {
            def call = a(es).get 
        })

    def asyncF[A, B](f: A => B): A => Par[B] = 
        a => fork(unit(f(a)))    //think of the previous lazyUnit

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = 
        map2(parList, unit(()))((a, _) => a.sorted)

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = 
        map2(pa, unit(()))((a, _) => f(a))
    
    //below is difficult
    def sequence[A](ps: List[Par[A]]): Par[List[A]]
 
    def sequence_simple[A](l: List[Par[A]]): Par[List[A]] = 
        l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = 
        as match {
            case Nil => unit(Nil)
            case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _) 
        }
    //think more deeply needed

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork{
        val fbs: List[Par[B]] = ps.map(asynF(f))
        sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        val fbs: List[Par[List[A]]] = as.map(async((a: A) => if f(a) List(a) else List() ))
        map(sequence(fbs))(_.flatten)
    }

    //这里前后两个map并不一样

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = 
        es => 
          if (run(es)(cond).get) t(es)
          else f(es)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]): Par[A] = 
        es => 
          choices[n](es)

    //注意审题"Let us say that choiceN runs n, and then uses that to select a ..."
    //所以得先运算出indice

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = 
        es =>
          val indice = run(es)(n).get
          run(es)choices(indeice)
          //choices(indices)(es)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = 
        choiceN(map(cond)(indice => if indice 0 else 1)List(t, f))
        //将Par[Boolean] map 为Par[Int]

    def choiceMap[K, V](key :Par[K])(choices: Map[K, Par[V]]): Par[V] = 
        es =>
          val keys = run(es)(key).get
          run(es)choices(keys)

    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = 
        es => 
          val pas = run(es)(pa).get
          run(es)choices(pas)
}