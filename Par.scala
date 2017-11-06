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
}