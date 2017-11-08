sealed trait Input
case object Coin extends Input
case object Turn extends Input

type State[S, +A] = S => (A, S)
case class State[S, +A](run: S => (A, S))

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Dispenser{
    def update = (i: Input) => (m: Machine) => (i, m) match{
        case(_, Machine(_, 0, _)) => m
        case(Turn, Machine(true, _, _)) => m
        case(Coin, Machine(false, _, _)) => m
        case(Coin, Machine(true,candies, coins)) => Machine(false, candies, coins + 1)
        case(Turn, Machine(false,candies, coins)) => Machine(true, candies - 1, coins)  
    }
}

def get[S]: State[S, S] = State(s => (s, s))
def set[S](s: S): State[S, Unit] = State(_ => ((), s))


def simulateMachine(inputs: List[Input]): State(Machine, (Int, Int)) = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
}yield(s.coins, s.candies)
    
}

//lessons learned
/*  case class Machine即为一个状态，通过输入Input(Coin/Turn)->
当前状态m->下一状态完成一次状态转移，状态转移受控于依据题干条件设置
的模式匹配。函数simulateMachine 通过for循环完成对输入的依次执行。
该for循环每成功执行一次就会有糖果硬币的输出（我之前以为只输出halt的状态）
sequence将状态的某列表转换成列表的某状态（combine a List of transitions
into a single transition）map的第2个参数先执行更新再通过modify改变状态，将
inputs转换成了状态组成的列表，再通过sequence...,s后去当前状态，通过yield
输出糖果和硬币数目。挺难的~，之前并不想看标准答案然而卡住了 