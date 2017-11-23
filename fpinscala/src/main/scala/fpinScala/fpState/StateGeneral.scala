type State[S, +A] = S => (A, S)

// may be below
case class State[S, +A](run: S => (A, S))

//make Rand a type alias for State:

type Rand[A] = State[RNG, A]

def map[S, A, B](a: S => (A, S))(f: A => B): S => (B,S) = {
    state => {
        val (ax, state2) = a(state)
        (f(ax), state2)
    }

}