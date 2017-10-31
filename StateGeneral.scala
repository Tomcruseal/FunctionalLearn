type State[S, +A] = S => (A, S)

// may be below
case class State[S, +A](run: S => (A, S))

//make Rand a type alias for State:

type Rand[A] = State[RNG, A]