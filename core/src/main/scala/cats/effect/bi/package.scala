package cats.effect

package object bi {
  type BiIO[+E, +A] = BiIO.Type[E, A]
  type IO[+A] = BiIO.Type[INothing, A]

  type INothing <: Nothing
}
