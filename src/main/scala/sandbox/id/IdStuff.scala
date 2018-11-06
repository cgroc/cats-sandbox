package sandbox.id

import cats.{Id, Monad}

object IdStuff {

  val idMonad: Monad[Id] = new Monad[Id] {
    def pure[A](x: A): Id[A] = x

    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = ??? // ouch
  }

}
