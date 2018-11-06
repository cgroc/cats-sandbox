package sandbox.monadtransformers

import cats.Monad
import cats.Monoid
import cats.data.EitherT
import cats.syntax.flatMap._  // for flatMap
import cats.syntax.functor._  // for map
import cats.syntax.monoid._   // for |+|
import cats.syntax.either._
import cats.instances.option._
import cats.instances.int._

object Transformers {

  def monadicAdd[F[_]: Monad, A: Monoid](x: F[A], y: F[A]): F[A] =
    for {
      a <- x
      b <- y
    } yield a |+| b

  def main(args: Array[String]): Unit = {
    val a: EitherT[Option, String, Int] = EitherT(Option(4.asRight[String]))
    val b: EitherT[Option, String, Int] = EitherT(Option(7.asRight[String]))
    val c: EitherT[Option, String, Int] = EitherT(Option("KABOOM".asLeft[Int]))

    println(monadicAdd(a, b))

    println(monadicAdd(b, c))
  }
}
