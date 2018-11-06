package sandbox.calc

import cats.Monad

object Ting {

  def parseInt(str: String): Either[String, Int] =
    try {
      Right(str.toInt)
    } catch {
      case exn: NumberFormatException =>
        Left("Bad number: " + str)
    }

  def parseInt[F[_]](str: String)(implicit M: Monad[F]): F[Int] =
    try {
      M.pure(str.toInt)
    } catch {
      case exn: NumberFormatException =>
        ??? //HUH? Holy crap we need a MonadError
    }
}
