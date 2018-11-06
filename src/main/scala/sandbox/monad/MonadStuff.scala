package sandbox.monad

import cats.Monad
//import cats.Id

import cats.instances.option._
import cats.instances.future._
import cats.instances.try_._
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Success, Try}

object MonadStuff {

  def monadAdd[M[_]](ma: M[Int], mb: M[Int])(implicit M: Monad[M]): M[Int] =
    M.flatMap(ma)(
      a => M.map(mb)(
        b => a + b
      )
    )

  // Note in order to do the for-comp you need the 2 syntax imports
  // import cats.syntax.functor._
  // import cats.syntax.flatMap._
  def monadAdd2[M[_]: Monad](ma: M[Int], mb: M[Int]): M[Int] =
    for {
      a <- ma
      b <- mb
    } yield a + b


  def main(args: Array[String]): Unit = {

    val res = monadAdd(Option(3), Option(4))
    val res2 = monadAdd2(Option(8), Option(4))

    println(res)
    println(res2)

    val futureRes = monadAdd(Future(1), Future(2))

    println(Await.result(futureRes, 1.second))

    val success1: Try[Int] = Success(4)
    val success2: Try[Int] = Success(5)
//      val fail =
    val tryRes = monadAdd(success1, success2)

    println(tryRes)

//    println(monadAdd(123, 321))

  }
}
