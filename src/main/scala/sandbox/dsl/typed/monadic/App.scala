package sandbox.dsl.typed.monadic

object App {

  import cats.instances.option._
  import Expr._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def main(args: Array[String]): Unit = {

    val interpreter = new GenericInterpreter[Option]()

    val program: Expr[Boolean] = for {
      a      <- pure(7)
      b      <- pure(2)
      r1     <- add(a, b)
      c      <- pure(7)
      d      <- pure(2)
      r2     <- mul(c, d)
      result <- gte(r1, r2)
    } yield result

    println(interpreter.eval(program))
 }

}
