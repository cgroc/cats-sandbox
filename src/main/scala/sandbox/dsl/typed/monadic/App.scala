package sandbox.dsl.typed.monadic

object App {

  import cats.instances.option._
  import Expr.exprMonad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def main(args: Array[String]): Unit = {

    val e1 = Add(Mul(Pure(2), Pure(3)), Pure(4))

    val interpreter = new GenericInterpreter[Option]()

    println(interpreter.eval(e1))

//    val res = for {
//      a <- Pure(1): Expr[Int]
//      b <- Pure(2)
//    } yield Add(a, b)
//
//    println(res)

//    val e2 = FlatMapExpr(Val(2), (i: Int) => Val(2 * i))
//
//    println(interpreter.eval(e2))
 }

}
