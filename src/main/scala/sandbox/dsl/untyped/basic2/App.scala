package sandbox.dsl.basic2.untyped

object App {

  import cats.instances.option._

  def main(args: Array[String]): Unit = {
    val expression = Add((Mul(Num(2), Num(3))), Num(4))

    val result = PlainInterpreter.eval(expression)

    println(result)

    val applicativeOptionInterpreter = new GenericInterpreter[Option]

    val otherOptionResult = applicativeOptionInterpreter.eval(expression)

    println(otherOptionResult)
  }
}
