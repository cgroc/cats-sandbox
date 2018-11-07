package sandbox.dsl.basic1.untyped

object App {

  import cats.instances.option._

  def main(args: Array[String]): Unit = {
    val expression = Add((Mul(Num(2), Num(3))), Num(4))

    val result = PlainInterpreter.eval(expression)

    println(result)

    val optionInterpreter = new GenericMonadInterpreter[Option]

    val optionResult = optionInterpreter.eval(expression)

    println(optionResult)

    val applicativeOptionInterpreter = new GenericApplicativeInterpreter[Option]

    val otherOptionResult = applicativeOptionInterpreter.eval(expression)

    println(otherOptionResult)
  }
}
