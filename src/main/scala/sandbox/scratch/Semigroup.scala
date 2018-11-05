package sandbox.scratch

trait Semigroup[A] {

  def add(first: A, second: A): A
}

object Semigroup {

  implicit val intSemiGroup: Semigroup[Int] = new Semigroup[Int] {
    def add(first: Int, second: Int): Int =
      first + second
  }
}

trait Monoid[A] extends Semigroup[A] {

  def empty: A
}

object Monoid {

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0

    def add(first: Int, second: Int): Int = ???
  }
}