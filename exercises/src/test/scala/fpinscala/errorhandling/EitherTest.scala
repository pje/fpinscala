package fpinscala.errorhandling

import org.specs2.mutable.Specification

class EitherTest extends Specification {
  "#map" >> {
    def f(x: Int): Int = x + 1

    Right(1).map(f) ==== Right(2)
    Left("wat").map(f) ==== Left("wat")
  }

  "#flatMap" >> {
    def f(x: Int): Right[Int] = Right(x + 1)

    Right(1).flatMap(f) ==== Right(2)
    Left("wat").flatMap(f) ==== Left("wat")
  }

  "#orElse" >> {
    Right("yes").orElse(Right("hello")) ==== Right("yes")
    Left("wat").orElse(Right("hello")) ==== Right("hello")
  }

  "#map2" >> {
    def f(x: Int, y: Int): Int = x + y

    Right(1).map2(Right(2))(f) ==== Right(3)
    Right(1).map2(Left("no"))(f) ==== Left("no")
  }
}
