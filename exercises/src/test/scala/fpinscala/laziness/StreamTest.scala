package fpinscala.laziness

import org.specs2.mutable.Specification

class StreamTest extends Specification {
  "#toList" >> {
    Stream(1, 2, 3).toList ==== List(1, 2, 3)
  }

  "#take" >> {
    Stream(1, 2).take(0).toList ==== Nil
    Stream(1, 2).take(1).toList ==== List(1)
    Stream(1, 2).take(2).toList ==== List(1, 2)
    Stream(1, 2).take(3).toList ==== List(1, 2)
  }

  "#drop" >> {
    Stream(1, 2).drop(0).toList ==== List(1, 2)
    Stream(1, 2).drop(1).toList ==== List(2)
    Stream(1, 2).drop(2).toList ==== Nil
    Stream(1, 2).drop(3).toList ==== Nil
  }

  "#takeWhile" >> {
    def f(i: Int): Boolean = i > 1

    Stream(1).takeWhile(f).toList ==== Nil
    Stream(1, 2).takeWhile(f).toList ==== Nil
    Stream(2, 3).takeWhile(f).toList ==== List(2, 3)
  }

  "#forAll" >> {
    def f(x: Int): Boolean = x > 1

    Stream(2, 3).forAll(f) ==== true
  }

  ".ones" >> {
    Stream.ones.take(3).toList ==== List(1, 1, 1)
  }

  ".constant" >> {
    Stream.constant(1).take(3).toList ==== List(1, 1, 1)
  }

  ".from" >> {
    Stream.from(1).take(3).toList ==== List(1, 2, 3)
  }

  ".fibs" >> {
    Stream.fibs().take(6).toList ==== List(1, 1, 2, 3, 5, 8)
  }
}
