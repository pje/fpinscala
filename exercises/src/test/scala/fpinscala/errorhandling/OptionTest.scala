package fpinscala.errorhandling

import org.specs2.mutable.Specification

class OptionTest extends Specification {
  "#map" >> {
    def f(x: Int): Int =  x + 1

    Some(1).map(f) ==== Some(2)
    None.map(f) ==== None
  }

  "#getOrElse" >> {
    Some(1).getOrElse(2) ==== 1
    None.getOrElse(2) ==== 2
  }

  "#flatMap" >> {
    def f(x: Int): Option[Int] = Some(x + 1)

    Some(1).flatMap(f) ==== Some(2)
    None.flatMap(f) ==== None
  }

  "#orElse" >> {
    Some(1).orElse(Some(2)) ==== Some(1)
    None.orElse(Some(2)) ==== Some(2)
  }

  "#filter" >> {
    def f(x: Int): Boolean = x == 1

    Some(1).filter(f) ==== Some(1)
    Some(2).filter(f) ==== None
    None.filter(f) ==== None
  }

  ".variance" >> {
    Option.variance(Seq(1.0, 2.0, 3.0, 4.0)) ==== Some(1.25)
    Option.variance(Seq()) ==== None
  }

  ".map2" >> {
    def f(x: Int, y: Int): Int = x + y

    Option.map2(Some(1), Some(2))(f) ==== Some(3)
    Option.map2[Int,Int,Int](None, Some(2))(f) ==== None
    Option.map2(Some(2), None)(f) ==== None
    Option.map2(None, None)(f) ==== None
  }

  ".lift" >> {
    def f(x: Int): Int = x + 1

    Option.lift(f)(Some(1)) ==== Some(2)
    Option.lift(f)(None) ==== None
  }

  ".sequence" >> {
    Option.sequence(List(Some(1), Some(2), Some(3))) ==== Some(List(1, 2, 3))
    Option.sequence(List(Some(1), Some(2), None)) ==== None
  }
}
