package fpinscala.state

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class StateTest extends Specification {
  import RNG._

  trait Context extends Scope {
    val r = Simple(42)
    val negativeGenerator = Simple(1059025964525L)
  }

  "#nextInt" in new Context {
    r.nextInt ==== (16159453, Simple(1059025964525L))
    negativeGenerator.nextInt ==== (-1281479697, Simple(197491923327988L))
  }

  ".nonNegativeInt" in new Context {
    nonNegativeInt(negativeGenerator)._1 ==== 1281479696
  }

  ".nextDouble" in new Context {
    nextDouble(r)._1 ==== 0.007524831689672932
  }

  ".intDouble" in new Context {
    intDouble(r)._1 ==== (16159453, 0.596735485175967)
  }

  ".doubleInt" in new Context {
    doubleInt(r)._1 ==== (0.007524831689672932, -1281479697)
  }

  ".double3" in new Context {
    double3(r)._1 ==== (0.007524831689672932, 0.596735485175967, 0.15846728401187216)
  }

  ".ints" in new Context {
    ints(0)(r)._1 ==== Nil
    ints(4)(r)._1 ==== List(-2015756020, -340305902, -1281479697, 16159453)
  }

  ".doubleViaMap" in new Context {
    doubleViaMap(r)._1 ==== 0.007524831689672932
  }

  ".randIntDouble" in new Context {
    randIntDouble(r)._1 ==== (16159453, 0.596735485175967)
    randIntDouble(r)._1 ==== intDouble(r)._1
  }

  ".randDoubleInt" in new Context {
    randDoubleInt(r)._1 ==== (0.007524831689672932, -1281479697)
    randDoubleInt(r)._1 ==== doubleInt(r)._1
  }

  ".sequence" in new Context {
    ok
  }

  ".flatMap" in new Context {
    ok
  }

  ".productIterator" in new Context {
    ok
  }

  ".productPrefix" in new Context {
    ok
  }

  ".seed" in new Context {
    ok
  }

  ".copy" in new Context {
    ok
  }

  ".copy$default$1" in new Context {
    ok
  }

  ".canEqual" in new Context {
    ok
  }

  ".equals" in new Context {
    ok
  }

  ".productArity" in new Context {
    ok
  }

  ".productElement" in new Context {
    ok
  }

  ".unapply" in new Context {
    ok
  }

  ".compose" in new Context {
    ok
  }

  ".apply" in new Context {
    ok
  }

  ".andThen" in new Context {
    ok
  }
}
