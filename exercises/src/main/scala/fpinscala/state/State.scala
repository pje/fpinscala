package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = r => r.nextInt

  val double: Rand[Double] = r => RNG.nextDouble(r)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, nextRNG) = rng.nextInt
    (if (int < 0) Math.abs(int + 1) else int, nextRNG)
  }

  def nextDouble(rng: RNG): (Double, RNG) = {
    val (int, nextRNG) = nonNegativeInt(rng)
    (int.toDouble / Int.MaxValue, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, rng2) = rng.nextInt
    val (dbl, rng3) = nextDouble(rng2)
    ((int, dbl), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (dbl, rng2) = nextDouble(rng)
    val (int, rng3) = rng2.nextInt
    ((dbl, int), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (dbl1, rng2) = nextDouble(rng)
    val (dbl2, rng3) = nextDouble(rng2)
    val (dbl3, rng4) = nextDouble(rng3)
    ((dbl1, dbl2, dbl3), rng4)
  }

  def ints(count: Int, acc: List[Int] = Nil)(rng: RNG): (List[Int], RNG) = {
    if (count < 1) {
      (acc, rng)
    } else {
      val (i, r) = rng.nextInt
      ints(count - 1, i :: acc)(r)
    }
  }

  def doubleViaMap(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  def nonNegativeLessThan(n: Int): Rand[Int] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
