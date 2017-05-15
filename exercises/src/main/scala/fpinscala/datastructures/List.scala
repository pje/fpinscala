package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def foldRight[A, B](as: List[A], memo: B)(f: (A, B) => B): B = as match {
    case Nil => memo
    case Cons(x, xs) => f(x, foldRight(xs, memo)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
   if (n <= 0) l
    else l match {
     case Nil => Nil
     case Cons(x, xs) => drop(xs, n - 1)
   }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  def foldLeft[A,B](l: List[A], memo: B)(f: (B, A) => B): B = l match {
    case Nil => memo
    case Cons(x, xs) => foldLeft(xs, f(memo, x))(f)
  }

  def sumFL(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productFL(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthFL[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  def foldRightFL[A,B](l: List[A], memo: B)(f: (A,B) => B): B = foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(memo)

  def reverse[A](l: List[A]): List[A] = List.foldLeft(l, Nil:List[A])((x, y) => Cons(y, x))

  def appendFL[A](a1: List[A], a2: List[A]): List[A] = foldRightFL(a1, a2)((x, y) => Cons(x, y))

  def concat[A](ls: List[List[A]]): List[A] = foldRightFL(ls, Nil:List[A])(append)

  def addOneToEach(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOneToEach(xs))
  }

  def allToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, allToString(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightFL(l, Nil:List[B])((m, v) => Cons(f(m), v))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) => filter(xs)(f)
    case _ => l
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def zip(l1: List[Int], l2: List[Int], memo: List[Int] = Nil): List[Int] = (l1, l2) match {
    case (Cons(x, xs), Cons(y, ys)) => zip(xs, ys, Cons(x + y, memo))
    case (_, _) => append(reverse(memo), append(l1, l2))
  }

  def zipWith[A,B](l1: List[A], l2: List[A], memo: List[B] = Nil)(f: (A, A) => B): List[B] = (l1, l2) match {
    case (Cons(x, xs), Cons(y, ys)) => zipWith(xs, ys, Cons(f(x, y), memo))(f)
    case (_, _) => reverse(memo)
  }
}
