package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A], acc: Int = 1): Int = t match {
    case Leaf(x) => acc
    case Branch(l, r) => depth(l, acc + 1) max depth(r, acc + 1)
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeF[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => l + r)
//  def maximumF(t: Tree[Int]): Int = fold(t)(_ => _)((l, r) => l max r)
//  def mapF[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {}
}
