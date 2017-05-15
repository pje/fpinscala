package fpinscala.errorhandling

import scala.{Either => _, Option => _, Some => _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = Option.mean(xs) match {
    case None => None
    case Some(m) => Some(xs.map(x => math.pow(x - m, 2)).sum / xs.length)
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}