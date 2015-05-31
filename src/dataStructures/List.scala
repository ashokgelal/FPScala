package dataStructures

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum(ints: List[Int]): Int = {
    foldRight(ints, 0)(_ + _) // equivalent to foldRight(ints, 0)((x, y) => x + y)
  }

  def product(ds: List[Double]): Double = {
    foldRight(ds, 1.0)(_ * _) // equivalent to foldRight(ds, 1.0)((x, y) => x * y)
  }

  def sumL(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }

  def productL(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)(_ * _)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], elem: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(elem, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def appendR[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  def lengthL[A](as: List[A]): Int = {
    foldLeft(as, 0)((x, _) => x + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
  }

  def doubletoString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new ListBuffer[B]
    def loop(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); loop(t)
    }
    loop(l)
    List(buf.toList: _*) // convert from the standard Scala list to our list
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new ListBuffer[A]
    def loop(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; loop(t)
    }
    loop(l)
    List(buf.toList: _*) // convert from the standard Scala list to our list
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
}

object RunList {
  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    println(list)
    println(List.tail(list))
    println(List.setHead(list, 4))
    println(List.drop(list, 2))
    println(List.dropWhile(list)(e => e < 4))
    println(List.append(list, List(6, 7, 8)))
    println(List.init(list))

    println(List.sum(List(1, 2, 3, 4)))
    println(List.product(List(1, 2, 3)))

    println(List.length(list))

    println(List.sumL(List(1, 2, 3, 4)))
    println(List.productL(List(1, 2, 3)))
    println(List.lengthL(list))

    println(List.reverse(list))
    println(List.appendR(list, List(6, 7, 8)))

    println(List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))


    println(s"add1(): ${List.add1(list)}")
    println(s"doubleToString(): ${List.doubletoString(List(4.5, 2.2, 1, 3))}")
    println(s"map(): ${List.map(list)(_ + 1)}")
  }
}
