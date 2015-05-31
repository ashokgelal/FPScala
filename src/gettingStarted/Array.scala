package gettingStarted

import scala.annotation.tailrec

object Array {

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1 // didn't find
      else if (p(as(n))) n // found
      else loop(n + 1) // next
    loop(0)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true // reached the end
      else if (gt(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    val index = findFirst(scala.Array("abc", "def"), (str: String) => str == "def")
    println(index)
    val sorted = isSorted(scala.Array(1, 2, 3), (n1: Int, n2: Int) => n1 >= n2)
    println(sorted)
  }
}
