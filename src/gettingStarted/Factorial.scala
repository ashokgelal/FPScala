package gettingStarted

import scala.annotation.tailrec

object Factorial {

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else loop(n - 1, n * acc)
    loop(n, 1)
  }

  def main(args: Array[String]) {
    println(factorial(5))
  }

}
