package gettingStarted

import scala.annotation.tailrec

object Fibonacci {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, curr, prev + curr)
    }
    loop(n, 0, 2)
  }

  def main(args: Array[String]) {
    println(fib(5))
  }
}
