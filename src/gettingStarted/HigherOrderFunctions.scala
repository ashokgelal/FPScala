package gettingStarted

object HigherOrderFunctions {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def product(n1: Int, n2: Int): Int = {
    n1 * n2
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    val curried = curry(product) // curry function product

    val productOf2Times = curried(2)
    println(s"2 x 3 = ${productOf2Times(3)}")
    println(s"2 x 4 = ${productOf2Times(4)}")
    println(s"2 x 8 = ${productOf2Times(8)}")

    println()
    val uncurried = uncurry(curried)
    println(s"2 x 3 = ${uncurried(2, 3)}")
    println(s"2 x 4 = ${uncurried(2, 4)}")
    println(s"2 x 8 = ${uncurried(2, 8)}")

    println()
    val composed = compose((a: Int) => a / 5, (b: Int) => b * 10) // equivalent of 2nd func andThen 1st func
    println(s"3 * 10 / 5 = ${composed(3)}")
  }
}
