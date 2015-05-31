package dataStructures

sealed trait BTree[+A]

case class Leaf[A](value: A) extends BTree[A]

case class Branch[A](left: BTree[A], right: BTree[A]) extends BTree[A]

object BTree {

  def size[A](t: BTree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(t: BTree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: BTree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  def map[A, B](t: BTree[A])(f: A => B): BTree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}

object RunTree {

  def main(args: Array[String]) {
    val b1 = Branch(Leaf(8), Leaf(2))
    val b2 = Branch(Leaf(3), Leaf(6))
    val root = Branch(b1, b2)
    println(s"size(): ${BTree.size(root)}")
    println(s"max(): ${BTree.maximum(root)}")
    println(s"depth(): ${BTree.depth(root)}")
    println(s"map(): ${BTree.map(root)(_ + 2)}")
  }
}