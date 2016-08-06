package main.scala.ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /* Exercise 3.25
   * Write a function size that counts the number of nodes in a tree
   */
  def size[A](as: Tree[A]): Int = {
    as match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  /* Exercise 3.26
   * Write a function maximum that returns the maximum element in a Tree[Int]
   */
  def maximum(as: Tree[Int]): Int = {
    def loop(xs: Tree[Int], maxNum: Int): Int = {
      xs match {
        case Leaf(a) => a max maxNum
        case Branch(l, r) => loop(l, maxNum) max loop(r, maxNum)
      }
    }
    loop(as, -1000)
  }
  // Better way. Don't even need to calculate max twice
  def maximum2(as: Tree[Int]): Int = {
    as match {
      case Leaf(a) => a
      case Branch(l, r) => maximum2(l) max maximum2(r)
    }
  }

  /* Exercise 3.27
   * Write a function depth that return the maximum path length from the
   * root of a tree to any leaf
   */
  def depth[A](as: Tree[A]): Int = {
    as match {
      case Leaf(_) => 0 // If only the root is depth 0
      case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
      // Above can be factored to 1 + (depth(l) + depth(r))
    }
  }
}

object MainTree {
  def main(args: Array[String]) = {
    println(Tree.size(Leaf(0))) // 1
    println(Tree.size(Branch(Leaf(1), Leaf(2)))) // 3
    println(Tree.size(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))) // 7

    println(1 max 2) // Returns the max of the pair

    println(Tree.maximum(Leaf(0))) // 0
    println(Tree.maximum(Branch(Leaf(1), Leaf(2)))) // 2
    println(Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))) // 4

    println()

    println(Tree.depth(Leaf(0))) // 0
    println(Tree.depth(Branch(Leaf(1), Leaf(2)))) // 1
    println(Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))))) // 3

  }
}