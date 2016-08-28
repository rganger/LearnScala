package learn.basics

import org.slf4j.LoggerFactory

/**
  * Created by Ganger on 8/27/2016.
  */
sealed trait BinaryTree[+A]
case object Empty extends BinaryTree[Nothing]
case class Node[A](node: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]
//case class Builder[A](sorter: (A, A) => Boolean, node: A, tree: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {
  def sortCommon[A](first: A, second: A): Boolean = {
    (first, second) match {
      case (f: Int, s: Int) =>
        f < s
      case (f: Double, s: Double) =>
        f < s
      case (f: Float, s: Float) =>
        f < s
      case (f: Boolean, s: Boolean) =>
        if (f == true && s == false) true else false
      case (_) => {
        first.toString.compareTo(second.toString) < 0
      }
    }
  }

  def construct[A](sorter: (A, A) => Boolean, a: A, tree: BinaryTree[A]): BinaryTree[A] = {
    // Starting at the top reconstruct the tree until the insert point is found
    // If the data indicates that we're not going down a particular path, it is safe to attach the old path
    // When we find the insert point, create a new node and attach the previous and next nodes as the left and right
    // Now, we are complete and simply need to return the new top node
    if (tree == Empty) {
      Empty
    } else {
      val node = tree.asInstanceOf[Node[A]]
      if (sorter(a, node.node)) {
        Node[A](a, node, Empty)
      } else {
        construct[A](sorter, node.node, node.left)
      }
    }
  }

  def apply[A](sorter: (A, A) => Boolean, as: A*): BinaryTree[A] = {
    if (as.isEmpty) {
      Empty
    } else {
      construct(sorter, as.head, apply(sorter, as.tail: _*))
    }
  }

  def apply[A](as: A*): BinaryTree[A] = {
    val  defaultSorter: (A, A) => Boolean = sortCommon
    if (as.isEmpty) {
      Empty
    } else {
      construct(defaultSorter, as.head, apply(defaultSorter, as.tail: _*))
    }
  }

  def add[A](sorter: (A, A) => Boolean, a: A, bt: BinaryTree[A]): BinaryTree[A] = {
    construct(sorter, a, bt)
  }

  def add[A](a: A, bt: BinaryTree[A]): BinaryTree[A] = {
    val  defaultSorter: (A, A) => Boolean = sortCommon
    construct(defaultSorter, a, bt)
  }
}



object Chapter3ver2 {
  val logger = LoggerFactory.getLogger(getClass)
  def main(args: Array[String]): Unit = {
    val tree = BinaryTree[Int](1,2,-3,5,-8,9)
    logger.info(s"Tree: ${tree}")
  }
}
