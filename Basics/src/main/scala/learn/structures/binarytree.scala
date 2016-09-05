package learn.structures

import org.slf4j.LoggerFactory

/**
  * Created by Ganger on 8/27/2016.
  */
sealed trait BinaryTree[+A]
case object Nil extends BinaryTree[Nothing]
case class Node[A](node: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {
  val logger = LoggerFactory.getLogger(getClass)
  def compareCommon[A](first: A, second: A): Int = {
    (first, second) match {
      case (f: Int, s: Int) => f.compareTo(s)
      case (f: Double, s: Double) => f.compareTo(s)
      case (f: Float, s: Float) => f.compareTo(s)
      case (f: Boolean, s: Boolean) => f.compareTo(s)
      case (_) => first.toString.compareTo(second.toString)
    }
  }

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

  def construct[A](comparator: (A, A) => Int, a: A, tree: BinaryTree[A]): BinaryTree[A] = {
    // Starting at the top reconstruct the tree until the insert point is found
    // If the data indicates that we're not going down a particular path, it is safe to attach the old path
    // When we find the insert point, create a new node and attach the previous and next nodes as the left and right
    // Now, we are complete and simply need to return the new top node
    if (tree == Nil) {
      new Node[A](a, Nil, Nil)
    } else {
      val head = tree.asInstanceOf[Node[A]]
      if (comparator(a, head.node) < 0) {
        new Node[A](head.node, construct(comparator, a, head.left), head.right)
      } else if (comparator(a, head.node) > 0) {
        new Node[A](head.node, head.left, construct(comparator, a, head.right))
      } else {
        head
      }
    }
  }

  def find[A](a: A, tree: BinaryTree[A]): Boolean = {
    find(compareCommon, a, tree)
  }

  def find[A](comparator: (A, A) => Int, a: A, tree: BinaryTree[A]): Boolean = {
    if (tree == Nil) {
      false
    } else {
      val headNode = tree.asInstanceOf[Node[A]]
      (comparator(a, headNode.node) == 0) || (find(comparator, a, headNode.left)) || (find(comparator, a, headNode.right))
    }
  }

  def apply[A](comparator: (A, A) => Int, as: A*): BinaryTree[A] = {
    if (as.isEmpty) {
      Nil
    } else {
      construct(comparator, as.head, apply(comparator, as.tail: _*))
    }
  }

  def apply[A](as: A*): BinaryTree[A] = {
    val  defaultComparator: (A, A) => Int = compareCommon
    if (as.isEmpty) {
      Nil
    } else {
      construct(defaultComparator, as.head, apply(defaultComparator, as.tail: _*))
    }
  }

  def add[A](comparator: (A, A) => Int, a: A, bt: BinaryTree[A]): BinaryTree[A] = {
    construct(comparator, a, bt)
  }

  def add[A](a: A, bt: BinaryTree[A]): BinaryTree[A] = {
    val  defaultComparator: (A, A) => Int = compareCommon
    construct(defaultComparator, a, bt)
  }
}



object Chapter3ver2 {
  val logger = LoggerFactory.getLogger(getClass)
  def main(args: Array[String]): Unit = {
    val tree = BinaryTree[Int](1,2,-3,5,-8,9)
    logger.info(s"Tree: ${tree}")
    logger.info(s"Find 2: ${BinaryTree.find(2, tree)}")
    logger.info(s"Find 6: ${BinaryTree.find(6, tree)}")
    logger.info(s"Find 9: ${BinaryTree.find(9, tree)}")
    logger.info(s"Find -8: ${BinaryTree.find(-8, tree)}")
    logger.info(s"Find -7: ${BinaryTree.find(-7, tree)}")
    val tree2 = BinaryTree.add(7, tree)
    logger.info(s"Tree2: ${tree2}")
    logger.info(s"Find2 7: ${BinaryTree.find(7, tree2)}")
  }
}
