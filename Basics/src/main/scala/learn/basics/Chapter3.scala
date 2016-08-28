package learn.basics

import org.slf4j.LoggerFactory

/**
  * Created by ganger on 8/24/2016.
  */

sealed trait Stack[+A]
case object Empty extends Stack[Nothing]

case class StackImpl[A](top: A, tail: Stack[A]) extends Stack[A] {
  override def toString: String = {
    s"${top.toString}::${tail.toString}"
  }
}

object Stack {
  def push[A](item: A, tail: Stack[A]): Stack[A] = {
    new StackImpl[A](item, tail)
  }
  def peek[A](head: Stack[A]): A = {
    head.asInstanceOf[StackImpl[A]].top
  }
  def pop[A](head: Stack[A]): Stack[A] = {
    head.asInstanceOf[StackImpl[A]].tail
  }
  def apply[A](a: A): Stack[A] = {
    Stack.push[A](a, Empty)
  }
}

object TestStack {
  val logger = LoggerFactory.getLogger(TestStack.getClass)

  def main(args: Array[String]): Unit = {
    var stack = Stack[Int](7)
    logger.info(s"${stack}")
    stack = Stack.push(5, stack)
    logger.info(s"${stack}")
    stack = Stack.push(6, stack)
    logger.info(s"${stack}")
    stack = Stack.push(7, stack)
    logger.info(s"${stack}")
    val top = Stack.peek(stack)
    logger.info(s"${top}")
    stack = Stack.pop(stack)
    logger.info(s"${stack}")
    stack = Stack.pop(stack)
    logger.info(s"${stack}")
    stack = Stack.pop(stack)
    logger.info(s"${stack}")
  }
}