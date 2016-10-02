package learn.laziness.nonlazylist

/**
  * Created by Ganger on 10/2/2016.
  */
sealed trait NonLazyList[+A] {
  def takeWhile(p: A => Boolean): NonLazyList[A] = {
    if (this == Nil)
      Nil
    else {
      val cons = this.asInstanceOf[Cons[A]]
      if (!p(cons.head))
        Nil
      else
        Cons(cons.head, cons.tail.takeWhile(p))
    }
  }

  override def toString(): String = {
    def getItems(l: NonLazyList[A]): String = {
      if (this == Nil)
        ""
      else {
        val cons = this.asInstanceOf[Cons[A]]
        s"${cons.head.toString}${if(cons.tail != Nil)" : " else ""}${cons.tail.toString}"
      }
    }
    s"${getItems(this)}"
  }
}
case object Nil extends NonLazyList[Nothing]
case class Cons[+A](head: A, tail: NonLazyList[A]) extends NonLazyList[A]

object NonLazyList {
  def apply[A](as: A*): NonLazyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}