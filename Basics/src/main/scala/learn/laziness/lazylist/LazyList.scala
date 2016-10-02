package learn.laziness.lazylist

import learn.laziness.lazylist

/**
  * Created by Ganger on 10/2/2016.
  */
sealed trait LazyList[+A] {
  def takeWhile(p: A => Boolean): LazyList[A] = {
    if (this == Nil)
      Nil
    else {
      val cons = this.asInstanceOf[Cons[A]]
      if (!p(cons.head.apply))
        Nil
      else
        LazyList.cons[A](cons.head.apply, cons.tail.apply.takeWhile(p))
    }
  }

  override def toString(): String = {
    def getItems(l: LazyList[A]): String = {
      if (this == Nil)
        ""
      else {
        val cons = this.asInstanceOf[Cons[A]]
        s"${cons.head.apply.toString}${if(cons.tail.apply != Nil)" : " else ""}${cons.tail.apply.toString}"
      }
    }
    s"${getItems(this)}"
  }
}
case object Nil extends LazyList[Nothing]
case class Cons[+A](head: () => A, tail: () => LazyList[A]) extends LazyList[A]

object LazyList {
  def cons[A](head: => A, tail: => LazyList[A]): LazyList[A] = {
    lazy val hd = head
    lazy val tl = tail
    Cons(() => hd, () => tl)
  }

  def apply[A](as: A*): LazyList[A] =
    if (as.isEmpty) Nil
    else cons(as.head, apply(as.tail: _*))

}