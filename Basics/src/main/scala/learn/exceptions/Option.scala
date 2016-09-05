package learn.exceptions

/**
  * Created by Ganger on 9/4/2016.
  * Re-implementation of Option class to study its usage.
  */
sealed trait Option[+A] {
  // map applies function f to A (if not None), and returns Option[B]
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case s: Some[A] => Option(f(s.get))
    }
  }

  // flatmap applies function f to A (if not None), handles a failure of f, and returns Option[B]
  def flatmap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case s: Some[A] => f(s.get)
    }
  }

  // getOrElse returns A or a supertype B if not None, otherwise returns the default
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case s: Some[A] => s.get.asInstanceOf[B]
    }
  }

  // orElse returns A or a supertype B if not None, otherwise evaluates function ob
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case s: Some[A] => Option(s.get.asInstanceOf[B])
    }
  }

  // filter returns None if A does not satisfy function f
  def filter(f: A => Boolean): Option [A] = {
    this match {
      case None => None
      case s: Some[A] =>
        if (f(s.get))
          this
        else
          None
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

case object Option {
  def apply[A](get: A): Option[A] = {
    try {
      val result = get
      if (result != null && result != None)
        new Some[A](get).asInstanceOf[Option[A]]
      else
        None
    } catch {
      case _: Throwable => None
    }
  }
}
