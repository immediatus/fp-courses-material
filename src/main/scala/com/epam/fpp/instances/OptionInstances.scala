package com.epam.fpp.instances

import com.epam.fpp.Monad

trait OptionInstances {
  implicit val optionInstance = new Monad[Option] {
    def point[A](a: => A) = Some(a)
    def bind[A, B](fa: Option[A])(f: A => Option[B]) = fa flatMap f
    def empty[A]: Option[A] = None

    override def map[A, B](fa: Option[A])(f: A => B) = fa map f

    override def ap[A, B](fa: => Option[A])(f: => Option[A => B]) =
      f match {
        case Some(f) => fa match {
          case Some(x) => Some(f(x))
          case None => None
        }
        case None => None
    }
  }
}
