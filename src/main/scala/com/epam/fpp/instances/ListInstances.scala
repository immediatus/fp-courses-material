package com.epam.fpp.instances

import com.epam.fpp.MonadExt

trait ListInstances {
  implicit val listInstance = new MonadExt[List] {
    def point[A](a: => A):List[A] = List(a)
    def bind[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def empty[A]: List[A] = List()

    override def map[A, B](l: List[A])(f: A => B) = l map f
  }
}
