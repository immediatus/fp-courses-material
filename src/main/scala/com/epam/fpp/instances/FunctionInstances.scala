package com.epam.fpp.instances

import com.epam.fpp.{Arrow ,Functor, Monad, Category}

trait FunctionInstances {

  implicit val function0Instance = new Monad[Function0] {
    def point[A](a: => A) = () => a
    def bind[A, B](fa: () => A)(f: (A) => () => B) = () => f(fa())()
    override def map[A,B](fa: () => A)(f: A => B) = () => f(fa())
  }

  implicit val function1Instance = new Category[Function1] with Arrow[Function1] {
    def compose[A, B, C](f: B => C, g: A => B) = f compose g
    def id[A]: A => A = a => a

    def arr[A, B](f: A => B) = f
    def first[A, B, C](a: A => B) =(ac: (A, C)) => (a(ac._1), ac._2)
  }

  implicit def function1Covariant[T]: Monad[({type l[a] = (T => a)})#l] = new Monad[({type l[a] = (T => a)})#l] {
    def point[A](a: => A) = _ => a
    def bind[A, B](fa: T => A)(f: A => T => B) = (t: T) => f(fa(t))(t)
  }

  implicit def function2Instance[T1, T2] = new Monad[({type l[a] = ((T1, T2) => a)})#l] {
    def point[A](a: => A) = (t1, t2) => a
    def bind[A, B](fa: (T1, T2) => A)(f: (A) => (T1, T2) => B) = (t1, t2) => f(fa(t1, t2))(t1, t2)
  }

  implicit def function3Instance[T1, T2, T3] = new Monad[({type l[a] = ((T1, T2, T3) => a)})#l] {
    def point[A](a: => A) = (t1, t2, t3) => a
    def bind[A, B](fa: (T1, T2, T3) => A)(f: (A) => (T1, T2, T3) => B) = (t1, t2, t3) => f(fa(t1, t2, t3))(t1, t2, t3)
  }
}
