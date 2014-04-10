package com.epam.fpp.instances

import com.epam.fpp.{Category, Arrow}

trait PartialFunctionInstances {
  implicit val partialFunctionInstance = new Category[PartialFunction] with Arrow[PartialFunction] {
    def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]) =
      new PartialFunction[A, C] {
        def apply(a: A): C = f(g(a))
        def isDefinedAt(a: A): Boolean = g.isDefinedAt(a) && f.isDefinedAt(g(a))
      }

    def id[A] = {
      case a => a
    }

    def arr[A, B](f: A => B) = {
      case a => f(a)
    }

    override def split[A, B, C, D](f: PartialFunction[A, B], g: PartialFunction[C, D]): PartialFunction[(A,  C), (B, D)] = {
      case (a, c) if f.isDefinedAt(a) && g.isDefinedAt(c) => (f(a), g(c))
    }

    def first[A, B, C](f: PartialFunction[A, B]): PartialFunction[(A, C), (B, C)] = {
      case (a, c) if f.isDefinedAt(a) => (f(a), c)
    }
  }
}
