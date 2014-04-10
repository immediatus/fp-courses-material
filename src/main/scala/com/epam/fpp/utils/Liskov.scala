package com.epam.fpp.utils

import com.epam.fpp.Category

trait Liskov[-A, +B] {
  def subst[F[-_]](p: F[B]): F[A]

  def apply(a: A): B = Liskov.witness(this)(a)
  final def *[+[+_, +_], C, D](that: Liskov[C, D]): Liskov[A + C, B + D] = Liskov.lift2(this, that)
  final def andThen[C](that: Liskov[B, C]): Liskov[A, C] = Liskov.trans(that, this)
  final def compose[C](that: Liskov[C, A]): Liskov[C, B] = Liskov.trans(this, that)
}

object Liskov extends LiskovInstances with LiskovFunctions {
  type <~<[-A, +B] = Liskov[A, B]
  type >~>[+B, -A] = Liskov[A, B]
}

trait LiskovInstances {
  import Liskov._

  implicit def liskov: Category[<~<] = new Category[<~<] {
    def id[A]: (A <~< A) = refl[A]
    def compose[A, B, C](bc: B <~< C, ab: A <~< B): (A <~< C) = trans(bc, ab)
  }
}

trait LiskovFunctions {
  import Liskov._

  implicit def refl[A]: (A <~< A) = new (A <~< A) {
    def subst[F[_]](p: F[A]): F[A] = p
  }

  implicit def witness[A, B](lt: A <~< B): A => B = {
    type f[-X] = X => B
    lt.subst[f](identity)
  }

  def trans[A, B, C](f: B <~< C, g: A <~< B): A <~< C = g.subst[({type λ[-α]= α <~< C})#λ](f)

  def lift2[T[+_, +_], A, A2, B, B2](a: A <~< A2, b: B <~< B2): (T[A, B] <~< T[A2, B2]) = {
    type a[-X] = T[X, B2] <~< T[A2, B2]
    type b[-X] = T[A, X] <~< T[A2, B2]
    b.subst[b](a.subst[a](refl))
  }
}