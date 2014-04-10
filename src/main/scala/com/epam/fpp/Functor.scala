package com.epam.fpp

import com.epam.fpp.utils.Unapply

trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def apply[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)
}

object Functor {
  @inline def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
}

trait FunctorTypeClass[F[_], A] extends BaseTypeClass[F[A], Functor[F]] {
  final def map[B](f: A => B): F[B] = F.map(self)(f)
  final def ∘[B](f: A => B): F[B] = F.map(self)(f)

  final def >|[B](b: => B): F[B] = F.map(self)(_ => b)
}

trait ToFunctorTypeClass {
  implicit def toFunctorFunctions[F[_],A](v: F[A])(implicit F0: Functor[F]) =
    new FunctorTypeClass[F,A] {
      def self = v
      implicit def F: Functor[F] = F0
    }

  implicit def toFunctorFunctionsUnapply[FA](v: FA)(implicit F0: Unapply[Functor, FA]) =
    new FunctorTypeClass[F0.M,F0.A] {
      def self = F0(v)
      implicit def F: Functor[F0.M] = F0.TC
    }
}
