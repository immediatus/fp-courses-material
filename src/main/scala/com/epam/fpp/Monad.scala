package com.epam.fpp

import com.epam.fpp.utils.{Unapply, Liskov}

trait Monad[F[_]] extends Applicative[F] { self =>
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  def join[A](ffa: F[F[A]]) = bind(ffa)(a => a)

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = bind(f)(f => map(fa)(f))
  override def map[A,B](fa: F[A])(f: A => B) = bind(fa)(a => point(f(a)))
}

object Monad {
  @inline def apply[F[_]](implicit F: Monad[F]): Monad[F] = F
}

trait MonadTypeClass[F[_],A] extends BaseTypeClass[F[A], Monad[F]] {
  import Liskov.<~<

  def >>=[B](f: A => F[B]) = F.bind(self)(f)
  def flatMap[B](f: A => F[B]) = F.bind(self)(f)

  def join[B](implicit ev: A <~< F[B]): F[B] = F.bind(self)(ev(_))
  def μ[B](implicit ev: A <~< F[B]): F[B] = F.bind(self)(ev(_))

  def >>[B](b: => F[B]): F[B] = F.bind(self)(_ => b)
}

trait ToMonadTypeClass extends ToApplicativeTypeClass {
  implicit def toMonadFunctions[F[_],A](v: F[A])(implicit F0: Monad[F]) =
    new MonadTypeClass[F,A] {
      def self = v
      implicit def F: Monad[F] = F0
    }

  implicit def toMonadFunctionsUnapply[FA](v: FA)(implicit F0: Unapply[Monad, FA]) =
    new MonadTypeClass[F0.M,F0.A] {
      def self = F0(v)
      implicit def F: Monad[F0.M] = F0.TC
    }
}
