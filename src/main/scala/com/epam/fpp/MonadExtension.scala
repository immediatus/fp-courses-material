package com.epam.fpp

import com.epam.fpp.utils.{Unapply, Liskov}

trait MonadExt[F[_]] extends Monad[F] { self =>
  def empty[A]: F[A]
  def filter[A](fa: F[A])(f: A => Boolean):F[A] = bind(fa)(a => if (f(a)) point(a) else empty[A])
}

object MonadExt {
  @inline def apply[F[_]](implicit F: MonadExt[F]): MonadExt[F] = F

  @inline def sequenceM[F[_], A](as: List[F[A]])(implicit F: Monad[F]): F[List[A]] =
    (as :\ F.point(List[A]())) {
      (v, acc) => F.bind(v)(
        (x: A) => F.bind(acc)(
          (ys: List[A]) => F.point(x :: ys)))
  }

  @inline def filterM[F[_], A](f: A => F[Boolean])(as: List[A])(implicit F: Monad[F]): F[List[A]] = 
    as match {
      case Nil => F.point(List[A]())
      case x :: xs =>
        F.bind(f(x))(
          (p: Boolean) => F.bind(filterM(f)(xs))(
            (ys: List[A]) => F.point(if(p) x :: ys else ys)))
    }
}

trait MonadExtTypeClass[F[_],A] extends BaseTypeClass[F[A], MonadExt[F]] {
  import Liskov.<~<

  def filter(f: A => Boolean) = F.filter(self)(f)
  def withFilter(f: A => Boolean) = F.filter(self)(f)
}

trait ToMonadExtTypeClass extends ToMonadTypeClass {
  implicit def toMonadExtFunctions[F[_],A](v: F[A])(implicit F0: MonadExt[F]) =
    new MonadExtTypeClass[F,A] {
      def self = v
      implicit def F: MonadExt[F] = F0
    }

  implicit def toMonadExtFunctionsUnapply[FA](v: FA)(implicit F0: Unapply[MonadExt, FA]) =
    new MonadExtTypeClass[F0.M,F0.A] {
      def self = F0(v)
      implicit def F: MonadExt[F0.M] = F0.TC
    }
}

