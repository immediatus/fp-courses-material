package com.epam.fpp

import com.epam.fpp.utils.Unapply

trait Pointed[F[_]] { self =>
  def point[A](a: => A): F[A]
  def pure[A](a: => A): F[A] = point(a)  
}

object Pointed {
  @inline def apply[F[_]](implicit F: Pointed[F]): Pointed[F] = F
}

trait PointedTypeClass[F[_],A] extends BaseTypeClass[F[A], Pointed[F]] {
}

trait ToPointedTypeClass {
  implicit def toPointedFunctions[F[_],A](v: F[A])(implicit F0: Pointed[F]) =
    new PointedTypeClass[F,A] {
      def self = v
      implicit def F: Pointed[F] = F0
    }

  implicit def toPointedFunctionsUnapply[FA](v: FA)(implicit F0: Unapply[Pointed, FA]) =
    new PointedTypeClass[F0.M,F0.A] {
      def self = F0(v)
      implicit def F: Pointed[F0.M] = F0.TC
    }
}