package com.epam.fpp

import com.epam.fpp.utils.Unapply2

trait Category[=>:[_, _]] extends Compose[=>:] { self =>
  def id[A]: A =>: A
}

object Category {
  @inline def apply[F[_, _]](implicit F: Category[F]): Category[F] = F
}

trait CategoryTypeClass[F[_, _],A, B] extends BaseTypeClass[F[A, B], Category[F]] {
}

trait ToCategoryTypeClass {
  implicit def toCategoryFunctions[F[_, _],A, B](v: F[A, B])(implicit F0: Category[F]) =
    new CategoryTypeClass[F,A, B] {
      def self = v
      implicit def F: Category[F] = F0
    }

  implicit def toCategoryFunctionsUnapply[FA](v: FA)(implicit F0: Unapply2[Category, FA]) =
    new CategoryTypeClass[F0.M,F0.A,F0.B] {
      def self = F0(v)
      implicit def F: Category[F0.M] = F0.TC
    }
}