package com.epam.fpp

import com.epam.fpp.utils.Unapply2

trait Arrow[=>:[_, _]] extends Category[=>:] { self =>
  def arr[A, B](f: A => B): A =>: B

  def <<<[A, B, C](fbc: (B =>: C), fab: (A =>: B)): =>:[A, C] = compose(fbc, fab)
  def >>>[A, B, C](fab: (A =>: B), fbc: (B =>: C)): (A =>: C) = compose(fbc, fab)

  def first[A, B, C](f: (A =>: B)): ((A, C) =>: (B, C))
  def second[A, B, C](f: (A =>: B)): ((C, A) =>: (C, B)) = {
    def swap[X, Y] = arr[(X, Y), (Y, X)] { case (x, y) => (y, x) }
    >>>(<<<(first[A, B, C](f), swap), swap)
  }

  def split[A, B, C, D](f: A =>: B, g: C =>: D): ((A,  C) =>: (B, D)) = >>>(first[A, B, C](f), second[C, D, B](g))
  def combine[A, B, C](fab: (A =>: B), fac: (A =>: C)): (A =>: (B, C)) = >>>(arr((a: A) => (a, a)), split(fab, fac))
  def product[A, B](fab: (A =>: B)): ((A, A) =>: (B, B)) = split(fab, fab)
}

object Arrow {
  @inline def apply[F[_, _]](implicit F: Arrow[F]): Arrow[F] = F
}

trait ArrowTypeClass[F[_, _],A, B] extends BaseTypeClass[F[A, B], Arrow[F]] {
  implicit def F: Arrow[F]

  final def first[C]: F[(A, C), (B, C)] = F.first(self)
  final def second[C]: F[(C, A), (C, B)] = F.second(self)

  final def ***[C, D](k: F[C, D]): F[(A, C), (B, D)] = F.split(self, k)
  final def &&&[C](k: F[A, C]): F[A, (B, C)] = F.combine(self, k)

  final def product: F[(A, A), (B, B)] = F.product(self)
}

trait ToArrowTypeClass extends ToCategoryTypeClass {
  implicit def toArrowTypeClass[F[_, _],A, B](v: F[A, B])(implicit F0: Arrow[F]) =
      new ArrowTypeClass[F,A, B] { def self = v; implicit def F: Arrow[F] = F0 }

  implicit def toArrowTypeClassUnapply[FA](v: FA)(implicit F0: Unapply2[Arrow, FA]) =
      new ArrowTypeClass[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Arrow[F0.M] = F0.TC }
}
