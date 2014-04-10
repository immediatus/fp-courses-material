package com.epam.fpp

import com.epam.fpp.utils.Unapply2

trait Compose[=>:[_, _]] {
  def compose[A, B, C](f: B =>: C, g: A =>: B): A =>: C
}

object Compose {
  @inline def apply[F[_, _]](implicit F: Compose[F]): Compose[F] = F
}

trait ComposeTypeClass[F[_, _],A, B] extends BaseTypeClass[F[A, B], Compose[F]] {
  final def <<<[C](x: F[C, A]): F[C, B] = F.compose(self, x)
  final def >>>[C](x: F[B, C]): F[A, C] = F.compose(x, self)
}

trait ToComposeTypeClass {
  implicit def toComposeFunctions[F[_, _],A, B](v: F[A, B])(implicit F0: Compose[F]) =
    new ComposeTypeClass[F,A, B] {
      def self = v
      implicit def F: Compose[F] = F0
    }

  implicit def toComposeFunctionsUnapply[FA](v: FA)(implicit F0: Unapply2[Compose, FA]) =
    new ComposeTypeClass[F0.M,F0.A,F0.B] {
      def self = F0(v)
      implicit def F: Compose[F0.M] = F0.TC
    }
}



