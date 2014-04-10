package com.epam.fpp

import com.epam.fpp.utils.Unapply

trait Applicative[F[_]] extends Functor[F] with Pointed[F] { self =>
  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]
  def ap2[A,B,C](fa: => F[A], fb: => F[B])(f: F[(A,B) => C]): F[C] = ap(fb)(ap(fa)(map(f)(_.curried)))
  def ap3[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(f: F[(A,B,C) => D]): F[D] = ap(fc)(ap2(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C) => f(a,b,c)))))
  def ap4[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: F[(A,B,C,D) => E]): F[E] = ap2(fc, fd)(ap2(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C, d:D) => f(a,b,c,d)))))

//def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] = ap2(fa, fb)(point(f))
  def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(f.curried))
  def apply3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: (A, B, C) => D): F[D] = apply2(apply2(fa, fb)((_, _)), fc)((ab, c) => f(ab._1, ab._2, c))
  def apply4[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: (A, B, C, D) => E): F[E] = apply2(apply2(fa, fb)((_, _)), apply2(fc, fd)((_, _)))((t, d) => f(t._1, t._2, d._1, d._2))

  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] = apply2(_, _)(f)
  def lift3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] = apply3(_, _, _)(f)
  def lift4[A, B, C, D, E](f: (A, B, C, D) => E): (F[A], F[B], F[C], F[D]) => F[E] = apply4(_, _, _, _)(f)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(point(f))
}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
}

trait ApplicativeTypeClass[F[_],A] extends BaseTypeClass[F[A], Applicative[F]] {
  final def <*>[B](f: F[A => B]): F[B] = F.ap(self)(f)
  final def *>[B](fb: F[B]): F[B] = F.apply2(self,fb)((_,b) => b)
  final def <*[B](fb: F[B]): F[A] = F.apply2(self,fb)((a,_) => a)

  final def |@|[B](fb: F[B]) = new ApplicativeBuilder[F, A, B] {
    val a: F[A] = self
    val b: F[B] = fb
  }
  final def ⊛[B](fb: F[B]) = |@|(fb)
}

trait ToApplicativeTypeClass extends ToFunctorTypeClass {
  implicit def toApplicativeFunctions[F[_],A](v: F[A])(implicit F0: Applicative[F]) =
    new ApplicativeTypeClass[F,A] {
      def self = v
      implicit def F: Applicative[F] = F0
    }

  implicit def toApplicativeFunctionsUnapply[FA](v: FA)(implicit F0: Unapply[Applicative, FA]) =
    new ApplicativeTypeClass[F0.M,F0.A] {
      def self = F0(v)
      implicit def F: Applicative[F0.M] = F0.TC
    }

  implicit def lift2[F[_],A,B,C](f: (A,B) => C)(implicit F: Applicative[F]) = F.lift2(f)
}
