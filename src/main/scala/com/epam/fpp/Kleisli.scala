package com.epam.fpp

sealed trait Kleisli[M[+_], -A, +B] { self =>
  import Kleisli.kleisli

  def run(a: A): M[B]

  def >=>[C](k: Kleisli[M, B, C])(implicit b: Monad[M]): Kleisli[M, A, C] =  kleisli((a: A) => b.bind(this(a))(k(_)))
  def andThen[C](k: Kleisli[M, B, C])(implicit b: Monad[M]): Kleisli[M, A, C] = this >=> k

  def <=<[C](k: Kleisli[M, C, A])(implicit b: Monad[M]): Kleisli[M, C, B] = k >=> this
  def compose[C](k: Kleisli[M, C, A])(implicit b: Monad[M]): Kleisli[M, C, B] = k >=> this

  def =<<[AA <: A](a: M[AA])(implicit m: Monad[M]): M[B] = m.bind(a)(run _)

  def map[C](f: B => C)(implicit M: Functor[M]): Kleisli[M, A, C] = kleisli(a => M.map(run(a))(f))

  def flatMap[C, AA <: A](f: B => Kleisli[M, AA, C])(implicit M: Monad[M]): Kleisli[M, AA, C] = kleisli((r: AA) => M.bind[B, C](run(r))(((b: B) => f(b).run(r))))
}

object Kleisli {
  def apply[M[+_], A, B](f: A => M[B]): Kleisli[M, A, B] = kleisli(f)

  def kleisli[M[+_], A, B](f: A => M[B]): Kleisli[M, A, B] =
    new Kleisli[M, A, B] {
      def run(a: A) = f(a)
    }

  implicit def kleisliFn[M[+_], A, B](k: Kleisli[M, A, B]): A => M[B] = (a: A) => k.run(a)
}