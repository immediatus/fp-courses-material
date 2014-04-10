package com.epam.fpp.instances

import com.epam.fpp.{Functor, Applicative, Monad, Kleisli}

trait KleisliInstances {

  implicit def kleisliFunctor[F[+_], R](implicit F0: Functor[F]): Functor[({type λ[α] = Kleisli[F, R, α]})#λ] =
    new KleisliFunctor[F, R] {
      implicit def F: Functor[F] = F0
    }

  implicit def kleisliApplicative[F[+_], R](implicit F0: Applicative[F]): Applicative[({type λ[α] = Kleisli[F, R, α]})#λ] =
    new KleisliApplicative[F, R] {
      implicit def F: Applicative[F] = F0
    }

  implicit def kleisliMonad[F[+_], R](implicit F0: Monad[F]) =
    new KleisliMonad[F, R] {
      implicit def F: Monad[F] = F0
    }

  trait KleisliFunctor[F[+_], R] extends Functor[({type λ[α] = Kleisli[F, R, α]})#λ] {
    implicit def F: Functor[F]
    override def map[A, B](fa: Kleisli[F, R, A])(f: A => B): Kleisli[F, R, B] = fa map f
  }

  trait KleisliApplicative[F[+_], R] extends Applicative[({type λ[α] = Kleisli[F, R, α]})#λ] {
    implicit def F: Applicative[F]

    import Kleisli.kleisli

		override def point[A](a: => A): Kleisli[F, R, A] = kleisli((r: R) => F.point(a))
    override def ap[A, B](fa: => Kleisli[F, R, A])(f: => Kleisli[F, R, A => B]): Kleisli[F, R, B] = Kleisli[F, R, B](r => F.ap(fa(r))(f(r)))
  }

  trait KleisliMonad[F[+_], R] extends Monad[({type λ[α] = Kleisli[F, R, α]})#λ] with KleisliApplicative[F, R] {
    implicit def F: Monad[F]
    def bind[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, B] = fa flatMap f
  }
}
