package com.epam.fpp

trait ApplicativeBuilder[M[_], A, B] {
  val a: M[A]
  val b: M[B]

  def apply[C](f: (A, B) => C)(implicit ap: Applicative[M]): M[C] = ap.apply2(a, b)(f)
  def |@|[C](cc: M[C]) = ⊛(cc)
  def ⊛[C](cc: M[C]) = new ApplicativeBuilder3[C] {
    val c = cc
  }

  sealed trait ApplicativeBuilder3[C] {
    val c: M[C]

    def apply[D](f: (A, B, C) => D)(implicit ap: Applicative[M]): M[D] = ap.apply3(a, b, c)(f)
    def |@|[D](dd: M[D]) = ⊛(dd)
    def ⊛[D](dd: M[D]) = new ApplicativeBuilder4[D] {
      val d = dd
    }

    sealed trait ApplicativeBuilder4[D] {
      val d: M[D]

      def apply[E](f: (A, B, C, D) => E)(implicit ap: Applicative[M]): M[E] = ap.apply4(a, b, c, d)(f)
    }
  }
}
