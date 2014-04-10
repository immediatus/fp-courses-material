package com.epam.fpp.utils

import scala.annotation.implicitNotFound

@implicitNotFound(
  """Unable to unapply type `${MA}` into a type constructor of kind `M[_]` that is classified by the type class `${TC}`
1) Check that the type class is defined by compiling `implicitly[${TC}[<type constructor>]]`.
2) Review the implicits in object Unapply, which only cover common type 'shapes'
(implicit not found: scalaz.Unapply[${TC}, ${MA}])""")
trait Unapply[TC[_[_]], MA] {

  /** The type constructor */
  type M[_]

  /** The type that `M` was applied to */
  type A

  /** The instance of the type class */
  def TC: TC[M]

  /** Evidence that MA =:= M[A] */
  def apply(ma: MA): M[A]
}

trait Unapply_4 {
  // /** Unpack a value of type `A0` into type `[a]A0`, given a instance of `TC` */
  implicit def unapplyA[TC[_[_]], A0](implicit TC0: TC[({type λ[α] = A0})#λ]): Unapply[TC, A0] {
    type M[X] = A0
    type A = A0
  } = new Unapply[TC, A0] {
    type M[X] = A0
    type A = A0
    def TC = TC0
    def apply(ma: M[A0]) = ma
  }
}

trait Unapply_3 extends Unapply_4 {
  /**Unpack a value of type `M0[F[_], A0, A0, B0]` into types `[a]M0[F, a, a, B0]` and `A0`, given an instance of `TC` */
  implicit def unapplyMFABC1and2[TC[_[_]], F[_], M0[F[_], _, _, _], A0, B0](implicit TC0: TC[({type λ[α] = M0[F, α, α, B0]})#λ]): Unapply[TC, M0[F, A0, A0, B0]] {
    type M[X] = M0[F, X, X, B0]
    type A = A0
  } = new Unapply[TC, M0[F, A0, A0, B0]] {
    type M[X] = M0[F, X, X, B0]
    type A = A0
    def TC = TC0
    def apply(ma: M0[F, A0, A0, B0]) = ma
  }

  /**Unpack a value of type `M0[F[_], A0, B0, C0]` into types `[c]M0[F, A0, B0, c]` and `C0`, given an instance of `TC` */
  implicit def unapplyMFABC3[TC[_[_]], F[_], M0[F[_], _, _, _], A0, B0, C0](implicit TC0: TC[({type λ[ɣ] = M0[F, A0, B0, ɣ]})#λ]): Unapply[TC, M0[F, A0, B0, C0]] {
    type M[X] = M0[F, A0, B0, X]
    type A = C0
  } = new Unapply[TC, M0[F, A0, B0, C0]] {
    type M[X] = M0[F, A0, B0, X]
    type A = C0
    def TC = TC0
    def apply(ma: M0[F, A0, B0, C0]) = ma
  }
}

trait Unapply_2 extends Unapply_3 {
  /**Unpack a value of type `M0[F[_], A0, B0]` into types `[a]M0[F, a, B0]` and `A0`, given an instance of `TC` */
  implicit def unapplyMFAB1[TC[_[_]], F[_], M0[F[_], _, _], A0, B0](implicit TC0: TC[({type λ[α] = M0[F, α, B0]})#λ]): Unapply[TC, M0[F, A0, B0]] {
    type M[X] = M0[F, X, B0]
    type A = A0
  } = new Unapply[TC, M0[F, A0, B0]] {
    type M[X] = M0[F, X, B0]
    type A = A0
    def TC = TC0
    def apply(ma: M0[F, A0, B0]) = ma
  }

  /**Unpack a value of type `M0[F[_], A0, B0]` into types `[b]M0[F, A0, b]` and `B0`, given an instance of `TC` */
  implicit def unapplyMFAB2[TC[_[_]], F[_], M0[F[_], _, _], A0, B0](implicit TC0: TC[({type λ[β] = M0[F, A0, β]})#λ]): Unapply[TC, M0[F, A0, B0]] {
    type M[X] = M0[F, A0, X]
    type A = B0
  } = new Unapply[TC, M0[F, A0, B0]] {
    type M[X] = M0[F, A0, X]
    type A = B0
    def TC = TC0
    def apply(ma: M0[F, A0, B0]) = ma
  }
}

trait Unapply_1 extends Unapply_2 {
  /**Unpack a value of type `M0[A0, B0, C0, D0, E0, F0, G0]` into types `[g]M0[A0, B0, C0, D0, E0, F0, g]` and `G0`, given an instance of `TC` */
  implicit def unapplyMABCDEFG7[TC[_[_]], M0[_, _, _, _, _, _, _], A0, B0, C0, D0, E0, F0, G0](implicit TC0: TC[({type λ[α] = M0[A0, B0, C0, D0, E0, F0, α]})#λ]): Unapply[TC, M0[A0, B0, C0, D0, E0, F0, G0]] {
    type M[X] = M0[A0, B0, C0, D0, E0, F0, X]
    type A = G0
  } = new Unapply[TC, M0[A0, B0, C0, D0, E0, F0, G0]] {
    type M[X] = M0[A0, B0, C0, D0, E0, F0, X]
    type A = G0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0, D0, E0, F0, G0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0, C0, D0, E0, F0]` into types `[f]M0[A0, B0, C0, D0, E0, f]` and `F0`, given an instance of `TC` */
  implicit def unapplyMABCDEF6[TC[_[_]], M0[_, _, _, _, _, _], A0, B0, C0, D0, E0, F0](implicit TC0: TC[({type λ[α] = M0[A0, B0, C0, D0, E0, α]})#λ]): Unapply[TC, M0[A0, B0, C0, D0, E0, F0]] {
    type M[X] = M0[A0, B0, C0, D0, E0, X]
    type A = F0
  } = new Unapply[TC, M0[A0, B0, C0, D0, E0, F0]] {
    type M[X] = M0[A0, B0, C0, D0, E0, X]
    type A = F0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0, D0, E0, F0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0, C0, D0, E0]` into types `[e]M0[A0, B0, C0, D0, e]` and `E0`, given an instance of `TC` */
  implicit def unapplyMABCDE5[TC[_[_]], M0[_, _, _, _, _], A0, B0, C0, D0, E0](implicit TC0: TC[({type λ[α] = M0[A0, B0, C0, D0, α]})#λ]): Unapply[TC, M0[A0, B0, C0, D0, E0]] {
    type M[X] = M0[A0, B0, C0, D0, X]
    type A = E0
  } = new Unapply[TC, M0[A0, B0, C0, D0, E0]] {
    type M[X] = M0[A0, B0, C0, D0, X]
    type A = E0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0, D0, E0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0, C0, D0]` into types `[d]M0[A0, B0, C0, d]` and `D0`, given an instance of `TC` */
  implicit def unapplyMABCD4[TC[_[_]], M0[_, _, _, _], A0, B0, C0, D0](implicit TC0: TC[({type λ[α] = M0[A0, B0, C0, α]})#λ]): Unapply[TC, M0[A0, B0, C0, D0]] {
    type M[X] = M0[A0, B0, C0, X]
    type A = D0
  } = new Unapply[TC, M0[A0, B0, C0, D0]] {
    type M[X] = M0[A0, B0, C0, X]
    type A = D0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0, D0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0, C0]` into types `[c]M0[A0, B0, c]` and `C0`, given an instance of `TC` */
  implicit def unapplyMABC3[TC[_[_]], M0[_, _, _], A0, B0, C0](implicit TC0: TC[({type λ[α] = M0[A0, B0, α]})#λ]): Unapply[TC, M0[A0, B0, C0]] {
    type M[X] = M0[A0, B0, X]
    type A = C0
  } = new Unapply[TC, M0[A0, B0, C0]] {
    type M[X] = M0[A0, B0, X]
    type A = C0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0]) = ma
  }
}

trait Unapply_0 extends Unapply_1 {
  /** Unpack a value of type `M0[F0, A0]` where `F0: * -> *` into
    * types `[a]M0[F0, a]` and `A`, given an instance of `TC`
    */
  implicit def unapplyMFA[TC[_[_]], M0[_[_], _], F0[_], A0](implicit TC0: TC[({type λ[α] = M0[F0, α]})#λ]): Unapply[TC, M0[F0, A0]] {
    type M[X] = M0[F0, X]
    type A = A0
  } = new Unapply[TC, M0[F0, A0]] {
    type M[X] = M0[F0, X]
    type A = A0
    def TC = TC0
    def apply(ma: M0[F0, A0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0]` into types `[a]M0[a, B0]` and `A`, given an instance of `TC` */
  implicit def unapplyMAB1[TC[_[_]], M0[_, _], A0, B0](implicit TC0: TC[({type λ[α] = M0[α, B0]})#λ]): Unapply[TC, M0[A0, B0]] {
    type M[X] = M0[X, B0]
    type A = A0
  } = new Unapply[TC, M0[A0, B0]] {
    type M[X] = M0[X, B0]
    type A = A0
    def TC = TC0
    def apply(ma: M0[A0, B0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0]` into types `[b]M0[A0, b]` and `B`, given an instance of `TC` */
  implicit def unapplyMAB2[TC[_[_]], M0[_, _], A0, B0](implicit TC0: TC[({type λ[α] = M0[A0, α]})#λ]): Unapply[TC, M0[A0, B0]] {
    type M[X] = M0[A0, X]
    type A = B0
  } = new Unapply[TC, M0[A0, B0]] {
    type M[X] = M0[A0, X]
    type A = B0
    def TC = TC0
    def apply(ma: M0[A0, B0]) = ma
  }
}

object Unapply extends Unapply_0 {
  /** Unpack a value of type `M0[A0]` into types `M0` and `A0`, given a instance of `TC` */
  implicit def unapplyMA[TC[_[_]], M0[_], A0](implicit TC0: TC[M0]): Unapply[TC, M0[A0]] {
    type M[X] = M0[X]
    type A = A0
  } = new Unapply[TC, M0[A0]] {
    type M[X] = M0[X]
    type A = A0
    def TC = TC0
    def apply(ma: M0[A0]) = ma
  }
}

trait Unapply2[TC[_[_, _]], MAB] {

  /** The type constructor */
  type M[_, _]

  /** The first type that `M` was applied to */
  type A

  /** The second type that `M` was applied to */
  type B

  /** The instance of the type class */
  def TC: TC[M]

  /** Evidence that MA =:= M[A] */
  def apply(ma: MAB): M[A, B]
}

trait Unapply2_0 {
  /**Unpack a value of type `M0[F[_], A0, B0]` into types `[a, b]=M0[F, a, b]`, `A0`, and 'B9', given an instance of `TC` */
  implicit def unapplyMFAB[TC[_[_, _]], F[_], M0[F[_], _, _], A0, B0](implicit TC0: TC[({type λ[α, β] = M0[F, α, β]})#λ]): Unapply2[TC, M0[F, A0, B0]] {
    type M[X, Y] = M0[F, X, Y]
    type A = A0
    type B = B0
  } = new Unapply2[TC, M0[F, A0, B0]] {
    type M[X, Y] = M0[F, X, Y]
    type A = A0
    type B = B0
    def TC = TC0
    def apply(ma: M0[F, A0, B0]) = ma
  }
}

object Unapply2 extends Unapply2_0 {
  /**Unpack a value of type `M0[A0, B0]` into types `M0`, `A`, and 'B', given an instance of `TC` */
  implicit def unapplyMAB[TC[_[_, _]], M0[_, _], A0, B0](implicit TC0: TC[M0]): Unapply2[TC, M0[A0, B0]] {
    type M[X, Y] = M0[X, Y]
    type A = A0
    type B = B0
  } = new Unapply2[TC, M0[A0, B0]] {
    type M[X, Y] = M0[X, Y]
    type A = A0
    type B = B0
    def TC = TC0
    def apply(ma: M0[A0, B0]) = ma
  }
}

trait Unapply21[TC[_[_, _], _], MAB]{
  type M[_, _]
  type A
  type B
  def TC: TC[M, A]

  def apply(mabc: MAB): M[A, B]
}

object Unapply21 {
  implicit def unapply210MFABC[TC[_[_, _], _], F[+_,+_], M0[_[+_], _, _], A0, B0, C](implicit TC0: TC[({type f[a, b] = M0[({type m[+x] = F[a, x]})#m, C, b]})#f, A0]): Unapply21[TC, M0[({type f[+x] = F[A0, x]})#f, C, B0]]{
    type M[X, Y] = M0[({type f[+a] = F[X, a]})#f, C, Y]
    type A = A0
    type B = B0
  } = new Unapply21[TC, M0[({type f[+x] = F[A0, x]})#f, C, B0]]{
    type M[X, Y] = M0[({type f[+a] = F[X, a]})#f, C, Y]
    type A = A0
    type B = B0

    def TC = TC0
    def apply(ma: M0[({type f[+a] = F[A0, a]})#f, C, B0]) = ma
  }
}

/** Unapply a covariant type constructor, maintaining the covariance */
trait UnapplyCo[TC[_[_]], MA] {

  /** The type constructor */
  type M[+_]

  /** The type that `M` was applied to */
  type A

  /** The instance of the type class */
  def TC: TC[M]

  /** Evidence that MA =:= M[A] */
  def apply(ma: MA): M[A]
}

trait UnapplyCo_3 {
  /** Unpack a value of type `A0` into type `[a]A0`, given a instance of `TC` */
  implicit def unapplyA[TC[_[_]], A0](implicit TC0: TC[({type λ[α] = A0})#λ]): UnapplyCo[TC, A0] {
    type M[+X] = A0
    type A = A0
  } = new UnapplyCo[TC, A0] {
    type M[+X] = A0
    type A = A0
    def TC = TC0
    def apply(ma: M[A0]) = ma
  }
}

trait UnapplyCo_2 extends UnapplyCo_3 {
  /**Unpack a value of type `M0[F[+_], A0, B0]` into types `[a]M0[F, a, B0]` and `A0`, given an instance of `TC` */
  implicit def unapplyMFAB1[TC[_[_]], F[+_], M0[F[+_], +_, _], A0, B0](implicit TC0: TC[({type λ[α] = M0[F, α, B0]})#λ]): UnapplyCo[TC, M0[F, A0, B0]] {
    type M[+X] = M0[F, X, B0]
    type A = A0
  } = new UnapplyCo[TC, M0[F, A0, B0]] {
    type M[+X] = M0[F, X, B0]
    type A = A0
    def TC = TC0
    def apply(ma: M0[F, A0, B0]) = ma
  }

  /**Unpack a value of type `M0[F[+_], A0, B0]` into types `[b]M0[F, A0, b]` and `B0`, given an instance of `TC` */
  implicit def unapplyMFAB2[TC[_[_]], F[+_], M0[F[+_], _, +_], A0, B0](implicit TC0: TC[({type λ[β] = M0[F, A0, β]})#λ]): UnapplyCo[TC, M0[F, A0, B0]] {
    type M[+X] = M0[F, A0, X]
    type A = B0
  } = new UnapplyCo[TC, M0[F, A0, B0]] {
    type M[+X] = M0[F, A0, X]
    type A = B0
    def TC = TC0
    def apply(ma: M0[F, A0, B0]) = ma
  }
}

trait UnapplyCo_1 extends UnapplyCo_2 {
  /**Unpack a value of type `M0[A0, B0, C0, D0, E0, F0, G0]` into types `[g]M0[A0, B0, C0, D0, E0, F0, g]` and `G0`, given an instance of `TC` */
  implicit def unapplyMABCDEFG7[TC[_[_]], M0[_, _, _, _, _, _, +_], A0, B0, C0, D0, E0, F0, G0](implicit TC0: TC[({type λ[α] = M0[A0, B0, C0, D0, E0, F0, α]})#λ]): UnapplyCo[TC, M0[A0, B0, C0, D0, E0, F0, G0]] {
    type M[+X] = M0[A0, B0, C0, D0, E0, F0, X]
    type A = G0
  } = new UnapplyCo[TC, M0[A0, B0, C0, D0, E0, F0, G0]] {
    type M[+X] = M0[A0, B0, C0, D0, E0, F0, X]
    type A = G0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0, D0, E0, F0, G0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0, C0, D0, E0, F0]` into types `[f]M0[A0, B0, C0, D0, E0, f]` and `F0`, given an instance of `TC` */
  implicit def unapplyMABCDEF6[TC[_[_]], M0[_, _, _, _, _, +_], A0, B0, C0, D0, E0, F0](implicit TC0: TC[({type λ[α] = M0[A0, B0, C0, D0, E0, α]})#λ]): UnapplyCo[TC, M0[A0, B0, C0, D0, E0, F0]] {
    type M[+X] = M0[A0, B0, C0, D0, E0, X]
    type A = F0
  } = new UnapplyCo[TC, M0[A0, B0, C0, D0, E0, F0]] {
    type M[+X] = M0[A0, B0, C0, D0, E0, X]
    type A = F0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0, D0, E0, F0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0, C0, D0, E0]` into types `[e]M0[A0, B0, C0, D0, e]` and `E0`, given an instance of `TC` */
  implicit def unapplyMABCDE5[TC[_[_]], M0[_, _, _, _, +_], A0, B0, C0, D0, E0](implicit TC0: TC[({type λ[α] = M0[A0, B0, C0, D0, α]})#λ]): UnapplyCo[TC, M0[A0, B0, C0, D0, E0]] {
    type M[+X] = M0[A0, B0, C0, D0, X]
    type A = E0
  } = new UnapplyCo[TC, M0[A0, B0, C0, D0, E0]] {
    type M[+X] = M0[A0, B0, C0, D0, X]
    type A = E0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0, D0, E0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0, C0, D0]` into types `[d]M0[A0, B0, C0, d]` and `D0`, given an instance of `TC` */
  implicit def unapplyMABCD4[TC[_[_]], M0[_, _, _, +_], A0, B0, C0, D0](implicit TC0: TC[({type λ[α] = M0[A0, B0, C0, α]})#λ]): UnapplyCo[TC, M0[A0, B0, C0, D0]] {
    type M[+X] = M0[A0, B0, C0, X]
    type A = D0
  } = new UnapplyCo[TC, M0[A0, B0, C0, D0]] {
    type M[+X] = M0[A0, B0, C0, X]
    type A = D0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0, D0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0, C0]` into types `[c]M0[A0, B0, c]` and `C0`, given an instance of `TC` */
  implicit def unapplyMABC3[TC[_[_]], M0[_, _, +_], A0, B0, C0](implicit TC0: TC[({type λ[α] = M0[A0, B0, α]})#λ]): UnapplyCo[TC, M0[A0, B0, C0]] {
    type M[+X] = M0[A0, B0, X]
    type A = C0
  } = new UnapplyCo[TC, M0[A0, B0, C0]] {
    type M[+X] = M0[A0, B0, X]
    type A = C0
    def TC = TC0
    def apply(ma: M0[A0, B0, C0]) = ma
  }
}

trait UnapplyCo_0 extends UnapplyCo_1 {
  /**Unpack a value of type `M0[A0, B0]` into types `[a]M0[a, B0]` and `A`, given an instance of `TC` */
  implicit def unapplyMAB1[TC[_[_]], M0[+_, _], A0, B0](implicit TC0: TC[({type λ[α] = M0[α, B0]})#λ]): UnapplyCo[TC, M0[A0, B0]] {
    type M[+X] = M0[X, B0]
    type A = A0
  } = new UnapplyCo[TC, M0[A0, B0]] {
    type M[+X] = M0[X, B0]
    type A = A0
    def TC = TC0
    def apply(ma: M0[A0, B0]) = ma
  }

  /**Unpack a value of type `M0[A0, B0]` into types `[b]M0[A0, b]` and `B`, given an instance of `TC` */
  implicit def unapplyMAB2[TC[_[_]], M0[_, +_], A0, B0](implicit TC0: TC[({type λ[α] = M0[A0, α]})#λ]): UnapplyCo[TC, M0[A0, B0]] {
    type M[+X] = M0[A0, X]
    type A = B0
  } = new UnapplyCo[TC, M0[A0, B0]] {
    type M[+X] = M0[A0, X]
    type A = B0
    def TC = TC0
    def apply(ma: M0[A0, B0]) = ma
  }
}

object UnapplyCo extends UnapplyCo_0 {
  /** Unpack a value of type `M0[A0]` into types `M0` and `A0`, given a instance of `TC` */
  implicit def unapplyMA[TC[_[_]], M0[+_], A0](implicit TC0: TC[M0]): UnapplyCo[TC, M0[A0]] {
    type M[+X] = M0[X]
    type A = A0
  } = new UnapplyCo[TC, M0[A0]] {
    type M[+X] = M0[X]
    type A = A0
    def TC = TC0
    def apply(ma: M0[A0]) = ma
  }

  // TODO More!
}