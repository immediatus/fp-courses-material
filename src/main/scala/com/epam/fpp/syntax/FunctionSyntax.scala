package com.epam.fpp.syntax


trait Function1Syntax[T1, R] {
  def $(v1: T1):R
}

trait Function2Syntax[T1, T2, R] {
  def $(v1: T1): Function[T2, R]
}

trait ToFunctionSyntax
  extends ToFunction1Syntax
  with    ToFunction2Syntax

trait ToFunction1Syntax {
  implicit def toFunctionSyntax1[T1, R](func: Function[T1, R]) =
    new Function1Syntax[T1, R] {
      def $(v1: T1):R = func(v1)
    }
}

trait ToFunction2Syntax {
  implicit def toFunctionSyntax2[T1, T2, R](func: Function2[T1, T2, R]) =
    new Function2Syntax[T1, T2, R] {
      def $(v1: T1):Function[T2, R] = func.curried(v1)
    }
}

