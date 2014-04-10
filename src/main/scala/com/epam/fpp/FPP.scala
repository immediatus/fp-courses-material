package com.epam.fpp

import com.epam.fpp.instances.{ListInstances, OptionInstances, FunctionInstances, PartialFunctionInstances, KleisliInstances}
import com.epam.fpp.syntax.ToFunctionSyntax

object FPP extends AnyRef

// Type Class declaration
  with ToComposeTypeClass
  with ToCategoryTypeClass
  with ToArrowTypeClass
  with ToFunctorTypeClass
  with ToApplicativeTypeClass
  with ToMonadTypeClass
  with ToMonadExtTypeClass

// Instances including
  with FunctionInstances
  with PartialFunctionInstances
  with ListInstances
  with OptionInstances
  with KleisliInstances

// Syntax Extension
  with ToFunctionSyntax
