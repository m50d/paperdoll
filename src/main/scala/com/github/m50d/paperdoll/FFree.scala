package com.github.m50d.paperdoll

import shapeless.Coproduct

sealed trait FFree[R <: Coproduct, A]
final case class Pure[R <: Coproduct, A](a: A) extends FFree[R, A]
final case class Impure[R <: Coproduct, X <: Coproduct, A]()