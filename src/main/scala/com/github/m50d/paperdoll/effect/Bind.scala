package com.github.m50d.paperdoll.effect

import com.github.m50d.paperdoll.layer.Layer
import com.github.m50d.paperdoll.layer.Layers
import shapeless.Coproduct

/**
 * Specific effects should provide implementations of this interface
 */
trait Bind[L <: Layer] {
  /**
   * Translate the effectful value eff (with an effect from layer L)
   * and effectful continuation cont (V => A with effects R)
   * into an effectful (lazy) value of type A
   * (usually by somehow "running" eff to obtain a V
   * and then passing it to cont)
   */
  def apply[V, RR <: Coproduct, RL <: Layers[RR], A](eff: L#F[V], cont: Arr[RR, RL, V, A]): Eff[RR, RL, A]
}