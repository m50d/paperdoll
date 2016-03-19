package paperdoll.core.effect

import paperdoll.core.layer.{Layer, Layers}
import shapeless.Coproduct

/**
 * Interface for something that knows how to handle a particular effect.
 * Specific effects should provide implementations of this interface
 * and use Eff.handle to convert them into Handlers
 */
trait Bind[L <: Layer] {
  /**
   * How the type of the value is transformed by handling this effect
   */
  type O[X]
  
  /**
   * Translate the pure value a
   */
  def pure[A](a: A): O[A]
  /**
   * Translate the effectful value eff (with an effect from layer L)
   * and effectful continuation cont (V => A with effects R)
   * into an effectful (lazy) value of type A
   * (usually by somehow "running" eff to obtain a V
   * and then passing it to cont)
   */
  def apply[V, RR <: Coproduct, RL <: Layers[RR], A](eff: L#F[V], cont: Arr[RR, RL, V, O[A]]): Eff[RR, RL, O[A]]
}