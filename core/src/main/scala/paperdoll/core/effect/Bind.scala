package paperdoll.core.effect

import paperdoll.core.layer.{ Layer, Layers }
import shapeless.Coproduct
import paperdoll.core.layer.Member

trait Bind[R <: Coproduct, L1 <: Layers[R], L <: Layer] extends Handler[R, L1, L] with Loop[R, L1, L]

trait PureBind[L <: Layer] extends PureHandler[L] {
  def pure[A](a: A): O[A]
  def bind[V, RR <: Coproduct, RL <: Layers[RR], A](eff: L#F[V], cont: Arr[RR, RL, V, O[A]]): Effects[RR, RL, O[A]]

  override def handler[R <: Coproduct](implicit me1: Member[R, L]): Handler[R, me1.L, L] {
    type RestR = me1.RestR
    type RestL = me1.RestL
    type O[X] = PureBind.this.O[X]
  } =
    new Bind[R, me1.L, L] {
      type RestR = me1.RestR
      type RestL = me1.RestL
      override def me = me1
      override def pure[A](a: A) = PureBind.this.pure(a)
      override type O[X] = PureBind.this.O[X]
      override def bind[V, A](eff: L#F[V], cont: Arr[RestR, RestL, V, O[A]]) = PureBind.this.bind(eff, cont)
    }
}