package paperdoll.arm

import paperdoll.reader.Reader_
import paperdoll.scalaz.Writer_
import shapeless.Nat

/**
 * Marker that effects from the given layer can be used safely with regions
 * (managed resources).
 * L must be <: Layer, however the Leibniz necessary to make type inference
 * work correctly can be expressed more easily if we don't include this constraint.
 */
trait SafeForRegion[L /*<: Layer*/ ]

object SafeForRegion {
  implicit def readerSafeForRegion[I] = new SafeForRegion[Reader_[I]] {}
  implicit def writerSafeForRegion[O] = new SafeForRegion[Writer_[O]] {}
  /**
   * We could constrain S to enforce a rule about the order of nesting
   * e.g. that higher-numbered regions are always inside lower-numbered ones
   * However I see very little value in that
   */
  implicit def regionSafeForRegion[S <: Nat, R] = new SafeForRegion[Region_[S, R]] {}
}