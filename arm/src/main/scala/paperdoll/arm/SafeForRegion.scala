package paperdoll.arm

import paperdoll.core.layer.Layer
import paperdoll.reader.Reader_
import paperdoll.writer.Writer_

/**
 * Marker that effects from the given layer can be used safely with regions
 * (managed resources).
 */
trait SafeForRegion[L /*<: Layer*/]

object SafeForRegion {
  implicit def readerSafeForRegion[I] = new SafeForRegion[Reader_[I]] {}
  implicit def writerSafeForRegion[O] = new SafeForRegion[Writer_[O]] {}
  implicit def regionSafeForRegion[S, R] = new SafeForRegion[Region_[S, R]] {}
}